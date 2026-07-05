//! The `#[pv_foreign]` attribute macro (ADR-0078 §3): adapts an idiomatic Rust function to the
//! `pvf_<mangle(key)>` `AbiCodeFn` shape of the `pv_*` C ABI, so a Rust foreign is
//! link-indistinguishable from a C one (ADR-0078 §1).
//!
//! ```ignore
//! #[pv_foreign(module = "Data.Show", name = "showNumberImpl")]
//! fn show_number(n: f64) -> String { … }
//! ```
//!
//! generates, next to the untouched user function, a `pub unsafe extern "C"` wrapper exported
//! under the injectively-mangled symbol, which opens the shadow-stack frame, converts the
//! arguments via `FromPv`, calls the user function, converts the result via `IntoPv`, and
//! installs the panic guard — all through `purvasm-foreign`'s hidden `__rt` module, so the
//! contract lives in ONE checked place, not in per-leaf expansions.
//!
//! With the `effect` marker the macro instead generates the ADR-0067 pair: an outer leaf of the
//! value arity that captures its arguments into a thunk closure, plus the thunk `AbiCodeFn` that
//! reads the captures back and runs the body when the effect is performed.

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, FnArg, Ident, ItemFn, LitStr, Pat, Token, Type};

/// The injective identifier escape of ADR-0072 §2, shared with the compiler's `escape_ident`
/// (boot `codegen_llvm.ml`): alphanumerics pass through, every other **byte** (including `_`)
/// becomes `_hh` lowercase hex. Reproduced here so the symbol is computed in exactly one place
/// per crate graph — at macro expansion — never hand-written by the leaf author.
fn escape_ident(key: &str) -> String {
    let mut out = String::with_capacity(key.len() + 8);
    for b in key.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' => out.push(b as char),
            _ => out.push_str(&format!("_{:02x}", b)),
        }
    }
    out
}

/// `#[pv_foreign(module = "...", name = "...", effect)]` — `name` defaults to the Rust fn name,
/// `effect` marks the ADR-0067 thunk shape.
struct Args {
    module: String,
    name: Option<String>,
    effect: bool,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut module = None;
        let mut name = None;
        let mut effect = false;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            match key.to_string().as_str() {
                "module" => {
                    input.parse::<Token![=]>()?;
                    module = Some(input.parse::<LitStr>()?.value());
                }
                "name" => {
                    input.parse::<Token![=]>()?;
                    name = Some(input.parse::<LitStr>()?.value());
                }
                "effect" => effect = true,
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown pv_foreign argument `{other}` (expected `module`, `name`, `effect`)"),
                    ))
                }
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        let module = module.ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "pv_foreign requires `module = \"...\"`",
            )
        })?;
        Ok(Args {
            module,
            name,
            effect,
        })
    }
}

/// Whether a parameter type is `&Ctx<…>` — the optional first parameter giving the leaf body
/// access to the safe context (for `PvValue`-typed work). Detected syntactically by the reference
/// target's last path segment; a false positive fails to compile in the expansion, never silently
/// mis-runs.
fn is_ctx_ref(ty: &Type) -> bool {
    if let Type::Reference(r) = ty {
        if let Type::Path(p) = &*r.elem {
            if let Some(seg) = p.path.segments.last() {
                return seg.ident == "Ctx";
            }
        }
    }
    false
}

/// Whether a parameter type is `&str` — the one supported reference parameter (ADR-0078 §3): the
/// generated prologue owns a copied-out `String`, and the user fn borrows it for the duration of
/// the call. Never a borrow into the guest heap.
fn is_str_ref(ty: &Type) -> bool {
    if let Type::Reference(r) = ty {
        if r.mutability.is_none() {
            if let Type::Path(p) = &*r.elem {
                return p.path.is_ident("str");
            }
        }
    }
    false
}

/// How a value parameter converts at the boundary.
enum ParamKind {
    /// `FromPv` produces the owned value the fn takes (`i32`, `String`, `PvValue`, …).
    Owned,
    /// `&str`: convert to a shim-owned `String`, pass a borrow of it to the user fn.
    StrRef,
}

#[proc_macro_attribute]
pub fn pv_foreign(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as Args);
    let func = parse_macro_input!(item as ItemFn);
    match expand(args, &func) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(args: Args, func: &ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let fn_ident = &func.sig.ident;
    let key = format!(
        "{}.{}",
        args.module,
        args.name.unwrap_or_else(|| fn_ident.to_string())
    );
    let symbol = format!("pvf_{}", escape_ident(&key));

    // Split the signature into the optional leading `&Ctx` and the value parameters.
    let mut takes_ctx = false;
    let mut params: Vec<(Ident, ParamKind)> = Vec::new();
    for (i, input) in func.sig.inputs.iter().enumerate() {
        match input {
            FnArg::Receiver(r) => {
                return Err(syn::Error::new_spanned(
                    r,
                    "pv_foreign: methods are not supported",
                ))
            }
            FnArg::Typed(pt) => {
                if i == 0 && is_ctx_ref(&pt.ty) {
                    takes_ctx = true;
                    continue;
                }
                let ident = match &*pt.pat {
                    Pat::Ident(pi) => pi.ident.clone(),
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            "pv_foreign: parameters must be plain identifiers",
                        ))
                    }
                };
                let kind = if is_str_ref(&pt.ty) {
                    ParamKind::StrRef
                } else if matches!(&*pt.ty, Type::Reference(_)) {
                    return Err(syn::Error::new_spanned(
                        &pt.ty,
                        "pv_foreign: the only supported reference parameter is `&str` \
                         (take the owned type — String, i32, f64, bool, PvValue — instead)",
                    ));
                } else {
                    ParamKind::Owned
                };
                params.push((ident, kind));
            }
        }
    }
    let arity = params.len();

    // The conversion prologue + user call, shared by every shape. `__cx`/`__vals` are the shim
    // closure's context and rooted argument slice. Owned parameters get no type ascription: the
    // user fn's own signature drives `FromPv` inference through the call below, so parameter
    // types may mention the fn's lifetimes (`PvValue<'f>`) without the shim re-declaring them.
    // A `&str` parameter converts to a shim-owned `String` (ascribed — inference cannot see
    // through the borrow) that the user fn borrows for the duration of the call (ADR-0078 §3).
    let binds = params.iter().enumerate().map(|(i, (ident, kind))| {
        let bind = format_ident!("__arg_{}", ident);
        match kind {
            ParamKind::StrRef => quote! {
                let #bind: ::std::string::String =
                    ::purvasm_foreign::FromPv::from_pv(__cx, __vals[#i]);
            },
            ParamKind::Owned => quote! {
                let #bind = ::purvasm_foreign::FromPv::from_pv(__cx, __vals[#i]);
            },
        }
    });
    let call_args = {
        let value_args = params.iter().map(|(ident, kind)| {
            let bind = format_ident!("__arg_{}", ident);
            match kind {
                ParamKind::StrRef => quote! { #bind.as_str() },
                ParamKind::Owned => quote! { #bind },
            }
        });
        if takes_ctx {
            quote! { __cx, #(#value_args),* }
        } else {
            quote! { #(#value_args),* }
        }
    };
    let body = quote! {
        #(#binds)*
        ::purvasm_foreign::IntoPv::into_pv(#fn_ident(#call_args), __cx)
    };

    let shim_ident = format_ident!("__pvf_{}", fn_ident);
    let generated = if !args.effect {
        // Pure leaf: one AbiCodeFn under the pvf_ symbol.
        quote! {
            #[doc(hidden)]
            #[export_name = #symbol]
            pub unsafe extern "C" fn #shim_ident(
                __ctx: *mut ::purvasm_foreign::sys::PVContext,
                __closure: ::purvasm_foreign::sys::PVWord,
                __args: *const ::purvasm_foreign::sys::PVWord,
                __nargs: usize,
            ) -> ::purvasm_foreign::sys::PVWord {
                ::purvasm_foreign::__rt::leaf_shim(__ctx, __args, __nargs, #key, #arity, |__cx, __vals| {
                    #body
                })
            }
        }
    } else if arity == 0 {
        // Arity-0 effect: the leaf itself is the thunk (an arity-1 closure applied to `unit`).
        quote! {
            #[doc(hidden)]
            #[export_name = #symbol]
            pub unsafe extern "C" fn #shim_ident(
                __ctx: *mut ::purvasm_foreign::sys::PVContext,
                __closure: ::purvasm_foreign::sys::PVWord,
                __args: *const ::purvasm_foreign::sys::PVWord,
                __nargs: usize,
            ) -> ::purvasm_foreign::sys::PVWord {
                ::purvasm_foreign::__rt::leaf_shim(__ctx, __args, __nargs, #key, 1usize, |__cx, __vals| {
                    let _ = __vals; // the forced `unit`
                    #body
                })
            }
        }
    } else {
        // Effectful leaf of value arity n >= 1 (ADR-0067 shape): the outer leaf captures its
        // arguments into a thunk closure; the thunk reads them back at perform time.
        let thunk_ident = format_ident!("__pvf_thunk_{}", fn_ident);
        quote! {
            #[doc(hidden)]
            #[export_name = #symbol]
            pub unsafe extern "C" fn #shim_ident(
                __ctx: *mut ::purvasm_foreign::sys::PVContext,
                __closure: ::purvasm_foreign::sys::PVWord,
                __args: *const ::purvasm_foreign::sys::PVWord,
                __nargs: usize,
            ) -> ::purvasm_foreign::sys::PVWord {
                ::purvasm_foreign::__rt::leaf_shim(__ctx, __args, __nargs, #key, #arity, |__cx, __vals| {
                    ::purvasm_foreign::__rt::make_thunk(__cx, #thunk_ident, __vals)
                })
            }

            #[doc(hidden)]
            unsafe extern "C" fn #thunk_ident(
                __ctx: *mut ::purvasm_foreign::sys::PVContext,
                __closure: ::purvasm_foreign::sys::PVWord,
                __args: *const ::purvasm_foreign::sys::PVWord,
                __nargs: usize,
            ) -> ::purvasm_foreign::sys::PVWord {
                ::purvasm_foreign::__rt::thunk_shim(__ctx, __closure, #arity, #key, |__cx, __vals| {
                    #body
                })
            }
        }
    };

    Ok(quote! {
        #func
        #generated
    })
}

#[cfg(test)]
mod tests {
    use super::escape_ident;

    /// Pinned against the compiler's mangling: `Data.Show.showNumberImpl` must produce the exact
    /// symbol body the shipped `Data.Show.c` exports (`pvf_Data_2eShow_2eshowNumberImpl`).
    #[test]
    fn escape_matches_compiler_vectors() {
        assert_eq!(
            escape_ident("Data.Show.showNumberImpl"),
            "Data_2eShow_2eshowNumberImpl"
        );
        // `_` itself escapes (the injectivity case: `A.B` vs `A_B` must not collide).
        assert_eq!(escape_ident("A.B"), "A_2eB");
        assert_eq!(escape_ident("A_B"), "A_5fB");
        assert_eq!(
            escape_ident("Purvasm.Stdio.writeLineImpl"),
            "Purvasm_2eStdio_2ewriteLineImpl"
        );
    }
}
