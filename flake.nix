{
  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Nightly Rust with the `miri` component for the ADR-0063 §4 island check — stable nixpkgs
    # `rustc` provides neither nightly nor `miri`. Only the `miri` devShell uses it; the default
    # shell stays on stable nixpkgs `rustc`.
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
 };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            purescript-overlay.overlays.default
            rust-overlay.overlays.default
          ];
        };

        # A nightly toolchain carrying `miri` + `rust-src` (the latter lets `cargo miri setup`
        # build a checked std). `selectLatestNightlyWith` picks the newest nightly on which every
        # requested component actually exists, so it never lands on a day `miri` was missing.
        rustNightlyMiri = pkgs.rust-bin.selectLatestNightlyWith
          (toolchain: toolchain.default.override {
            extensions = [ "miri" "rust-src" ];
          });

        # Override the OCaml packages to set `dontStrip = true` for `wasm_of_ocaml-compiler`.
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3.overrideScope
          (final: prev: {
            wasm_of_ocaml-compiler = prev.wasm_of_ocaml-compiler.overrideAttrs (old: {
              dontStrip = true;
            });
          });

        ocamlDeps = with ocamlPackages; [
          cmdliner
          base
          yojson
          js_of_ocaml
          js_of_ocaml-ppx
          alcotest
        ];
      in
        {
          # `mkShell`, not `mkShellNoCC`: `ocamlopt` shells out to the system C compiler to link
          # every executable, and the linked-in OCaml runtime objects come from the Nix store —
          # so the C toolchain must come from the same (Nix) libc world. Without it, a bare
          # environment has no `gcc` at all, and a host with its own gcc (the GitHub ubuntu
          # runner) silently mixes host glibc with Nix-built objects, producing binaries that
          # abort at startup with "*** stack smashing detected ***".
          devShells.default = pkgs.mkShell {
            # nixpkgs' stable `rustc` ships no stdlib *source*, so rust-analyzer can't find
            # `$sysroot/lib/rustlib/src/rust/library` ("try installing rust-src"). Point it at the
            # separately-packaged std source. rust-analyzer honours RUST_SRC_PATH.
            RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";

            buildInputs = ocamlDeps ++ (with ocamlPackages; [
              ocaml
              dune_3
              findlib
              js_of_ocaml-compiler
              wasm_of_ocaml-compiler
              ocaml-lsp
              ocamlformat
              utop
              odoc
            ]) ++ (with pkgs; [
              purs
              spago
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
              esbuild
              nodejs_24
              pnpm
              gnuplot

              # Rust toolchain — the v1 native runtime (ADR-0063: unsafe GC island + safe shell).
              # NB: Miri (ADR-0063 §4) needs a nightly toolchain + the `miri` component, which stable
              # nixpkgs `rustc` does not provide; adopt oxalica/rust-overlay when we start Miri-testing
              # the unsafe island.
              rustc
              cargo
              rustfmt
              clippy
              rust-analyzer

              # LLVM — ANF → LLVM IR → object codegen and linking (ADR-0060 / ADR-0064).
              # Pin a specific `llvmPackages_NN` once codegen targets a fixed textual-IR version.
              llvm
              clang
              lld
            ]);
          };

          # Miri-only shell for the ADR-0063 §4 unsafe-island check (nightly + `miri`). Kept separate
          # from `default` so the everyday stable toolchain is untouched. Usage:
          #   nix develop .#miri -c sh -c 'cd runtime && cargo miri test'
          #
          # Provenance mode: run under Miri's DEFAULT (exposed/permissive) provenance — do NOT set
          # `-Zmiri-strict-provenance`. The tagged-word ABI (ADR-0064 §1) round-trips heap pointers
          # through integers on purpose, so strict provenance would report the int↔pointer tagging as
          # false failures. "Miri clean" here = no real UB (out-of-bounds, use-after-free, bad
          # transmutes, Stacked-Borrows aliasing); one expected "integer-to-pointer cast" WARNING
          # remains and is fine. See ADR-0063 §4.
          devShells.miri = pkgs.mkShellNoCC {
            buildInputs = [ rustNightlyMiri ];
            shellHook = ''
              echo "nightly + miri ready — run: (cd runtime && cargo miri test)"
            '';
          };
        }
    );
}
