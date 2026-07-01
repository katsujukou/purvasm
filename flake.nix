{
  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
 };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            purescript-overlay.overlays.default
          ];
        };

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
          devShells.default = pkgs.mkShellNoCC {
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
        }
    );
}
