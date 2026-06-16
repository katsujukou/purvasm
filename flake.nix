{
  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-claude-code.url = "github:ryoppippi/nix-claude-code";
 };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, nix-claude-code }:
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
          js_of_ocaml
          js_of_ocaml-ppx
          alcotest
        ];
        claude-code = nix-claude-code.packages.${system}.default;
      in
        {
          devShells.default = pkgs.mkShellNoCC {
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
              claude-code
            ]);
          };
        }
    );
}
