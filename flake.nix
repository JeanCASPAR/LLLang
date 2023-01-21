{
  description = "LLLang";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    utils.url = github:numtide/flake-utils;
    rust-overlay = {
      url = github:oxalica/rust-overlay;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
      };
    };
    naersk.url = github:nix-community/naersk;
  };

  outputs = { self, nixpkgs, utils, naersk, rust-overlay }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
          ];
        };
        rust = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "clippy" ];
        };
        naerskLib = naersk.lib.${system}.override {
          cargo = rust;
          rustc = rust;
        };
        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive)
          scheme-full;
        };
      in {
        defaultPackage = naerskLib.buildPackage {
          pname = "petitc";
          root = ./.;
        };
        defaultApp = utils.lib.mkApp {
          drv = self.defaultPackage.${system};
        };
        devShell = with pkgs; mkShell {
          packages = [
            gdb
            rust
            cargo
            cargo-edit
            rustfmt
            rustPackages.clippy
            rust-analyzer
            tex
          ];
        };
      });
}
