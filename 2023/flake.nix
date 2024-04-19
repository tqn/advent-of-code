{
  # Flake inputs
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs"; # also valid: "nixpkgs"
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  # Flake outputs
  outputs = {
    self,
    nixpkgs,
    flake-compat,
  }: let
    # Systems supported
    allSystems = [
      "x86_64-linux" # 64-bit Intel/AMD Linux
      "aarch64-linux" # 64-bit ARM Linux
      "x86_64-darwin" # 64-bit Intel macOS
      "aarch64-darwin" # 64-bit ARM macOS
    ];

    # Helper to provide system-specific attributes
    forAllSystems = f:
      nixpkgs.lib.genAttrs allSystems (system:
        f {
          pkgs = import nixpkgs {inherit system;};
        });
  in {
    devShells = forAllSystems ({pkgs}: rec {
      default = pkgs.mkShell {inputsFrom = [clojure guile];};
      clojure = pkgs.mkShell {
        packages = [pkgs.clojure];
        nativeBuildInputs = [pkgs.clojure-lsp];
      };
      guile = pkgs.mkShell {packages = [pkgs.guile];};
    });
  };
}
