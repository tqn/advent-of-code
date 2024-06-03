{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    packageName = "haskell-template";

    # Systems supported
    allSystems = [
      "x86_64-linux" # 64-bit Intel/AMD Linux
      "aarch64-linux" # 64-bit ARM Linux
      "x86_64-darwin" # 64-bit Intel macOS
      "aarch64-darwin" # 64-bit ARM macOS
    ];

    # Helper to provide system-specific attributes
    forAllSystems = f:
      nixpkgs.lib.genAttrs allSystems (
        system: let
          config = {};

          overlays = [
            # This is an overlay we apply on top of Nixpkgs with some of our
            # own packages defined.
            (final: prev: {
              # A Haskell package set with our own overrides and packages defined.
              myHaskellPackages = final.haskell.packages.ghc928.override {
                overrides = hfinal: hprev: {
                  # This is our local Haskell package.
                  ${packageName} =
                    hfinal.callCabal2nix packageName ./. {};
                };
              };

              # This is just a convenient shortcut to our package from the
              # top-level of Nixpkgs.  We're also applying the
              # justStaticExecutables function to our package in order to
              # reduce the size of the output derivation.
              ${packageName} =
                final.haskell.lib.compose.justStaticExecutables
                final.myHaskellPackages.${packageName};

              # Wrap Stack to work with our Nix integration. We don't want to modify
              # stack.yaml so non-Nix users don't notice anything.
              # - no-nix: We don't want Stack's way of integrating Nix.
              # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
              # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
              # stack-wrapped = final.symlinkJoin {
              #   name = "stack"; # will be available as the usual `stack` in terminal
              #   paths = [final.stack];
              #   buildInputs = [final.makeWrapper];
              #   postBuild = ''
              #     wrapProgram $out/bin/stack \
              #       --add-flags "\
              #         --no-nix \
              #         --system-ghc \
              #         --no-install-ghc \
              #       "
              #   '';
              # };

              # A Haskell development shell for our package that includes
              # things like cabal and HLS.
              myDevShell = final.myHaskellPackages.shellFor {
                packages = p: [p.${packageName}];

                nativeBuildInputs = [
                  final.ghc
                  final.cabal-install
                  # final.stack-wrapped
                  final.myHaskellPackages.haskell-language-server
                  final.myHaskellPackages.hlint
                  final.myHaskellPackages.fourmolu
                ];
              };
            })
          ];
          # Our full Nixpkgs with the above overlay applied.
        in
          f (import nixpkgs {inherit config overlays system;})
      );
  in {
    packages = forAllSystems (pkgs: {default = pkgs.${packageName};});

    devShells = forAllSystems (pkgs: {default = pkgs.myDevShell;});
  };
}
