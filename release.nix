# You can build this repository using Nix by running:
#
#     $ nix-build -A dhall-json release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A dhall-json.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          aeson =
            pkgs.haskell.lib.dontCheck haskellPackagesOld.aeson_1_1_0_0;

          dhall-json = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { dhall-json = pkgs.haskellPackages.dhall-json;
  }
