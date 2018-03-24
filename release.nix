let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          dhall = haskellPackagesNew.callPackage ./dhall.nix { };

          dhall-json =
            pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./default.nix { });

          formatting =
            haskellPackagesNew.callPackage ./formatting.nix { };

          prettyprinter =
            haskellPackagesNew.callPackage ./prettyprinter.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) dhall-json;
  }
