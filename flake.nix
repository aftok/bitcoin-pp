{
  description = "Bippy: BIP-70 in Haskell";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    pkg-name = "bippy";
    haskell-overlay = hfinal: hprev: {
      ${pkg-name} = hfinal.callCabal2nix pkg-name ./. {};
    };

    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.extend haskell-overlay;
    };
  in {
      overlays = {
        default = overlay;
      };
    } //
    flake-utils.lib.eachDefaultSystem (system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [overlay];
        };

        hspkgs = pkgs.haskellPackages;
      in {
        packages = {
          bippy = hspkgs.bippy;
          default = self.packages.${system}.bippy;
        };

        devShells = {
          default = hspkgs.shellFor {
            packages = _: [self.packages.${system}.bippy];
            buildInputs = [
              pkgs.cabal-install
              hspkgs.ormolu
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        };

        formatter = pkgs.alejandra;
      }
    );
}
