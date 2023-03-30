{ pkgs ? (import <nixpkgs> {}) }:

let extension = self : super : {
        acid_base = super.callPackage ./acid_base.nix {};
        digestiveFunctorsBlaze = super.callPackage ./missingFromNixpkgs/digestiveFunctorsBlaze.nix {};
        digestiveFunctorsHappstack = super.callPackage ./missingFromNixpkgs/digestiveFunctorsHappstack.nix {};
    };
    hspkgs = pkgs.haskellPackages_ghc783.override { inherit extension; };
in hspkgs.ghcWithPackages (theGhc : with theGhc;
      [ acid_base ] )
