{ compiler ? "ghc882"
, pkgs ? import <nixpkgs> { }
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = haskellPackages.callPackage ./default.nix { };
in
  {
    project = project;
  }
