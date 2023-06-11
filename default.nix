{ pkgs }:
let
  localHaskellPkgs = pkgs.haskell.packages.ghc927;
in
  localHaskellPkgs.callCabal2nix "boring-packrat" ./. {}
