{ nixpkgs ? import ./nix/pinnedNix.nix {} }:
let
  inherit (nixpkgs) pkgs;
  haskellPkgs = pkgs.haskell.packages.ghc884;
in
  haskellPkgs.callCabal2nix "boring-peg-like-parser" ./. {}
