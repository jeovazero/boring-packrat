let
  nixpkgs = import ./nix/pinned.nix {};
  inherit (nixpkgs) pkgs;
  localHaskellPkgs = pkgs.haskell.packages.ghc902;
in
{ haskellPkgs ? localHaskellPkgs }:
  haskellPkgs.callCabal2nix "boring-packrat" ./. {}
