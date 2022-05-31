let
  nixpkgs = import ./nix/pinned.nix {};
  inherit (nixpkgs) pkgs;
  localHaskellPkgs = pkgs.haskell.packages.ghc8107;
in
{ haskellPkgs ? localHaskellPkgs }:
  haskellPkgs.callCabal2nix "boring-packrat" ./. {}
