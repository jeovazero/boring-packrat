let
  nixpkgs = import ./nix/pinned.nix {};
  inherit (nixpkgs) pkgs;
  haskellPkgs = pkgs.haskell.packages.ghc8104;
in
  haskellPkgs.callCabal2nix "boring-peg-like-parser" ./. {}
