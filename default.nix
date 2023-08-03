{ haskellPkgs }:
  haskellPkgs.callCabal2nix "boring-packrat" ./. {}
