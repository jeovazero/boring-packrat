let

  nixpkgs = import ./nix/pinnedNix.nix { };

  inherit (nixpkgs) pkgs;

  inherit (pkgs) haskell cabal2nix cabal-install;

  haskellPackages = haskell.packages.ghc884;

  ghcide = haskellPackages.ghcide;

in pkgs.stdenv.mkDerivation {
  name = "shell";

  buildInputs = [
    cabal-install
    cabal2nix
    ghcide
  ];
}
