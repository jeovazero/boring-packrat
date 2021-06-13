let

  nixpkgs = import ./nix/pinnedNix.nix { };

  inherit (nixpkgs) pkgs;

  inherit (pkgs) haskell cabal2nix cabal-install ghcid;

  haskellPackages = haskell.packages.ghc884;

  ghcide = haskellPackages.ghcide;

  project = import ./default.nix {};

in pkgs.stdenv.mkDerivation {
  name = "shell";

  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    ghcide
    ghcid
  ];
}
