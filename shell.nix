let

  nixpkgs = import ./nix/pinned.nix { };

  inherit (nixpkgs) pkgs;

  inherit (pkgs) haskell cabal2nix cabal-install ghcid;

  haskellPackages = haskell.packages.ghc902;

  project = import ./default.nix {};

in pkgs.stdenv.mkDerivation {
  name = "shell";

  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    ghcid
  ];
}
