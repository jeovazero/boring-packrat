{ pkgs }:
let
  haskellPackages = pkgs.haskell.packages.ghc927;

  project = import ./default.nix { inherit pkgs; };

in with pkgs;
mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    ghcid
  ];

  LOCALE_ARCHIVE = lib.optionalString stdenv.isLinux
    "${glibcLocales}/lib/locale/locale-archive";

}
