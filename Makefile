dev:
	ghcid --command "ghci Main -fobject-code" --test main

build:
	nix-build default.nix
