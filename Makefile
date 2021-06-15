dev:
	ghcid --command "ghci Main -fobject-code -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns" --test main

build:
	nix-build default.nix
