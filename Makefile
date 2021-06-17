dev:
	ghcid --command "ghci Main -fobject-code -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns -ilib" --test main

build:
	nix-build default.nix
