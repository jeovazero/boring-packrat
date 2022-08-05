 let
  pinnedPkgs = builtins.fetchTarball {
    name = "nixos-22.05-2022-08-21";
    url = "https://github.com/nixos/nixpkgs/archive/48e760269215857628caa9e08eb14754cefd0c1a.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1nvsc9r822kcs0zjnzsx5833az4ysqpj5hqswqfvqi7rrc6m17rx";
  };
 in import pinnedPkgs
