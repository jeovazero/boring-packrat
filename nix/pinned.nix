let
  pinnedPkgs = builtins.fetchTarball {
    name = "nixos-21.05-2021-06-22";
    url = "https://github.com/nixos/nixpkgs/archive/6613a30c5e3ee59753181512b4bedd4121569925.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "18v74cwjcl7qkdhgc8xic9fvp3330dsc82ah4xs3qzl5ks2h9d5h";
  };
 in import pinnedPkgs
