let
  pinnedPkgs = builtins.fetchTarball {
    name = "nixos-21.05-2021-06-22";
    url = "https://github.com/nixos/nixpkgs/archive/06db2e2197401b74fcf82d4e84be15b0b5851c7b.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0p3c9gjjdr1gz55al2s5yhh59kx8fqbgzhw4cab3mqair9h84m4j";
  };
 in import pinnedPkgs
