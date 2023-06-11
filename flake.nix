{
  description = "DEV";

  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
           packages = {
             default = import ./default.nix { pkgs = pkgs; };
           };
          devShell = import ./shell.nix { pkgs = pkgs; };
        }
      );
}
