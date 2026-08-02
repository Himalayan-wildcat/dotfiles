{
  description = "Home Manager configuration of hiroaki.hara";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, home-manager, ... }:
    let
      mkHomeConfiguration =
        {
          system,
          username,
          homeDirectory,
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [ ./home.nix ];

          # username/homeDirectory differ per machine (e.g. macOS vs WSL),
          # so they're passed in here rather than hardcoded in home.nix.
          extraSpecialArgs = { inherit username homeDirectory; };
        };
    in
    {
      homeConfigurations = {
        # macOS (Apple Silicon).
        "hiroaki.hara-darwin-aarch64" = mkHomeConfiguration {
          system = "aarch64-darwin";
          username = "hiroaki.hara";
          homeDirectory = "/Users/hiroaki.hara";
        };

        # Linux / WSL.
        "hiroh-linux-x86_64" = mkHomeConfiguration {
          system = "x86_64-linux";
          username = "hiroh";
          homeDirectory = "/home/hiroh";
        };
        "hiroh-linux-aarch64" = mkHomeConfiguration {
          system = "aarch64-linux";
          username = "hiroh";
          homeDirectory = "/home/hiroh";
        };
      };
    };
}
