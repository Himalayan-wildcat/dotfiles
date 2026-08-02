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
        system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [ ./home.nix ];

          # Optionally use extraSpecialArgs
          # to pass through arguments to home.nix
        };
    in
    {
      homeConfigurations = {
        # macOS (Apple Silicon).
        "hiroaki.hara-darwin-aarch64" = mkHomeConfiguration "aarch64-darwin";

        # Linux / WSL.
        "hiroaki.hara-linux-x86_64" = mkHomeConfiguration "x86_64-linux";
        "hiroaki.hara-linux-aarch64" = mkHomeConfiguration "aarch64-linux";
      };
    };
}
