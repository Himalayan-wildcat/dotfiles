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
        system: module:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [ module ];
        };
    in
    {
      homeConfigurations = {
        # macOS (Apple Silicon).
        "hiroaki.hara-darwin-aarch64" = mkHomeConfiguration "aarch64-darwin" ./home-darwin.nix;

        # Linux / WSL. Username differs per machine, so each config points
        # at its own home-linux.nix owner rather than a shared file.
        "hiroh-linux-x86_64" = mkHomeConfiguration "x86_64-linux" ./home-linux.nix;
        "hiroh-linux-aarch64" = mkHomeConfiguration "aarch64-linux" ./home-linux.nix;
      };
    };
}
