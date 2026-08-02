{ config, pkgs, lib, username, homeDirectory, ... }:

{
  # username/homeDirectory are passed in per-machine via extraSpecialArgs in
  # flake.nix, since they differ between machines (e.g. macOS vs WSL).
  home.username = username;
  home.homeDirectory = homeDirectory;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    pkgs.buf
    pkgs.ktlint
    pkgs.uv
    pkgs.basedpyright
    pkgs.gh
    pkgs.tmux
    pkgs.starship
    pkgs.k9s
    pkgs.gopls
    pkgs.kubernetes-helm
    pkgs.helmfile
    pkgs.htop
    pkgs.rust-analyzer
    pkgs.shellcheck
    pkgs.terraform-ls
    pkgs.tree
    pkgs.cmake
    pkgs.cargo-binstall
    pkgs.coreutils-prefixed
    pkgs.fd
    pkgs.postgresql
    pkgs.libtool
    pkgs.pandoc
    pkgs.sqlite
    pkgs.xz
    pkgs.yamlfmt
    pkgs.google-cloud-sdk
    pkgs.codex
    pkgs.cowsay
    pkgs.fortune

    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
    # macOS-only: AeroSpace window borders, and running Linux VMs via Lima
    # (native Docker/containerd on Linux makes Lima unnecessary there).
    pkgs.jankyborders
    pkgs.lima

    # macOS-only: docker client + compose plugin, paired with Lima as the
    # daemon. On Linux/WSL, install docker (client + daemon) via the distro's
    # package manager instead - home-manager can't manage the system-level
    # systemd service the daemon needs there.
    pkgs.docker
    pkgs.docker-compose
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  }
  // lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".docker/cli-plugins/docker-compose".source = "${pkgs.docker-compose}/bin/docker-compose";
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/hiroaki.hara/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
