{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rosstimson";
  home.homeDirectory = "/Users/rosstimson";

  home.packages = with pkgs; [
    adoptopenjdk-hotspot-bin-11
    awscli2
    babashka
    bat
    boot
    clj-kondo
    clojure-lsp
    direnv
    du-dust
    exa
    fd
    git
    go
    hunspell
    hunspellDicts.en_GB-large
    leiningen
    lorri
    mg
    nodePackages.eslint
    nodePackages.pyright
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodejs
    poetry
    procs
    python3
    python38Packages.jedi
    python38Packages.pip
    ripgrep
    rust-analyzer
    rustup
    sccache
    shellcheck
    skim
    starship
    terraform
    terraform-ls
    tmux
    vim
    wget
    yarn
    zoxide
    zsh
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
