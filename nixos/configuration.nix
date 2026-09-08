# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Custom packages
  kde-blur = pkgs.kdePackages.callPackage ./pkgs/kde-blur/nix/package.nix { };
in
{
  imports = [ ./hardware-configuration.nix ];

  # Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hostname
  networking.hostName = "nixos";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Networking
  networking.networkmanager.enable = true;

  # Time Zone
  time.timeZone = "America/Sao_Paulo";

  # Keymap
  services.xserver.xkb = {
    layout = "br";
    variant = "";
  };

  # Console Keymap
  console.keyMap = "br-abnt2";

  # Locale settings
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Pipewire
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # hashylog
  users.users."hashylog" = {
    isNormalUser = true;
    description = "hashylog";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Disable Sudo Password
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # KDE Plasma
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  # System Packages
  environment.systemPackages = with pkgs; [
    wget
    git
    git-credential-oauth
    vscodium
    kde-blur
    emacs-nox
    neovim
    zip
    unzip
    rar
    unrar
    firefox
    zsh
    easyeffects
    gimp
  ];

  # Flatpak
  services.flatpak.enable = true;

  # Fonts
  fonts.packages = with pkgs; [
    jetbrains-mono
    nerd-fonts.jetbrains-mono
  ];

  # Docker
  virtualisation.docker = {
    enable = true;
  };

  # Run generic linux dynamically linked executables
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      # ...
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
