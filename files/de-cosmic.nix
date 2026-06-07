{ pkgs, lib, perloc, stateloc, user, host, ... }: let

my-lib = import ./my-lib.nix { inherit pkgs lib; };
inherit (my-lib) linked linked-script;

folded = { imports = lib.attrsets.attrValues parts; };

parts = {

# =============================================================================
cosmic-base = {
  # Enable COSMIC
  services.desktopManager.cosmic.enable = true;
  # services.displayManager.cosmic-greeter.enable = true;
  # services.displayManager.defaultSession = "cosmic";

  # Ostensibly improves performance
  services.system76-scheduler.enable = true;

  # Persist COSMIC config
  home-manager.users.${user} = {
    xdg.configFile."cosmic".source = linked (stateloc + "/cosmic-de");
  };
};

# =============================================================================
cosmic-ff-theming = {
  # Ensure Firefox uses COSMIC theming
  programs.firefox.preferences = {
    "widget.gtk.libadwaita-colors.enabled" = false;
  };
};

# =============================================================================
# Set the background image
background-image = {
  # My saved COSMIC config expects the image to be here
  home-manager.users.${user}.home.file.".background-image.png".source = ./background.png;
};

};

in folded
