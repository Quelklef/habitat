{ stateloc # Opt-in state directory location
, secrets  # Password hashes, etc
, user     # User name
}:

{ lib, config, pkgs, ... }: let

mylib = rec {

  /*

  Takes a path and returns a derivation producing a symlink to that path

  Derived from github.com/nix-community/home-manager/blob/e622bad16372aa5ada79a7fa749ec78715dffc54/modules/files.nix#L64-L69

  Useful for:
  1. Linking program state such as ~/.config/google-chrome elsewhere
     Integral to the success of opt-in state!
  2. Linking scripts and program configuration, so that a change can
     be seen immediately without requiring a rebuild.
     Also see 'linkedBin'

  */
  linked = path:
    with lib;
    pkgs.runCommandLocal
      (baseNameOf (toString path)) {}
      ''ln -s ${escapeShellArg (toString path)} $out'';

  /*

  Like 'link', but:
  1. Appends the given 'packages' to PATH
  2. Executes the given 'env' before running the script
  3. Removes the filename from the script name (eg, .sh)

  Example:
    linkedBin
      (with pkgs; [ mypkg ])
      "SOME_VAR=some-val"
      ./my-script.sh

  */
  linkedBin = packages: env: path:
    with lib;
    let name = lib.pipe path [ toString baseNameOf (strings.splitString ".") lists.head ];
    in pkgs.writeScriptBin name ''
      #!/usr/bin/env bash
      _path_append=${lib.strings.makeBinPath packages}
      [ -n "$_path_append" ] && PATH="''${PATH:+''${PATH}:}''${_path_append}"
      ${env + "\n"}
      source ${linked path}
    '';

};


result = { inherit mylib parts folded; };

folded = { imports = lib.attrsets.attrValues parts; };

parts = with mylib; {

# =============================================================================
generic-system-config = {

  users = {
    mutableUsers = false;
    users.root.hashedPassword = secrets.password;
    users.${user} = {
      isNormalUser = true;
      home = "/home/${user}";
      extraGroups = [ "wheel" "networkmanager" ];
      hashedPassword = secrets.password;
    };
  };

  security.sudo.wheelNeedsPassword = false;

  # sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # mouse n touchpad
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.tappingDragLock = false;
  services.xserver.libinput.mouse.tappingDragLock = false;

  # bluetooth
  hardware.bluetooth.enable = true;
  # To connect:
  # > bluetoothctl
  # > scan on
  # turn on device discovery mode
  # > pair <hex>
  # > connect <hex>

  # enable x
  services.xserver.enable = true;

  # internet
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.scanRandMacAddress = false;
    # ^ seems to be needed for some networks
  environment.etc."NetworkManager/system-connections".source =
    linked (stateloc + /etc.NetworkManager.system-connections);

  # compositor; basically optional
  services.picom.enable = true;

  # nix config
  nixpkgs.config.allowUnfree = true;
  nix.autoOptimiseStore = true;
  nix.settings.keep-outputs = true;
  nix.settings.keep-derivations = true;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # plug-in packages
  environment.systemPackages = with pkgs; [
    vim
    wget
    htop
    silver-searcher
    colordiff
    entr
    pv
    zip unzip
    nix-prefetch nix-prefetch-git
    ntfs3g
    sshfs rclone
    drive
    bc
    ghc nodejs python3  # for one-off uses
    (linkedBin (with pkgs; [ nodejs curl ]) "" ./files/scripts/loom-put.sh)
    (linkedBin [] "TRASH_LOC=${builtins.toString (stateloc + /trash)}" ./files/scripts/del.sh)
  ];

  environment.interactiveShellInit = ''
    # source bashrc on bash only
    [ -n "$BASH" ] && source ${builtins.toString ./files/bashrc}
  '';

  networking.firewall.allowedTCPPorts = [ 8000 ];

};

# =============================================================================
automatic-system-cleanup = {
  systemd.services.custodian = {
    description = "Regular system cleanup";
    script = ''
      bin=${config.nix.package.out}/bin
      $bin/nix-env --profile /nix/var/nix/profiles/system --delete-generations +25
      $bin/nix-collect-garbage
    '';
    startAt = "06:00";
    enable = false;  # seems to wipe out stuff I need for work, so disable. WANT: fix!
  };
};

# =============================================================================
cachix = {
  imports = [ ./cachix/cachix.nix ];
  environment = {
    systemPackages = with pkgs; [ cachix ];
    interactiveShellInit = let
      cachix-dir = builtins.toString ./cachix;
    in ''
      function cachix {
        echo >&2 "Use 'command cachix', and remember to point it to '${cachix-dir}'"
        return 1
      }
    '';
  };
};

# =============================================================================
# Remote directory
dragon = {
  # Allows sshfs to use allow_root
  programs.fuse.userAllowOther = true;

  environment.interactiveShellInit = ''
    [ -n "$BASH" ] && source ${builtins.toString ./files/dragon.sh}
  '';
};

# =============================================================================
# System backups
kopia = {
  environment.systemPackages = with pkgs; [ kopia ];

  # nb Some to-be-backed-up files are root-owned, so use kopia with root
  #    (Also kopia config is in root's $XDG_CONFIG_HOME)

  home-manager.users.root = {
    xdg.configFile."kopia".source = linked (stateloc + /kopia);
  };

  systemd.services.backup = {
    enable = false;  # temp disabled
    description = "Regular system backup";
    startAt = "hourly";
    script = ''
      ${pkgs.kopia}/bin/kopia snapshot ${builtins.toString stateloc}
    '';
  };
};

# =============================================================================
work-stuff = {
  # tailscale
  services.tailscale.enable = true;
  networking.firewall.checkReversePath = "loose";
};

# =============================================================================
# fixes unicode not working on a tty
tty-unicode-fix = {
  security.wrappers.fbterm = {
    setuid = true;
    owner = "root";
    group = "root";
    source = "${pkgs.fbterm}/bin/fbterm";
  };
  environment.interactiveShellInit = ''
    grep -q /dev/tty <(tty) && exec fbterm
  '';
};

# =============================================================================
home-manager-init = {
  imports = [
    (let home-manager = builtins.fetchGit
      { url = "https://github.com/nix-community/home-manager/";
        rev = "87d30c164849a7471d99749aa4d2d28b81564f69";
      };
    in import "${home-manager}/nixos")
  ];
  home-manager.users.root = {
    targets.genericLinux.enable = true;
    xdg.enable = true;
  };
  home-manager.users.${user} = {
    targets.genericLinux.enable = true;
    xdg.enable = true;
  };
};

# =============================================================================
home-manager-generic = {
  home-manager.users.${user} = {
    home.file.".background-image".source = ./files/background.png;
    home.file.".ssh".source = linked (stateloc + /ssh);
  };
};

# =============================================================================
i3 = {

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
  };

  home-manager.users.${user} = {
    home.file.".i3/config".source = linked ./files/i3/i3config;
    home.file.".i3/i3status-config".source = linked ./files/i3/i3status-config;
  };

  # packages used by i3 config
  environment.systemPackages = with pkgs; [
    (import ./files/i3/i3wsgroups.nix { inherit pkgs; })  # .. for workspace groups
    scrot xclip  # .. for screenshots
    acpi  # .. for battery info in status bar
  ];

  # Enables 'light' command which is used in i3 config to manage backlight
  # nb. Might require a reboot before 'light' can be used without sudo
  programs.light.enable = true;
  users.users.${user}.extraGroups = [ "video" ];

  # WANT: a fair amount of the i3 config (i3-status.sh in particular)
  # is system-specific (eg assumes existence of a battery) and therefore
  # really belongs in ./systems/lake

};

# =============================================================================
lightdm = {
  services.xserver.displayManager = {
    defaultSession = "none+i3";
    lightdm = {
      enable = true;
      background = ./files/background.png;
    };

    # Use auto-login instead of greeter
    # (I keep the greeter config around just in case)
    autoLogin.enable = true;
    autoLogin.user = user;
    lightdm.greeter.enable = false;
  };

  services.xserver.displayManager.lightdm.greeters.tiny = {
    enable = true;

    # making changes to this is a pain. Recommendation:
    # 1. clone https://github.com/tobiohlala/lightdm-tiny-greeter and cd
    # 2. nix-shell -p pkg-config wrapGAppsHook lightdm gtk3 glib
    # 3. make && sudo make install
    # 4. cp /etc/lightdm/lightdm.conf ./conf
    # 5. modify ./conf and set greeters-directory = /usr/share/xgreeters
    # 6. run lightdm --config=./conf --test-mode --debug
    # 7. it doesnt' work
    # 8. give up and use nixos-rebuild switch && systemctl restart display-manager

    # I want to eventually move to a greeter which is HTML/CSS based, so that I
    # can really go crazy with customization

    label.user = "Username";
    label.pass = "Password";
    extraConfig = builtins.readFile ./files/lightdm-tiny-config.h;
  };

};

# =============================================================================
git = {
  programs.git = {
    enable = true;
    config = {
      user.name = "Maynard";
      user.email = "elimaynard923@gmail.com";
      init.defaultBranch = "main";
      core.sshCommand = "ssh -F '${builtins.toString (stateloc + /ssh/config)}'";
      # ^ nb idk why this is needed, but w/e
    };
  };
};

# =============================================================================
chrome = {
  environment.systemPackages = with pkgs; [ google-chrome ];
  home-manager.users.${user} = {
    xdg.configFile."google-chrome".source = linked (stateloc + /google-chrome);
  };
};

# =============================================================================
thunderbird = {
  environment.systemPackages = with pkgs; [ thunderbird ];
  home-manager.users.${user} = {
    home.file.".thunderbird".source = linked (stateloc + /thunderbird);
  };
  environment.interactiveShellInit = ''alias thunderbird="thunderbird --profile ~/.thunderbird/q2te5qzd.default-release"'';
  # ^ Not totally sure why this is necessary but whatever
};

# =============================================================================
telegram = {
  environment.systemPackages = with pkgs; [ tdesktop ];
  home-manager.users.${user} = {
    xdg.dataFile."TelegramDesktop".source = linked (stateloc + /telegram);
  };
};

# =============================================================================
discord = {
  environment.systemPackages = with pkgs; [ discord ];
  home-manager.users.${user} = {
    xdg.configFile."discord".source = linked (stateloc + /discord);
  };
};

# =============================================================================
kakoune = {
  environment.systemPackages = [
    (import ./files/kakoune/kakoune.nix { inherit pkgs; linked = true; })
  ];
};

# =============================================================================
alacritty = {
  environment.systemPackages = with pkgs; [ alacritty ];
  home-manager.users.${user} = {
    xdg.configFile."alacritty/alacritty.yml".source = linked ./files/alacritty.yml;
    xdg.configFile."alacritty/themes" = {
      recursive = true;
      source =
        let fetched =
          pkgs.fetchFromGitHub
            { owner = "eendroroy";
              repo = "alacritty-theme";
              rev = "e8757f9a6537d74947ee7a8f390a627c01390563";
              sha256 = "1mb5ijzhgjd8blxf35v42hkq1iri0w6ymczzlvrfdq359ij16wrv";
            };
        in "${fetched}/themes";
    };
  };
};

# =============================================================================
ulauncher = {
  environment.systemPackages = with pkgs; [ ulauncher ];
  home-manager.users.${user} = {
    xdg.dataFile."ulauncher".source = linked (stateloc + /ulauncher/home.local.share);
    xdg.configFile."ulauncher".source = linked (stateloc + /ulauncher/home.config);
    xsession.enable = true;
    xsession.initExtra = ''
      # This is a bit of a hack. Ideally, invocation would be handled by systemd, not xsession
      systemctl --user --no-block start ulauncher
    '';
  };
  systemd.user.services.ulauncher = {
    description = "ulauncher";
    script = "${pkgs.ulauncher}/bin/ulauncher --dev --hide-window";
  };
};

# =============================================================================
nixops = {
  environment.systemPackages = with pkgs; [ nixops ];
  nixpkgs.config.permittedInsecurePackages = [ "python2.7-pyjwt-1.7.1" ];
  home-manager.users.${user} = {
    home.file.".nixops".source = linked (stateloc + /nixops);
  };
};

# =============================================================================
z = {
  # https://github.com/rupa/z
  home-manager.users.${user} = {
    programs.bash = {
      enable = true;
      # This has to happen specifically in the home-manager bashrc, idk why
      bashrcExtra = ''
        export _Z_DATA=${builtins.toString (stateloc + /z/zfile)}
        export _Z_OWNER=${user}
        mkdir -p "$(dirname "$_Z_DATA")"
        source ${builtins.fetchurl
                    { url = "https://raw.githubusercontent.com/rupa/z/master/z.sh";
                      sha256 = "03lvs6wfd5sd10z4ygm5v5smvgnqzgkka0qkjkjkryqssf647r4q";
                    }}
      '';
    };
  };
};

# =============================================================================
steam = {
  environment.systemPackages = with pkgs; [ steam ];
  hardware.opengl.driSupport32Bit = true;  # https://github.com/NixOS/nixpkgs/issues/47932#issuecomment-447508411

  home-manager.users.${user} = {
    home.file.".steam".source = linked (stateloc + /steam/home.steam);
    home.file.".steampath".source = linked (stateloc + /steam/home.steampath);
    home.file.".steampid".source = linked (stateloc + /steampid/home.steampid);
    xdg.dataFile."Steam".source = linked (stateloc + /steam/home.local.share.steam);
  };
};

}; in result
