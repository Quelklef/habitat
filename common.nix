{ stateloc  # Opt-in state directory location
, secrets  # Password hashes, etc
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
# Boot- and erase your darlings- related config
boot = {
  boot.supportedFilesystems = [ "zfs" ];
  boot.initrd.postDeviceCommands = lib.mkAfter "zfs rollback -r rpool/eyd/root@blank";
};

# =============================================================================
generic-system-config = {

  users = {
    mutableUsers = false;
    users.root.hashedPassword = secrets.password;
    users.lark = {
      isNormalUser = true;
      home = "/home/lark";
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

  # enable x
  services.xserver.enable = true;

  # internet
  networking.networkmanager.enable = true;
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

  # need tailscale for work
  services.tailscale.enable = true;
  networking.firewall.checkReversePath = "loose";

  # plug-in packages
  environment.systemPackages = with pkgs; [
    vim
    kakoune
    wget
    htop
    silver-searcher
    entr
    nix-prefetch-git
    nix-prefetch
    (linkedBin (with pkgs; [ nodejs curl ]) "" ./files/scripts/loom-put.sh)
    (linkedBin [] "TRASH_LOC=${builtins.toString (stateloc + /trash)}" ./files/scripts/del.sh)
  ];

  environment.interactiveShellInit = ''
    if [ -n "$BASH" ]; then
      # source bashrc on bash only
      source ${builtins.toString ./files/bashrc}
    fi
  '';

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
  };
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
  home-manager.users.lark = {
    targets.genericLinux.enable = true;
    xdg.enable = true;
  };
};

# =============================================================================
home-manager-generic = {
  home-manager.users.lark = {
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

  home-manager.users.lark = {
    home.file.".i3/config".source = linked ./files/i3/i3config;
    home.file.".i3/i3status-config".source = linked ./files/i3/i3status-config;
  };

  # packages used by i3 config
  environment.systemPackages = with pkgs; [
    (import ./files/i3/i3wsgroups.nix { inherit pkgs; })  # .. for workspace groups
    scrot  # .. for screenshots
    xclip  # .. for screenshots
    acpi  # .. for battery info in status bar
  ];

  # Enables 'light' command which is used in i3 config to manage backlight
  # nb. Might require a reboot before 'light' can be used without sudo
  programs.light.enable = true;
  users.users.lark.extraGroups = [ "video" ];

  # TODO: a fair amount of the i3 config (i3-status.sh in particular)
  # is system-specific (eg assumes existence of a battery) and therefore
  # really belongs in ./systems/lake

};

# =============================================================================
lightdm = {
  services.xserver = {
    displayManager.defaultSession = "none+i3";
    displayManager.lightdm = {
      enable = true;
      background = ./files/background.png;
      greeters.tiny.enable = true;

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

      greeters.tiny.label.user = "Username";
      greeters.tiny.label.pass = "Password";
      greeters.tiny.extraConfig = builtins.readFile ./files/lightdm-tiny-config.h;
    };
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
  home-manager.users.lark = {
    xdg.configFile."google-chrome".source = linked (stateloc + /google-chrome);
  };
};

# =============================================================================
thunderbird = {
  environment.systemPackages = with pkgs; [ thunderbird ];
  home-manager.users.lark = {
    home.file.".thunderbird".source = linked (stateloc + /thunderbird);
  };
  environment.interactiveShellInit = ''alias thunderbird="thunderbird --profile ~/.thunderbird/q2te5qzd.default-release"'';
  # ^ Not totally sure why this is necessary but whatever
};

# =============================================================================
telegram = {
  environment.systemPackages = with pkgs; [ tdesktop ];
  home-manager.users.lark = {
    xdg.dataFile."TelegramDesktop".source = linked (stateloc + /telegram);
  };
};

# =============================================================================
discord = {
  environment.systemPackages = with pkgs; [ discord ];
  home-manager.users.lark = {
    xdg.configFile."discord".source = linked (stateloc + /discord);
  };
};

# =============================================================================
kakoune = {
  environment.systemPackages = with pkgs; [ xsel ];
  home-manager.users.lark = {
    xdg.configFile."kak/kakrc".source = linked ./files/kakrc;
  };
};

# =============================================================================
alacritty = {
  environment.systemPackages = with pkgs; [ alacritty ];
  home-manager.users.lark = {
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
  home-manager.users.lark = {
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
  home-manager.users.lark = {
    home.file.".nixops".source = linked (stateloc + /nixops);
  };
};

# =============================================================================
z = {
  # https://github.com/rupa/z
  home-manager.users.lark = {
    programs.bash = {
      enable = true;
      # This has to happen specifically in the home-manager bashrc, idk why
      bashrcExtra = ''
        export _Z_DATA=${builtins.toString (stateloc + /z/zfile)}
        export _Z_OWNER=lark
        mkdir -p "$(dirname "$_Z_DATA")"
        source ${builtins.fetchurl
                    { url = "https://raw.githubusercontent.com/rupa/z/master/z.sh";
                      sha256 = "03lvs6wfd5sd10z4ygm5v5smvgnqzgkka0qkjkjkryqssf647r4q";
                    }}
      '';
    };
  };
};

}; in result
