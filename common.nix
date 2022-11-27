{ perloc   # Opt-in state root ("perloc" = "persist" + "location")
, user     # User name
, host     # Host name (computer name)
}:

if !(builtins.isString perloc)
then builtins.throw ''
  The supplied state location '${builtins.toString perloc}' is a ${builtins.typeOf perloc}, but must be a string. This is to mitigate the risk of accidentally copying the entire state location to the nix store, should this nix expression mistakenly coercing the path it to a string. (This has happened before!)
''
else if !(
  builtins.substring 0 1 perloc == "/"
  && builtins.substring (builtins.stringLength perloc - 1) (builtins.stringLength perloc) perloc != "/"
) then builtins.throw ''
  The state location must start with a slash and must not end with a slash; the supplied value '${builtins.toString perloc}' does not respect this
''
else

{ lib, config, pkgs, ... }: let

stateloc = perloc + "/state";
secrets = (import (perloc + "/secrets.nix")).nixos;

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
  services.xserver.libinput = {
    enable = true;
    touchpad.tappingDragLock = false;
    touchpad.transformationMatrix = "2 0 0 0 2 0 0 0 1";
    touchpad.accelSpeed = "0";
    mouse.tappingDragLock = false;
    mouse.accelSpeed = "0";
  };

  # bluetooth
  hardware.bluetooth.enable = true;
  # To connect:
  # > bluetoothctl
  # > scan on
  # turn on device discovery mode
  # > connect <hex>

  # enable x
  services.xserver.enable = true;

  # internet
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.scanRandMacAddress = false;
    # ^ seems to be needed for some networks
  environment.etc."NetworkManager/system-connections".source =
    linked (stateloc + "/etc.NetworkManager.system-connections");

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
    vim wget htop file zip unzip bc silver-searcher colordiff entr pv
    magic-wormhole nix-prefetch nix-prefetch-git ntfs3g sshfs rclone
    drive bpytop killall
    ghc nodejs python3 cabal-install  # for one-off uses
    (linkedBin (with pkgs; [ nodejs curl ]) "" ./files/scripts/loom-put.sh)
    (linkedBin [] ''TRASH_LOC=${stateloc + "/trash"}'' ./files/scripts/del.sh)
  ];

  environment.interactiveShellInit = ''
    # source bashrc on bash only
    [ -n "$BASH" ] && source ${builtins.toString ./files/bashrc}
  '';

  networking.firewall.allowedTCPPorts = [ 8000 ];

  # printing???
  services.printing.enable = true;

  # Disable ipv6; it's messing with npm n stuff
  boot.kernel.sysctl."net.ipv6.conf.eth0.disable_ipv6" = true;

};

# =============================================================================
nixos-bootstrapping = {
  environment.etc."nixos/configuration.nix".text =
    "import ${perloc}/config/systems/${host}.nix";

  environment.interactiveShellInit = ''
    export NIX_PATH="$NIX_PATH:secrets=${perloc}/secrets.nix"
  '';
};

# =============================================================================
nix-caches = {
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://shpadoinkle.cachix.org"
    ];
    binaryCachePublicKeys = [
      "shpadoinkle.cachix.org-1:aRltE7Yto3ArhZyVjsyqWh1hmcCf27pYSmO1dPaadZ8="
    ];
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
borg = let
  borg = pkgs.borgbackup;
in {
  environment.systemPackages = [ borg ];

  # nb Some to-be-backed-up files are root-owned, so use borg with root

  systemd.services.system-backup = {
    description = "Regular system backup";
    startAt = "*-*-* 04:00:00";
    serviceConfig = { User = "root"; };
    environment = {
      # Bypass check when accessing 'previously unknown repo'
      # /root/.ssh isn't persisted, so after every reboot the repo will be 'unknown'
      BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK = "yes";
    };
    script = ''
      ${borg}/bin/borg create \
        --rsh 'ssh -F ${stateloc + "/ssh/config"}' \
        -e /per/dgn \
        -p \
        u309918@u309918.your-storagebox.de:/home/backups::'${host}-backup-{now}' \
          /per
    '';
    # WANT^ '/per' and '/per/dgn' ought to be configurable by-system
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
    home.file.".ssh".source = linked (stateloc + "/ssh");
  };
};

# =============================================================================
xmonad-wm = let

  latuc = let
    src = pkgs.fetchFromGitHub {
        owner = "quelklef";
        repo = "latuc";
        rev = "8735720dfa292baa4ffbb072cef9c41ad991cc40";
        sha256 = "sha256-vpYwbP8aPWneST/ORhtCGMOlRVMgt4KkXC2Zc9Lc5dc=";
      };
    in pkgs.writeScriptBin "latuc" ''
      echo "$1" | ${import src { inherit pkgs; }}/bin/latuc
    '';

  nifty = let
    src = /per/dev/nifty-launcher;
    /*
    src = pkgs.fetchFromGitHub {
        owner = "quelklef";
        repo = "nifty-launcher";
        rev = "fc7e2264824e2fdc76ec3c16c4d35b6597346ba5";
        sha256 = "sha256-U+Ll3dE2uj4W3RvApF3U2z2V94DWSWEF6badH+MopuY=";
      };
    */
    nifty-state = stateloc + "/nifty-launcher/";
    in pkgs.writeScriptBin "nifty" ''
      ${import src {}}/bin/nifty \
        ${nifty-state + "nifty.js"} \
        2>&1 | tee ${nifty-state + "log.log"}
    '';

  hpkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      xmonad-contrib =
        hself.callCabal2nix "xmonad-contrib" /per/dev/xmonad-contrib {};
    };
  };

  xmo-ghc = hpkgs.ghcWithPackages (p: with p; [
    xmonad xmonad-utils xmonad-contrib
    xmobar raw-strings-qq temporary
  ]);

  # Runtime dependencies for the xmonad/xmobar configs
  # These unfortunately will get leaked into the system PATH (WANT: fix)
  xmo-deps = [
    xmo-ghc
    pkgs.bash pkgs.coreutils pkgs.scrot pkgs.xclip pkgs.acpi pkgs.light

    nifty latuc  # WANT: move these

    (pkgs.writeScriptBin "alacritty-random"
      (builtins.readFile ./files/alacritty-with-random-theme.sh))
  ];

  my-xmonad = pkgs.writeScriptBin "xmonad" ''
    export PATH=${pkgs.lib.strings.makeBinPath xmo-deps}''${PATH:+:}''${PATH:+$PATH}
    export XMONAD_XMESSAGE=${pkgs.coreutils}/bin/true  # disable xmonad error popup
    exec ${hpkgs.xmonad}/bin/xmonad "$@"
  '';

  my-xmobar = pkgs.writeScriptBin "xmobar" ''
    export PATH=${pkgs.lib.strings.makeBinPath xmo-deps}''${PATH:+:}''${PATH:+$PATH}
    exec ${hpkgs.xmobar}/bin/xmobar "$@"
  '';

in lib.mkIf true {

  services.xserver = {
    displayManager.defaultSession = "none+xmonad";
    windowManager = {
      session = [{
        name = "xmonad";
        start = ''
          systemd-cat -t xmonad -- ${my-xmonad}/bin/xmonad &
          waitPID=$!
        '';
      }];
    };
  };

  home-manager.users.${user} = {
    home.file.".xmonad/xmonad.hs".source = linked ./files/xmonad/xmonad.hs;
    # Modules in lib/ will be available to xmonad.hs
    home.file.".xmonad/lib".source = linked ./files/xmonad/lib;
    xdg.configFile."xmobar/xmobar.hs".source = linked ./files/xmobar/xmobar.hs;
  };

  # These two lines enable a 'light' command which is used in xmonad config to manage backlight
  # nb. Might require a reboot before 'light' can be used without sudo
  programs.light.enable = true;
  users.users.${user}.extraGroups = [ "video" ];

  environment.systemPackages = [

    (pkgs.writeScriptBin "my-xmonad" ''${my-xmonad}/bin/xmonad "$@"'')
    (pkgs.writeScriptBin "my-xmobar" ''${my-xmobar}/bin/xmobar "$@"'')

    # Script to rebuild + rerun config on change to lib/
    # This will not work for changes to xmonad-contrib!
    (pkgs.writeScriptBin "xmonad-lib-devt" ''
      path_append=${pkgs.lib.strings.makeBinPath (with pkgs; [ entr stylish-haskell ])}
      export PATH=''${PATH:+$PATH}''${PATH:+:}''${path_append}
      src=${toString ./files}
      find $src/{xmonad,xmobar} -name '*.hs' | entr bash -c "stylish-haskell -i \$(find $src/{xmonad,xmobar} -name '*.hs')" &
      find $src/xmonad -name '*.hs' | entr -s '${my-xmonad}/bin/xmonad --recompile && ${my-xmonad}/bin/xmonad --restart' &
      find $src/xmobar -name '*.hs' | entr -sr 'pkill xmobar; ${my-xmobar}/bin/xmobar' &
      wait
    '')

  ];

};

# =============================================================================
lightdm = {

  services.xserver.displayManager = {
    lightdm = {
      enable = true;
      background = ./files/background.png;
    };

    # Use auto-login instead of greeter
    autoLogin.enable = true;
    autoLogin.user = user;
    lightdm.greeter.enable = false;
  };

};

# =============================================================================
git = {
  programs.git = {
    enable = true;
    config = {
      user.name = "Maynard";
      user.email = "me@maynards.site";
      init.defaultBranch = "main";
      core.sshCommand = ''
        ssh -F '${stateloc + "/ssh/config"}'
      '';
      # ^ nb idk why this is needed, but w/e
    };
  };
};

# =============================================================================
chrome = {
  environment.systemPackages = with pkgs; [ google-chrome ];
  # Chrome is made available but its state is not persisted
};

# =============================================================================
firefox = {
  environment.systemPackages = with pkgs; [ firefox ];
  home-manager.users.${user} = {
    home.file.".mozilla/firefox".source = linked (stateloc + "/firefox");
  };
};

# =============================================================================
telegram = {
  environment.systemPackages = with pkgs; [ tdesktop ];
  home-manager.users.${user} = {
    xdg.dataFile."TelegramDesktop".source = linked (stateloc + "/telegram");
  };
};

# =============================================================================
keepassxc = {
  environment.systemPackages = with pkgs; [ keepassxc ];
  home-manager.users.${user} = {
    xdg.configFile."keepassxc".source = linked (stateloc + "/keepassxc/config");
    home.file.".cache/keepassxc".source = linked (stateloc + "/keepassxc/cache");
  };
};

# =============================================================================
peek = {
  environment.systemPackages = with pkgs; [ peek ];
  programs.dconf.enable = true;
};

# =============================================================================
obs = {
  environment.systemPackages = with pkgs; [ obs-studio ];
  home-manager.users.${user} = {
    xdg.configFile."obs-studio".source = linked (stateloc + "/obs");
  };
};

# =============================================================================
discord = let

  # See github.com/NixOS/nixpkgs/issues/94806 and reddit.com/r/NixOS/comments/i5bpjy
  # use 'get-current-discord-version' to bump version number and hash
  ver = "0.0.21";
  sha = "1pw9q4290yn62xisbkc7a7ckb1sa5acp91plp2mfpg7gp7v60zvz";
  discord = pkgs.discord.overrideAttrs (_: {
    src = builtins.fetchTarball
      { url = "https://dl.discordapp.net/apps/linux/${ver}/discord-${ver}.tar.gz";
        sha256 = sha;
      };
  });

  # WANT: pin deps here. Where does nix-prefetch-url come from?
  get-current-discord-version =
    pkgs.writeScriptBin "get-current-discord-version" ''
      set -euo pipefail
      ver=$(
        curl -s 'https://discord.com/api/download?platform=linux' \
          | grep -oP '(?<=discord-)[.\d]+(?=\.)' \
          | head -n1
      )
      echo "Using version: $ver"
      url="https://dl.discordapp.net/apps/linux/$ver/discord-$ver.tar.gz"
      echo "Using url: $url"
      sha=$(nix-prefetch-url --unpack "$url")
      echo
      echo "version = $ver"
      echo "sha256  = $sha"
    '';

in {

  environment.systemPackages = [ discord get-current-discord-version ];

  home-manager.users.${user} =
    let
      # file/directory names to persist from discord/
      persistThese = [
        "Local Storage"
        "Session Storage"
        "Cookies"
        "Network Persistent State"
        "Preferences"
      ];
    in {
      xdg.configFile =
        pkgs.lib.lists.foldl
          (soFar: persistThis: soFar // {
            ${"discord/" + persistThis}.source =
              linked (stateloc + "/discord/" + persistThis);
          }) {} persistThese;
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
nixops = {
  environment.systemPackages = with pkgs; [ nixops ];
  nixpkgs.config.permittedInsecurePackages = [ "python2.7-pyjwt-1.7.1" ];
  home-manager.users.${user} = {
    home.file.".nixops".source = linked (stateloc + "/nixops");
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
        export _Z_DATA=${stateloc + "/z/zfile"}
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
redshift = {
  services.redshift.enable = true;
  location.latitude = 37.8715;
  location.longitude = -122.2730;
};

# =============================================================================
cpu-control = let

  scriptname = "set-cpu-governors";

in {

  security.wrappers.${scriptname} = {
    source = pkgs.writeScript scriptname ''
      #!${pkgs.bash}/bin/bash
      gov="$1"
      for i in {0..7}; do
        sudo cpufreq-set -g "$gov" -c $i
      done
    '';
    setuid = true;  # nb Set +s so it can be run from ulauncher
    owner = "root";
    group = "root";
  };

  environment.systemPackages = [

    pkgs.cpufrequtils

    (pkgs.writeScriptBin "perf.fast" ''
      ${scriptname} performance "$@"
    '')

    (pkgs.writeScriptBin "perf.slow" ''
      ${scriptname} powersave "$@"
    '')

    (pkgs.writeScriptBin "perf.which" ''
      # assumes all CPUs are set to the same governor
      mode=$(cpufreq-info | grep 'The governor' | awk -F\" '{print $2}' | head -n1)

      case "$mode" in
        performance) echo -n fast ;;
        powersave) echo -n slow ;;
        *) echo -n '???' ;;
      esac
    '')

    (pkgs.writeScriptBin "perf.switch" ''
      case "$(perf.which)" in
        fast) perf.slow ;;
        slow) perf.fast ;;
        *) perf.fast ;;
      esac
    '')

  ];

};

}; in result
