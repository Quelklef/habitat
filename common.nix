{ perloc   # Opt-in state root ("perloc" = "persist" + "location")
, user     # User name
, host     # Host name (computer name)
}:

if !(builtins.isString perloc)
then builtins.throw ''
  The supplied state location '${builtins.toString perloc}' is a ${builtins.typeOf perloc}, but must be a string. (Path values are not allowed; this is to mitigate the risk of accidentally copying the entire state location to the nix store, should the path value be coerced to a string. This has happened before!)
''
else if !(
  builtins.substring 0 1 perloc == "/"
  && builtins.substring (builtins.stringLength perloc - 1) (builtins.stringLength perloc) perloc != "/"
) then builtins.throw ''
  The state location must start with a slash and must not end with a slash; the supplied value '${builtins.toString perloc}' does not respect this
''
else

{ lib, config, pkgs, ... }: let

system = builtins.currentSystem;

# nixpkgs 23.05.1092.c7ff1b9b956
# FIXME: upgrade use-sites
pkgs_2305 =
  import (pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = "c7ff1b9b95620ce8728c0d7bd501c458e6da9e04";
      hash = "sha256-J1bX9plPCFhTSh6E3TWn9XSxggBh/zDD4xigyaIQBy8=";
    }) { inherit system; };

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
  services.libinput = {
    enable = true;
    touchpad.tappingDragLock = false;
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
  nix.settings.auto-optimise-store = true;
  nix.settings.keep-outputs = true;
  nix.settings.keep-derivations = true;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  nixpkgs.config.permittedInsecurePackages = [
    "python-2.7.18.6"  # reached end of life
  ];

  # plug-in packages
  environment.systemPackages = with pkgs; [
    vim
    wget
    killall
    jq
    htop btop
    zip unzip
    silver-searcher ripgrep
    colordiff
    entr
    magic-wormhole
    nix-prefetch nix-prefetch-git
    ghc nodejs python3 cabal-install  # for one-off uses
    tmate
    (linkedBin (with pkgs; [ nodejs curl ]) "" ./files/scripts/loom-put.sh)
    (linkedBin [] ''TRASH_LOC=$HOME/.trash'' ./files/scripts/del.sh)
  ];

  environment.interactiveShellInit = ''
    # source bashrc on bash only
    [ -n "$BASH" ] && source ${builtins.toString ./files/bashrc}
  '';

  networking.firewall.allowedTCPPorts = [ 8000 ];

  # printing???
  services.printing.enable = true;
  services.avahi = { enable = true; nssmdns4 = true; openFirewall = true; };  # auto-discover printers on the local network

  # Disable ipv6; it's messing with npm n stuff
  boot.kernel.sysctl."net.ipv6.conf.eth0.disable_ipv6" = true;

  # From sam
  programs.tmux.extraConfig = ''
    # act like vim
    set-window-option -g mode-keys vi
    setw -g mode-keys vi
    bind h select-pane -L
    bind j select-pane -D
    bind k select-pane -U
    bind l select-pane -R
    #bind-key -r C-h select-window -t :-
    #bind-key -r C-l select-window -t :+
    bind -T copy-mode-vi v send-keys -X begin-selection
    bind -T copy-mode-vi y send-keys -X copy-selection

    # scroll text, not commands, select text within panes
    set-option -g mouse off

    # fast escape
    set-option -g escape-time 0

    # I can't read messages in 750ms
    set-option -g display-time 2000
  '';

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
  nix.settings = {
    substituters = [
      "https://cache.nixos.org/"
      "https://shpadoinkle.cachix.org"
    ];
    trusted-public-keys = [
      "shpadoinkle.cachix.org-1:aRltE7Yto3ArhZyVjsyqWh1hmcCf27pYSmO1dPaadZ8="
    ];
  };
};

# =============================================================================
# Remote directory
dragon = {
  environment.interactiveShellInit = ''
    [ -n "$BASH" ] && source ${builtins.toString ./files/dragon.sh}
  '';

  # Runtime dependency of dragon.sh
  environment.systemPackages = [ pkgs.sshfs ];

  # Allows sshfs to use allow_root
  programs.fuse.userAllowOther = true;
};

# =============================================================================
# System backups
#
/*

System restore instructions:

- First use 'borg list <borg-repo>' to list available
  archives
- Then use EITHER
  - 'sudo borg extract --progress <borg-repo>::<archive-name> ./local/path'
    to directly download a chosen archive to local; or
  - 'sudo borg mount <borg-repo>::<archive-name> ./local/path'
    to mount an archive locally.
    One can follow this up by extracting with rsync, which
    is incremental ('borg extract' is not)
  Sudo is necessary (I think?) because some archive paths will
  be sudo-owned. Not really positive tho.

REMARKS:
- If on an existing machine, can use 'my-borg' in place
  of 'borg' and '$BORG_REPO' in place of '<borg-repo>'
- If on a *new* machine, will need to reset the Hetzner
  box password and then use borg manually. Hetzner boxes
  use port 23, so you'll need to use 'borg --rsh "ssh -p23"'

*/
borg = let
  borg-repo = "u309918@u309918.your-storagebox.de:/home/backups";
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
        --exclude ${perloc}/dgn \
        --exclude ${perloc}/work/live247/music/blobs/ \
        --exclude ${perloc}/dev/zoom-dl/files \
        --exclude ${perloc}/dev/zoom-dl/files.copy \
        --progress \
        ${borg-repo}::'${host}-backup-{now}' ${perloc}
    '';
  };

  home-manager.users.${user}.programs.bash.bashrcExtra = ''
    export BORG_REPO='${borg-repo}'
    alias my-borg="borg --rsh 'ssh -F ${stateloc + "/ssh/config"}'"
  '';
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
        ref = "release-24.05";
        rev = "2f23fa308a7c067e52dfcc30a0758f47043ec176";
      };
    in import "${home-manager}/nixos")
  ];
  home-manager.users.root = {
    targets.genericLinux.enable = true;
    xdg.enable = true;
    manual.manpages.enable = false;  # https://discourse.nixos.org/t/x/11012
    home.stateVersion = config.system.stateVersion;  # FIXME: should really be set per-system
  };
  home-manager.users.${user} = {
    targets.genericLinux.enable = true;
    xdg.enable = true;
    manual.manpages.enable = false;  # https://discourse.nixos.org/t/x/11012
    home.stateVersion = config.system.stateVersion;  # FIXME: should really be set per-system
  };
};

# =============================================================================
home-ssh = {
  home-manager.users.${user}.home.file.".ssh".source = linked (stateloc + "/ssh");
};

# =============================================================================
background-image = {
  home-manager.users.${user}.home.file.".background-image".source = ./files/background.png;
  services.xserver.desktopManager.wallpaper.mode = "fill";
};

# =============================================================================
notifications = {
  environment.systemPackages = [
    pkgs.libnotify  # provides notify-send
  ];

  # Modified from <https://github.com/nix-community/home-manager/blob/9ba7b3990eb1f4782ea3f5fe7ac4f3c88dd7a32c/modules/services/dunst.nix#L181-L195>
  systemd.user.services.dunst = {
    description = "dunst notification daemon";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.dunst}/bin/dunst -config ${linked ./files/dunstrc}";
    };
  };
};

# =============================================================================
nifty = let

  nifty = let
    src = pkgs.fetchFromGitHub {
        owner = "quelklef";
        repo = "nifty-launcher";
        rev = "e6ad9a9aba5415e1dbc77fcbe060051a3dfe17bb";
        sha256 = "0p9kk4l8ai4xj66rym1njb178lhikcaklrjslkyjkhlclld3a8cr";
      };
    in pkgs.writeScriptBin "nifty" ''
      export PATH=${pkgs.lib.strings.makeBinPath runtime-deps}''${PATH:+:}''${PATH:+$PATH}
      ${import src {}}/bin/nifty \
        ${builtins.toString ./files/nifty-launcher/nifty.js} \
        2>&1 | tee ${stateloc + "/nifty-launcher/log.log"}
    '';

  runtime-deps = [
    latuc
    pkgs.pmutils  # For pm-suspend
  ];

  latuc = let
    original = pkgs.fetchFromGitHub {
        owner = "quelklef";
        repo = "latuc";
        rev = "9929f933a030c1dea72164e79a75ca4a27706752";
        sha256 = "1cz9i5fp220y8xac9r8c1k811v22z1pbbd6am116cbvvxn81a3py";
      };
    in pkgs.writeScriptBin "latuc" ''
      echo "$1" | ${import original { inherit pkgs; }}/bin/latuc
    '';

in {
  environment.systemPackages = [ nifty ];
};


# =============================================================================
xmonad-wm = let

  # == XMobar == #

  # Runtime dependencies
  xmobar-deps = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ xmobar raw-strings-qq ]))
    pkgs.bash pkgs.coreutils pkgs.acpi
  ];

  my-xmobar = pkgs.writeScriptBin "xmobar" ''
    export PATH=${pkgs.lib.strings.makeBinPath xmobar-deps}''${PATH:+:}''${PATH:+$PATH}
    exec ${pkgs.xmobar}/bin/xmobar "$@"
  '';


  # == XMonad == #

  xmonad-contrib-src =
    let
      original = pkgs.fetchFromGitHub {
        owner = "quelklef";
        repo = "xmonad-contrib";
        rev = "bccf2ef778dd215bda19332bc060476fc7d55a69";
        sha256 = "0pza9kb4prp2cvx7s041pfr2znai39cqq01zk28ad3bizr596is9";
      };

      # Suppress -Werror (FIXME: upstream into quelklef/xmonad-contrib)
      # Also, forcibly lower the xmonad version bound from 0.18 to 0.17.2 (which is the
      # version in nixpkgs). The only breaking change between the two is dropping
      # support for GHC 8.4, which I doubt affects us.
      patched = pkgs.runCommand "xmonad-contrib-patched" {} ''
        cp -r ${original}/. . && chmod +w -R .
        ${pkgs.gnused}/bin/sed -i 's/-Werror/-Wwarn/g' ./XMonad/WorkspaceLayout/*.hs
        ${pkgs.gnused}/bin/sed -i 's/xmonad >= 0.18.0/xmonad >= 0.17.0/g' ./xmonad-contrib.cabal
        mkdir -p $out && mv ./* $out
      '';
    in patched;

  xmonad-hpkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      xmonad-contrib = hself.callCabal2nix "xmonad-contrib" xmonad-contrib-src {};
    };
  };

  # Runtime dependencies
  # Unfortunately, these will leak into the PATH of all children processes (ie, most processes) (WANT: fix)
  xmonad-deps = [
    (xmonad-hpkgs.ghcWithPackages (p: with p; [ xmonad xmonad-utils xmonad-contrib lens ]))
    my-xmobar
    pkgs.bash pkgs.coreutils pkgs.scrot pkgs.xclip pkgs.light
  ];

  my-xmonad = pkgs.writeScriptBin "xmonad" ''
    export PATH=${pkgs.lib.strings.makeBinPath xmonad-deps}''${PATH:+:}''${PATH:+$PATH}
    export XMONAD_XMESSAGE=${pkgs.coreutils}/bin/true  # disable xmonad error popup
    exec ${xmonad-hpkgs.xmonad}/bin/xmonad "$@"
  '';

in lib.mkIf true {

  services.displayManager.defaultSession = "none+xmonad";
  services.xserver.windowManager.session = [{
    name = "xmonad";
    start = ''
      systemd-cat -t xmonad -- ${my-xmonad}/bin/xmonad &
      waitPID=$!
    '';
  }];

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

  environment.systemPackages = let
    src = toString ./files;
  in [

    (pkgs.writeScriptBin "my-xmonad" ''${my-xmonad}/bin/xmonad "$@"'')
    (pkgs.writeScriptBin "my-xmobar" ''${my-xmobar}/bin/xmobar "$@"'')

    # Rebuilds and reruns xmonad on config change
    # Does not react to changes to xmonad-contrib!
    (pkgs.writeScriptBin "xmonad-devt" ''
      path_append=${pkgs.lib.strings.makeBinPath (with pkgs; [ entr stylish-haskell ])}
      export PATH=''${PATH:+$PATH}''${PATH:+:}''${path_append}

      find ${src}/xmonad -name '*.hs' | entr bash -c '
        clear
        echo "Formatting" && stylish-haskell -i $(find ${src}/xmonad -name \*.hs) || exit 1
        echo "Reupping xmonad"
        ${my-xmonad}/bin/xmonad --recompile || exit 1
        ${my-xmonad}/bin/xmonad --restart
      '
    '')

    # Rebuilds and reruns xmobar on config change
    (pkgs.writeScriptBin "xmobar-devt" ''
      path_append=${pkgs.lib.strings.makeBinPath (with pkgs; [ entr stylish-haskell ])}
      export PATH=''${PATH:+$PATH}''${PATH:+:}''${path_append}

      # Holy shit so for some reason xmobar only seems to work properly the SECOND time it's
      # started after being recompiled
      find ${src}/xmobar -name '*.hs' | entr -r bash -c '
        clear
        echo "Formatting" && stylish-haskell -i $(find ${src}/xmobar -name \*.hs) || exit 1
        echo "Reupping xmobar ... please wait until the SECOND execution of xmobar"
        pkill xmobar
        ${my-xmobar}/bin/xmobar --recompile &
        pid=$! ; sleep 3 ; kill $pid
        echo "Second execution!"
        ${my-xmobar}/bin/xmobar
        echo "ok"
      '
    '')

  ];

};

# =============================================================================
lightdm = {

  services.xserver.displayManager.lightdm = {
    enable = true;
    background = ./files/background.png;
  };

  # Use auto-login instead of greeter
  services.xserver.displayManager.lightdm.greeter.enable = false;
  services.displayManager = {
    autoLogin.enable = true;
    autoLogin.user = user;
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

      # idk why this is needed, but whatever
      core.sshCommand = "ssh -F '${stateloc + "/ssh/config"}'";

      # Use difftastic for diffing
      # No purescript support though :-(
      diff.external = "${pkgs.difftastic}/bin/difft";
      alias."my-show" = "show --ext-diff";

      # Have 'git detach' enter into detached HEAD state
      alias."my-detach" = ''!bash -c 'git checkout "$(git rev-parse HEAD)"' '';
      advice.detachedHead = false;  # Don't display help when entering detached HEAD
    };
  };
};

# =============================================================================
chrome = {
  environment.systemPackages = with pkgs; [ google-chrome ];
  home-manager.users.${user} = {
    xdg.configFile."google-chrome".source = linked (stateloc + "/google-chrome");
  };
  # nb Persisted for Scott Chrome profile
};

# =============================================================================
firefox = {
  environment.systemPackages = [
    (pkgs.writeScriptBin "firefox" ''${pkgs.firefox}/bin/firefox -profile ${linked (stateloc + "/firefox-profile")} "$@"'')
    (pkgs.writeScriptBin "firefox-alt" ''${pkgs.firefox}/bin/firefox -profile ${linked (stateloc + "/firefox-alt-profile")} "$@"'')
  ];

  # This sets the cursor theme for everything, not just Firefox; however, I am only setting the cursor
  # theme in the first place because some cursors were broken on Firefox.
  home-manager.users.${user}.home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.posy-cursors;
    name = "Posy_Cursor_Black_125_175";
    size = 28;
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
nushell = {
  environment.systemPackages = [ pkgs_2305.nushell ];
  home-manager.users.${user} = {
    xdg.configFile."nushell".source = linked (stateloc + "/nushell");
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
  ver = "0.0.25";
  sha = "12yrhlbigpy44rl3icir3jj2p5fqq2ywgbp5v3m1hxxmbawsm6wi";
  discord = pkgs.discord.overrideAttrs (_: {
    src = builtins.fetchTarball
      { url = "https://dl.discordapp.net/apps/linux/${ver}/discord-${ver}.tar.gz";
        sha256 = sha;
      };
  });

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
wezterm = {
  environment.systemPackages = with pkgs; [ wezterm ];
  home-manager.users.${user} = {
    xdg.configFile."wezterm/wezterm.lua".source = linked ./files/wezterm.lua;
  };
};

# =============================================================================
nixops = {
  environment.systemPackages = [ pkgs_2305.nixops ];
  nixpkgs.config.permittedInsecurePackages = [ "python2.7-pyjwt-1.7.1" ];
  home-manager.users.${user} = {
    home.file.".nixops".source = linked (stateloc + "/nixops");
  };
};

# =============================================================================
# https://github.com/ajeetdsouza/zoxide
zoxide = let
  zoxide = pkgs.zoxide;
in {
  environment.systemPackages = [ zoxide ];
  home-manager.users.${user} = {
    xdg.dataFile."zoxide".source = linked (stateloc + "/zoxide");
    programs.bash = {
      enable = true;
      bashrcExtra = ''
        eval "$( ${zoxide}/bin/zoxide init bash )"
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
      max_cpu=$( cpufreq-info | grep -Po '(?<=CPU )\d+' | tail -n1 )
      governor="$1"
      for (( i = 0; i <= $max_cpu; i++ )); do
        sudo cpufreq-set -g "$governor" -c $i
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

# =============================================================================
steam = {
  environment.systemPackages = [ pkgs.steam ];
  home-manager.users.${user} = {
    xdg.dataFile."Steam".source = linked (stateloc + "/steam/home-.local-share-Steam");
    home.file.".steam".source = linked (stateloc + "/steam/home-.steam");
  };
};

}; in result
