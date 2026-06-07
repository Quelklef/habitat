{ pkgs, lib, perloc, stateloc, user, host, ... }: let

my-lib = import ./my-lib.nix { inherit pkgs lib; };
inherit (my-lib) linked linked-script;

folded = { imports = lib.attrsets.attrValues parts; };

parts = {

# =============================================================================
# Enable and configure XMonad and XMobar
xmonad-wm = let

  # == XMobar == #

  # Runtime dependencies
  xmobar-deps = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ xmobar raw-strings-qq ]))
    pkgs.bash
    pkgs.coreutils
    pkgs.acpi
    pkgs.alsa-utils
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
    pkgs.bash
    pkgs.coreutils
    pkgs.brightnessctl  # for machine brightness control
    pkgs.pulseaudio  # For 'pactl'
  ];

  my-xmonad = pkgs.writeScriptBin "xmonad" ''
    export PATH=${pkgs.lib.strings.makeBinPath xmonad-deps}''${PATH:+:}''${PATH:+$PATH}
    export XMONAD_XMESSAGE=${pkgs.coreutils}/bin/true  # disable xmonad error popup
    exec ${xmonad-hpkgs.xmonad}/bin/xmonad "$@"
  '';

in {

  services.xserver.enable = true;
  services.xserver.windowManager.session = [{
    name = "xmonad";
    start = ''
      systemd-cat -t xmonad -- ${my-xmonad}/bin/xmonad &
      waitPID=$!
    '';
  }];
  # services.displayManager.defaultSession = "none+xmonad";

  home-manager.users.${user} = {
    home.file.".xmonad/xmonad.hs".source = linked ./xmonad/xmonad.hs;
    # Modules in lib/ will be available to xmonad.hs
    home.file.".xmonad/lib".source = linked ./xmonad/lib;
    xdg.configFile."xmobar/xmobar.hs".source = linked ./xmobar/xmobar.hs;
  };

  environment.systemPackages = let
    src = toString ./.;
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
# Set the background image
background-image = {
  home-manager.users.${user}.home.file.".background-image".source = ./background.png;
  services.xserver.desktopManager.wallpaper.mode = "fill";
};

};

in folded
