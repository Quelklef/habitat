{ lib, config, pkgs, modulesPath, ... }@args: let

user = "lark";

common =
  import ../common.nix
  { stateloc = builtins.toString /per/state;
    secrets = (import /per/secrets.nix).nixos;
    inherit user;
  }
  args;

inherit (common.mylib) linked;

folded = { imports = lib.attrsets.attrValues parts; };

parts = {

common = common.folded;

# =============================================================================
# either mostly- or fully- extracted from nixos-generate-config
hardware = {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_usb_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = { fsType = "zfs"; device = "rpool/eyd/root"; };
  fileSystems."/nix" = { fsType = "zfs"; device = "rpool/eyd/nix"; };
  fileSystems."/per" = { fsType = "zfs"; device = "rpool/eyd/per"; };
  fileSystems."/boot" = { fsType = "vfat"; device = "/dev/disk/by-uuid/FBA2-298E"; };

  swapDevices = [
    { device = "/dev/disk/by-uuid/d5ea672c-51ca-43d4-9c5a-4b104b25b598"; }
  ];

  networking.useDHCP = lib.mkDefault true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
};

# =============================================================================
boot = {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.device = "/dev/nvme0n1";
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.devNodes = "/dev/disk/by-path";
  networking.hostId = "00c06c06";  # required by zfs

  # WANT: ability to disable eyd for single reboot
  #       wasn't able to figure out a good way to do this tho
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/eyd/root@blank
  '';
};

# =============================================================================
base = {
  networking.hostName = "lake";
  system.stateVersion = "22.05";
  time.timeZone = "America/Los_Angeles";

  environment.etc."nixos/configuration.nix".text = "import /per/config/systems/lake.nix";
  # ^ nb
  # This causes the warning "something's wrong at /nix/store/XXX...XXX.pl line 120."
  # I don't think it's an issue.
  # If anything does go wrong, can always manually '-I nixos-config='

  environment.interactiveShellInit = ''
    export NIX_PATH="$NIX_PATH:secrets=/per/secrets.nix"
  '';

  # Disable ipv6; it's messing with npm n stuff
  boot.kernel.sysctl."net.ipv6.conf.eth0.disable_ipv6" = true;
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

# =============================================================================
screen-init = {
  home-manager.users.${user} = {
    xsession.enable = true;
    xsession.initExtra = ''
      # Hacky! I don't think xsession is the right place for this.
      sleep 1 && xrandr --output HDMI-2 --auto --primary --output HDMI-1 --auto --rotate right --below HDMI-2
    '';
  };
};

# =============================================================================
redshift = {
  services.redshift.enable = true;
  location.latitude = 37.8715;
  location.longitude = -122.2730;
};

}; in folded
