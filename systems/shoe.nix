{ lib, config, pkgs, modulesPath, ... }@args: let

user = "sock";
host = "shoe";

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
base = {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (builtins.fetchGit {
      url = "https://github.com/nixos/nixos-hardware";
      rev = "0e6593630071440eb89cd97a52921497482b22c6";
    } + /lenovo/thinkpad/t14/amd/gen2)
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = { device = "rpool/eyd/root"; fsType = "zfs"; };
  fileSystems."/nix" = { device = "rpool/eyd/nix"; fsType = "zfs"; };
  fileSystems."/per" = { device = "rpool/eyd/per"; fsType = "zfs"; };
  fileSystems."/boot" = { device = "/dev/disk/by-uuid/DBF4-C77A"; fsType = "vfat"; };

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  networking.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.device = "/dev/vda";
  boot.kernelPackages = pkgs.zfs.latestCompatibleLinuxPackages;
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "cc00ffee";

  # WANT: ability to disable eyd for single reboot
  #       wasn't able to figure out a good way to do this tho
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/eyd/root@blank
  '';
};

# =============================================================================
base2 = {
  networking.hostName = host;
  system.stateVersion = "22.05";
  time.timeZone = "America/Los_Angeles";

  environment.etc."nixos/configuration.nix".text = "import /per/config/systems/${host}.nix";
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

}; in folded
