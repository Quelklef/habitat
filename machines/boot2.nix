#
# Boot2: Thinkpad P16s Gen4 AMD
#

{ lib, config, pkgs, modulesPath, ... }@args: let

user = "sock";
host = "boot2";
perloc = builtins.toString /per;

common =
  import ../common.nix
    { inherit perloc user host; }
    args;

nixos-hardware =
  builtins.fetchGit {
    url = "https://github.com/nixos/nixos-hardware";
    rev = "4ed851c979641e28597a05086332d75cdc9e395f";
  };

folded = { imports = lib.attrsets.attrValues parts; };

parts = {

common = common.folded;

# =============================================================================
# Low-level hardware & initialization
base = {

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (nixos-hardware + /lenovo/thinkpad/p16s/amd/gen4)
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;

  fileSystems."/" = { device = "rpool/eyd/root"; fsType = "zfs"; };
  fileSystems."/nix" = { device = "rpool/eyd/nix"; fsType = "zfs"; };
  fileSystems."/per" = { device = "rpool/eyd/per"; fsType = "zfs"; };
  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7C5C-1D61";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  swapDevices = [
    {
      device = "/dev/disk/by-partlabel/disk-main-swap";
      randomEncryption.enable = true;
    }
  ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.stateVersion = "25.11";

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  networking.hostName = host;
  networking.hostId = "f54b0ffb";  # required by ZFS to be set

};

# =============================================================================
# Enable fs reset on boot (EYD-style)
#
# This wipes the disk on boot, preserving only /nix, /boot, and /per;
# see <https://grahamc.com/blog/erase-your-darlings/>
#
# Reset is suppressed if the previous shutdown was unclean (so the
# service 'request-eyd-reset' did not run), or if an explicit opt-out
# was indicated by the creation of '/run/no-reset-on-next-boot'
eyd-reset = {
  systemd.services.request-eyd-reset = {
    description = "Before clean shutdown, opt-in to disk reset on next boot";
    wantedBy = [ "shutdown.target" ];
    before = [ "shutdown.target" ];
    unitConfig.DefaultDependencies = false;
    serviceConfig.Type = "oneshot";
    path = [ pkgs.zfs pkgs.coreutils ];
    script = ''
      if [ ! -e /run/no-reset-on-next-boot ]; then
        zfs set org.maynard.eyd:should-reset=1 rpool/eyd/root
      fi
    '';
  };
  boot.initrd.systemd.enable = true;
  boot.initrd.systemd.services.eyd-reset = {
    description = "Rollback ZFS root to blank snapshot";
    wantedBy = [ "initrd.target" ];
    after = [ "zfs-import-rpool.service" ];
    before = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = false;
    serviceConfig.Type = "oneshot";
    path = [ pkgs.zfs pkgs.coreutils ];
    script = ''
      should_reset=$( zfs get -H -o value org.maynard.eyd:should-reset rpool/eyd/root )
      echo "Should reset: $should_reset"
      if [ "$should_reset" = 1 ]; then
        zfs rollback -r rpool/eyd/root@blank
        zfs inherit org.maynard.eyd:should-reset rpool/eyd/root  # unset the flag
      fi
    '';
  };
};

# =============================================================================
# Set the time zone
time-zone = {
  time.timeZone = "America/Los_Angeles";
};

# =============================================================================
# Modify touchpad speeds
touchpad-sensitivity = {
  services.libinput.touchpad = {
    transformationMatrix = "2 0 0 0 2 0 0 0 1";
  };
};

}; in folded




