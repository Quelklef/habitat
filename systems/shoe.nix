{ lib, config, pkgs, modulesPath, ... }@args: let

user = "sock";
host = "shoe";
perloc = builtins.toString /per;

common =
  import ../common.nix
    { inherit perloc user host; }
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
  fileSystems."/boot" = { device = "/dev/disk/by-uuid/9D4D-35FA"; fsType = "vfat"; };

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  networking.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.device = "/dev/vda";
  boot.kernelPackages = pkgs.zfs.latestCompatibleLinuxPackages;
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "cc00ffee";

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/eyd/root@blank
  '';
};

# =============================================================================
base2 = {
  networking.hostName = host;
  system.stateVersion = "22.05";
  time.timeZone = "America/Los_Angeles";
  touchpad.transformationMatrix = "3 0 0 0 3 0 0 0 1";
};

}; in folded
