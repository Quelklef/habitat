{ lib, config, pkgs, modulesPath, ... }@args: let

user = "lark";
host = "lake";
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

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/eyd/root@blank
  '';
};

# =============================================================================
base = {
  networking.hostName = "lake";
  system.stateVersion = "22.05";
  time.timeZone = "America/Los_Angeles";
  services.libinput.touchpad.transformationMatrix = "2 0 0 0 2 0 0 0 1";
};

# =============================================================================
screen-stuff = {
  home-manager.users.${user}.programs.bash.bashrcExtra = ''
    # Run once system has booted
    function booted {
      # Sets displays
      xrandr --output HDMI-2 --auto \
             --output HDMI-1 --off
      exit 0
    }
  '';
  environment.sessionVariables = {
    # Fucking fuck. Works around alacritty's DPI-sensitive font scaling
    "WINIT_X11_SCALE_FACTOR" = "2";
  };
};

}; in folded
