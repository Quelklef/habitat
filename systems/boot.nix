{ lib, config, pkgs, modulesPath, ... }@args: let

user = "sock";
host = "boot";
perloc = builtins.toString /per;

common =
  import ../common.nix
    { inherit perloc user host; }
    args;

inherit (common.mylib) linked;

nixos-hardware =
  builtins.fetchGit {
    url = "https://github.com/nixos/nixos-hardware";
    rev = "0e6593630071440eb89cd97a52921497482b22c6";
  };

folded = { imports = lib.attrsets.attrValues parts; };

parts = {

common = common.folded;

# =============================================================================
# Low-level hardware & initialization
base = {

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (nixos-hardware + /lenovo/thinkpad/p16s/amd/gen1)
    # (nixos-hardware + /lenovo/thinkpad/p14s/amd/gen2)
    # (nixos-hardware + /lenovo/thinkpad/t14/amd/gen1)
    # (nixos-hardware + /lenovo/thinkpad/t14/amd/gen2)
    # (nixos-hardware + /lenovo/thinkpad/t14/amd/gen3)
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = { device = "rpool/eyd/root"; fsType = "zfs"; };
  fileSystems."/nix" = { device = "rpool/eyd/nix"; fsType = "zfs"; };
  fileSystems."/per" = { device = "rpool/eyd/per"; fsType = "zfs"; };
  fileSystems."/boot" = { device = "/dev/disk/by-uuid/1215-6AB1"; fsType = "vfat"; };

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.supportedFilesystems = [ "zfs" ];
  #boot.loader.grub.device = "/dev/nvme0n1";
  boot.kernelPackages = pkgs.zfs.latestCompatibleLinuxPackages;

  networking.hostName = host;
  networking.hostId = "cc00ffee";  # required by ZFS

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/eyd/root@blank
  '';

  system.stateVersion = "23.05";
};

# =============================================================================
# Higher-level stuff
base2 = {

  # Set time zone
  time.timeZone = "America/Los_Angeles";

  # The screen is 4k (3840x2400), but that makes things tiny
  # For now, take the easy way out: just use it as 1920x1200
  # This makes stuff blurry; better would be to configure the system for hidpi
  services.xserver.virtualScreen = { x = 1920; y = 1200; };

  /*
  services.xserver.dpi = 180;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  # Set Xft.dpi and increase cursor size
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      Xft.dpi: 180
      Xcursor.theme: Adwaita
      Xcursor.size: 48
    EOF
  '';
  */

  # Modify touchpad speeds
  services.libinput.touchpad = {
    transformationMatrix = "2 0 0 0 2 0 0 0 1";
    # accelProfile = "flat";  # Disable acceleration
  };

};

}; in folded
