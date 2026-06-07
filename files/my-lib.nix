{ pkgs, lib }: let
in rec {

  /*

  Takes a path and returns a derivation producing a symlink to that path

  Derived from github.com/nix-community/home-manager/blob/e622bad16372aa5ada79a7fa749ec78715dffc54/modules/files.nix#L64-L69

  Useful for:
  1. Linking program state such as ~/.config/google-chrome elsewhere
     Integral to the success of opt-in state!
  2. Linking scripts and program configuration, so that a change can
     be seen immediately without requiring a rebuild.
     Also see 'linked-script'

  */
  linked = path:
    with lib;
    pkgs.runCommandLocal
      (baseNameOf (toString path)) {}
      ''ln -s ${escapeShellArg (toString path)} $out'';

  /*

  Like 'linked', but:
   . Removes extension from script name (eg, my-script.sh -> my-script)
   . Writes script to $out/bin/$name instead of just $out

  */
  linked-script = path:
    let bin-name = lib.pipe path
      [ toString baseNameOf (lib.strings.splitString ".") lib.lists.init (builtins.concatStringsSep ".") ];
    in pkgs.writers.writeBashBin bin-name ''
      exec ${linked path} "$@"
    '';

}
