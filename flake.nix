# Lightweight environment
# For use eg. when SSH'd into machines

{
  inputs = {
    pkgs.url = "github:nixos/nixpkgs/22.05";
  };
  outputs = inputs: let
    system = "x86_64-linux";
    pkgs = inputs.pkgs.legacyPackages.${system};
  in {
    devShells.${system}.kak = pkgs.mkShell {
      buildInputs = [
        (import ./files/kakoune/kakoune.nix { inherit pkgs; })
      ];
    };
  };
}
