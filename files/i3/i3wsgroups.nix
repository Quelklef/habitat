{ pkgs ? import <nixpkgs> {} }: let

# Installs i3-workspace-groups
# https://github.com/infokiller/i3-workspace-groups

python =
  pkgs.python39.withPackages (pkgs: with pkgs; [ i3ipc xlib six toml ]);

in pkgs.stdenv.mkDerivation {
  name = "i3-workspace-groups";

  buildInputs = [ python ];

  src = pkgs.fetchFromGitHub
          { owner = "infokiller";
            repo = "i3-workspace-groups";
            rev = "4dd06579df2666840c8de62a60d31467277194ef";
            sha256 = "0mh96ssd1y6fif482v4sfyfpb1w4dgq1rmpgrb08lc4s7qy5i45i";
          };

  installPhase = ''
    mkdir -p $out/bin
    for fname in $(ls $src/scripts); do

      {

        cat << EOF
export PYTHONPATH=$src
export PATH=\$PATH:${pkgs.rofi}/bin
export PATH=\$PATH:${pkgs.unixtools.column}/bin
EOF

        local targ=$src/scripts/$fname
        if cat $targ | head -n1 | grep -q python; then
          echo "${python}/bin/python $targ"' "$@"'
        else
          echo "bash <(cat $targ)"' "$@"'
          # don't ask my why 'bash <(cat)' is needed. I don't know.
        fi

      } > $out/bin/$fname
      chmod +x $out/bin/$fname

    done
  '';
}
