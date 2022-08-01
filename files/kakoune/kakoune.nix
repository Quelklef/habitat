{ pkgs ? import <nixpkgs> {}
, linked ? false  # if true, symlink config file
}:

pkgs.stdenv.mkDerivation {
  name = "configured-kak";
  dontUnpack = true;

  installPhase = ''

    mkdir -p $out

    mkdir -p $out/xch/kak
    if [ -n ${toString linked} ]; then
      ln -s ${toString ./kakrc} $out/xch/kak/kakrc
    else
      cp ${toString ./kakrc} $out/xch/kak/kakrc
    fi

    mkdir -p $out/bin
    touch $out/bin/kak
    chmod +x $out/bin/kak

    cat <<EOF > $out/bin/kak

    export XDG_CONFIG_HOME=$(realpath $out/xch)
    export PATH=\''${PATH}\''${PATH+:}${pkgs.xsel}/bin/xsel
    ${pkgs.kakoune}/bin/kak "\$@"

    EOF
  '';
}
