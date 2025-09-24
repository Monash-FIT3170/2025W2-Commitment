# This produces a function which wraps any given package in an extremely restrictive FHS container.

{
  pkgs,
  ...
}:

let
  nixwrapSrc = pkgs.fetchFromGitHub {
    owner = "rti";
    repo = "nixwrap";
    rev = "402d0ca7260cd66ab8d9772bfb902cc4c2b9cf7d";
    hash = "sha256-gYUf/sqHFrBVf2VeagOBc8vWU8asxFX/XDMpLH/hJDc=";
  };

  wrap-sh  = pkgs.stdenv.mkDerivation {
    pname = "wrap-sh";
    version = "1.0";

    src = nixwrapSrc;

    buildInputs = [ pkgs.bubblewrap pkgs.bash ];

    buildPhase = ''
      mkdir -p $out/bin
      cp wrap.sh $out/bin/wrap.sh
      chmod +x $out/bin/wrap.sh

      # Replace bwrap with path to bwrap in nix store
      sed -i "s|\\bbwrap\\b|${pkgs.bubblewrap}/bin/bwrap|g" $out/bin/wrap.sh
      patchShebangs $out
    '';
  };

  wrap =
    {
      package,
      wrapArgs ? "",
      executable ? package.pname,
    }:

    pkgs.symlinkJoin {
      name = package.name;
      paths = [ package ];
      postBuild = ''
        mv $out/bin/${executable}{,-nowrap}
        cat << _EOF > $out/bin/${executable}
          exec ${wrap-sh}/bin/wrap.sh ${wrapArgs} ${package}/bin/${executable} "\$@"
        _EOF
        chmod a+x $out/bin/${executable}
      '';
    };
in
  wrap
