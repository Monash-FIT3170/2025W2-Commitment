{
  pkgs,
  ...
}:

pkgs.stdenv.mkDerivation {
  pname = "wrap-sh";
  version = "1.0";

  src = ./.;

  buildInputs = [ pkgs.bubblewrap pkgs.bash ];

  buildPhase = ''
    mkdir -p $out/bin
    cp wrap.sh $out/bin/wrap.sh
    chmod +x $out/bin/wrap.sh

    # Replace bwrap with path to bwrap in nix store
    sed -i "s|\\bbwrap\\b|${pkgs.bubblewrap}/bin/bwrap|g" $out/bin/wrap.sh
    patchShebangs $out
  '';
}
