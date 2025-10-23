{
  pkgs ? import <nixpkgs> { }
}:

let
  fhs = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [
      fhs
      pkgs.bash
    ];

    shellHook = ''
      echo "Running shell ${fhs}"
      echo ""
      exec ${fhs}/bin/python-executor-env bash
    '';
  }
