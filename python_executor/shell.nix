{
  pkgs ? import <nixpkgs> { }
}:

let
  fhs = import ./default.nix { inherit pkgs; };
  x = throw fhs;
in
  pkgs.mkShell {
    buildInputs = [
      fhs
      pkgs.bash
    ];

    shellHook = ''
      echo "Running shell ${fhs}"
      exec ${fhs}/bin/python-executor-env
    '';
  }
