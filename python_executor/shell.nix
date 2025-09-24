{
  pkgs ? import <nixpkgs> { }
}:

let
  fhs = import ./default.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = [
      fhs
    ];

    shellHook = ''
      echo "Running shell ${fhs}"
      exec ${fhs}/bin/python-executor-env
    '';
  }
