{
  wrap ? throw "fhs.nix called without required argument 'wrap'.",
  ...
}:

let
  fhsEnvName = "python-executor-env";
in
  wrap {
    name = fhsEnvName;
    wrapArgs = "";

    targetPkgs = pkgs: [
      pkgs.python311
      pkgs.python311Packages.numpy
      pkgs.bash

    ];
  }