{
  pkgs,
  wrap ? throw "fhs.nix called without required argument 'wrap'.",
  ...
}:

let
  fhsEnvName = "python-executor-env";

  fhs = wrap {
    name = fhsEnvName;
    wrapArgs = "";

    targetPkgs = pkgs: [
      pkgs.python311
      pkgs.python311Packages.numpy
      pkgs.bash
      pkgs.coreutils  #< For timeout
      pkgs.vim
    ];

    runScript = "";
  };
in
  fhs
