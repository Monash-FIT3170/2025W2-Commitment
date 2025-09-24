{
  buildFHSEnv,
  wrap ? throw "fhs.nix called without required argument 'wrap'.",
  ...
}:

let
  fhsEnvName = "python-executor-env";
  fhsEnv = buildFHSEnv {
    name = fhsEnvName;
    targetPkgs = pkgs: [
      pkgs.python311
      pkgs.python311Packages.numpy
      pkgs.bash
      pkgs.bubblewrap
    ];

#    runScript = ''
#      bwrap \
#        --ro-bind /scripts
#    '';

  };
in
  wrap {
    package = fhsEnv;
    executable = fhsEnvName;
    wrapArgs = "";
  }