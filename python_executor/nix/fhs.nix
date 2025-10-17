{
  pkgs,
  wrap ? throw "fhs.nix called without required argument 'wrap'.",
  ...
}:

let
  fhsEnvName = "python-executor-env";

  runScript = pkgs.writeShellScriptBin "${fhsEnvName}-runScript" ''
    #!/usr/bin/env bash

    # Limit to 10 processes
    ulimit -u 10

    # Limit to 1 seconds of CPU time
    ulimit -t 1

    # Limit max memory to 200 MB
    ulimit -v $((200*1024))  # in KB

    # Limit max number of open files
    ulimit -n 100

    exec nice $@
  '';

  fhs = wrap {
    name = fhsEnvName;
    wrapArgs = "";

    targetPkgs = pkgs: [
      pkgs.python311
      pkgs.python311Packages.numpy
      pkgs.bash
      pkgs.coreutils  #< For timeout
      pkgs.vim

      runScript
    ];

#    runScript = runScript + "/bin/${fhsEnvName}-runScript";
  };
in
  fhs
