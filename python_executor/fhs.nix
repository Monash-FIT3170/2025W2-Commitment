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
    # runScript = "timeout --signal=SIGKILL 5m ";
  };
  fhsForDocker = pkgs.stdenv.mkDerivation {
    name = fhsEnvName + "-for-docker";

    src = fhs + "/bin";

    buildInputs = [ pkgs.bubblewrap pkgs.bash ];

    buildPhase = ''
      mkdir -p $out/bin
      cp ${fhsEnvName} $out/bin/${fhsEnvName}
      ln -s ${fhs}/bin/${fhsEnvName} $out/bin/original
      chmod a+rx $out/bin/${fhsEnvName}

      # Replace bwrap args with args safe for our docker environment

      # Replace all --* calls that have a --*-try variant
#      sed -i "s|\-\-dev\-bind\\b|\-\-dev\-bind\-try|g" $out/bin/${fhsEnvName}
#      sed -i "s|\-\-bind\\b|\-\-bind\-try|g" $out/bin/${fhsEnvName}
#      sed -i "s|\-\-ro\-bind\\b|\-\-ro\-bind\-try|g" $out/bin/${fhsEnvName}
      sed -i "s|\-try\-try|\-try|g" $out/bin/${fhsEnvName}

      # Remove binds to proc
      # sed -i '/KEYWORD/d' $out/bin/${fhsEnvName}

      # get rid of the remount-ro calls
      sed -i '/--proc/d' $out/bin/${fhsEnvName}

#      sed -i '/remount-ro/c\
#  --unshare-user-try \\\
#  --unshare-cgroup-try \\ ' $out/bin/${fhsEnvName}


      patchShebangs $out
    '';
  };
in
  fhs
