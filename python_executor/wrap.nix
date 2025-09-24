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
  };

in
  {
    name,
    wrapArgs ? "",
    runScript ? "bash",
    targetPkgs ? x: [],
    ...
  }@args:

  let
    buildFHSEnvArgs = builtins.removeAttrs args ["wrapArgs"];
    runScriptWritten = pkgs.writeShellScript "${name}-runScript" ''
      echo "Entering wrap FHS Env Shell"
      echo ">>>" ${wrap-sh}/bin/wrap.sh ${wrapArgs}
      echo ""
      ${runScript} "$@"
    '';
  in
    pkgs.buildFHSEnv (buildFHSEnvArgs // {
      targetPkgs = pkgs: targetPkgs pkgs ++ [
        runScriptWritten
      ] ++ (if runScript == "bash" then [pkgs.bash] else []);

      runScript = ''
        ${wrap-sh}/bin/wrap.sh ${wrapArgs} ${runScript}
      '';
    })
