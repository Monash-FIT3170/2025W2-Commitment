# This produces a function which wraps any given package in an extremely restrictive FHS container.
{
  pkgs,
  wrap-sh ? throw "Tried to import wrap.nix without required argument 'wrap-sh'.",
  ...
}:

{
  wrapArgs ? "",
  runScript ? "",
  targetPkgs ? x: [],
  ...
}@args:

let
  buildFHSEnvArgs = builtins.removeAttrs args ["wrapArgs"];
in
  pkgs.buildFHSEnv (buildFHSEnvArgs // {
    targetPkgs = pkgs: targetPkgs pkgs ++ (if runScript == "bash" then [pkgs.bash] else []);

    runScript = ''
      ${wrap-sh}/bin/wrap.sh ${wrapArgs} ${runScript} $@
    '';
  })
