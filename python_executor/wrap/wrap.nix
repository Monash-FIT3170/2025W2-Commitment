# This produces a function which wraps any given package in an extremely restrictive FHS container.
{
  pkgs,
  wrap-sh ? throw "Tried to import wrap.nix without required argument 'wrap-sh'.",
  lib,
  ...
}:

{
  name,
  wrapArgs ? "",
  runScript ? "",
  targetPkgs ? x: [],
  ...
}@args:

let
  buildFHSEnvArgs = builtins.removeAttrs args ["wrapArgs"];

  # Compute all store paths needed by the target packages
  pkgsToBind = targetPkgs pkgs;
  closureStorePathsFile = (pkgs.closureInfo { rootPaths = pkgsToBind; }) + "/store-paths";
  closureStorePaths = builtins.readFile closureStorePathsFile;

  storePathsFile = pkgs.writeText "${name}-storePathsFile" (
    # Combine derivation paths with root paths for the final store paths file
    builtins.concatStringsSep "\n" (pkgsToBind ++ [closureStorePaths])
  );
in
  pkgs.buildFHSEnv (buildFHSEnvArgs // {
    targetPkgs = pkgs: targetPkgs pkgs ++ (if runScript == "bash" then [pkgs.bash] else []);

    runScript = ''
      ${wrap-sh}/bin/wrap.sh --store-paths ${storePathsFile} ${wrapArgs} ${runScript} $@
    '';
  })
