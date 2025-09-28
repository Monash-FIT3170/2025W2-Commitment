# This produces a function which wraps any given package in an extremely restrictive FHS container.
# You can treat wrap exactly like pkgs.buildFHSEnv
# https://ryantm.github.io/nixpkgs/builders/special/fhs-environments/
{
  nixpkgs ? <nixpkgs>,
  pkgs,
  wrap-sh ? throw "Tried to import wrap.nix without required argument 'wrap-sh'.",
  lib ? pkgs.lib,
  pkgsi686Linux ? pkgs.pkgsi686Linux,
  stdenv ? pkgs.stdenv,
  ...
}:

{
  name,
  wrapArgs ? "",
  runScript ? "",

  targetPkgs ? pkgs: [ ],
  multiPkgs ? pkgs: [ ],
  profile ? "",
  nativeBuildInputs ? [ ],
  extraBuildCommands ? "",
  extraBuildCommandsMulti ? "",
  extraOutputsToInstall ? [ ],

  multiArch ? false, # Whether to include 32bit packages
  includeClosures ? false, # Whether to include closures of all packages

  ...
}@args:

let
  buildFHSEnvArgs = builtins.removeAttrs args ["wrapArgs"];

  # "use of glibc_multi is only supported on x86_64-linux"
  isMultiBuild = multiArch && stdenv.system == "x86_64-linux";

  # list of packages (usually programs) which match the host's architecture
  # (which includes stuff from multiPkgs)
  targetPaths = targetPkgs pkgs ++ (if multiPkgs == null then [ ] else multiPkgs pkgs);

  # list of packages which are for x86 (only multiPkgs, only for x86_64 hosts)
  multiPaths = multiPkgs pkgsi686Linux;

  # base packages of the fhsenv
  # these match the host's architecture, glibc_multi is used for multilib
  # builds. glibcLocales must be before glibc or glibc_multi as otherwiese
  # the wrong LOCALE_ARCHIVE will be used where only C.UTF-8 is available.
  baseTargetPaths = with pkgs; [
    glibcLocales
    (if isMultiBuild then glibc_multi else glibc)
    gcc.cc.lib
    bashInteractiveFHS
    coreutils
    less
    shadow
    su
    gawk
    diffutils
    findutils
    gnused
    gnugrep
    gnutar
    gzip
    bzip2
    xz
  ];
  baseMultiPaths = with pkgsi686Linux; [
    gcc.cc.lib
  ];

  ldconfig = pkgs.writeShellScriptBin "ldconfig" ''
    # due to a glibc bug, 64-bit ldconfig complains about patchelf'd 32-bit libraries, so we use 32-bit ldconfig when we have them
    exec ${
      if isMultiBuild then pkgsi686Linux.glibc.bin else pkgs.glibc.bin
    }/bin/ldconfig -f /etc/ld.so.conf -C /etc/ld.so.cache "$@"
  '';

  etcProfile = pkgs.writeTextFile {
    name = "${name}-wrap-profile";
    destination = "/etc/profile";
    text = ''
      export PS1='${name}-wrap:\u@\h:\w\$ '
      export LOCALE_ARCHIVE="''${LOCALE_ARCHIVE:-/usr/lib/locale/locale-archive}"
      export PATH="/run/wrappers/bin:/usr/bin:/usr/sbin:$PATH"
      export TZDIR='/etc/zoneinfo'

      # XDG_DATA_DIRS is used by pressure-vessel (steam proton) and vulkan loaders to find the corresponding icd
      export XDG_DATA_DIRS=$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}/run/opengl-driver/share:/run/opengl-driver-32/share

      # Following XDG spec [1], XDG_DATA_DIRS should default to "/usr/local/share:/usr/share".
      # In nix, it is commonly set without containing these values, so we add them as fallback.
      #
      # [1] <https://specifications.freedesktop.org/basedir-spec/latest>
      case ":$XDG_DATA_DIRS:" in
        *:/usr/local/share:*) ;;
        *) export XDG_DATA_DIRS="$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}/usr/local/share" ;;
      esac
      case ":$XDG_DATA_DIRS:" in
        *:/usr/share:*) ;;
        *) export XDG_DATA_DIRS="$XDG_DATA_DIRS''${XDG_DATA_DIRS:+:}/usr/share" ;;
      esac

      # Force compilers and other tools to look in default search paths
      unset NIX_ENFORCE_PURITY
      export NIX_BINTOOLS_WRAPPER_TARGET_HOST_${stdenv.cc.suffixSalt}=1
      export NIX_CC_WRAPPER_TARGET_HOST_${stdenv.cc.suffixSalt}=1
      export NIX_CFLAGS_COMPILE='-idirafter /usr/include'
      export NIX_CFLAGS_LINK='-L/usr/lib -L/usr/lib32'
      export NIX_LDFLAGS='-L/usr/lib -L/usr/lib32'
      export PKG_CONFIG_PATH=/usr/lib/pkgconfig
      export ACLOCAL_PATH=/usr/share/aclocal

      # GStreamer searches for plugins relative to its real binary's location
      # https://gitlab.freedesktop.org/gstreamer/gstreamer/-/commit/bd97973ce0f2c5495bcda5cccd4f7ef7dcb7febc
      export GST_PLUGIN_SYSTEM_PATH_1_0=/usr/lib/gstreamer-1.0:/usr/lib32/gstreamer-1.0

      ${profile}
    '';
  };

  ensureGsettingsSchemasIsDirectory =
    pkgs.runCommandLocal "wrap-ensure-gsettings-schemas-directory" { }
      ''
        mkdir -p $out/share/glib-2.0/schemas
        touch $out/share/glib-2.0/schemas/.keep
      '';

  pickOutputs =
    let
      pickOutputsOne =
        outputs: drv:
        let
          isSpecifiedOutput = drv.outputSpecified or false;
          outputsToInstall = drv.meta.outputsToInstall or null;
          pickedOutputs =
            if isSpecifiedOutput || outputsToInstall == null then
              [ drv ]
            else
              map (out: drv.${out} or null) (outputsToInstall ++ outputs);
          extraOutputs = map (out: drv.${out} or null) extraOutputsToInstall;
          cleanOutputs = lib.filter (o: o != null) (pickedOutputs ++ extraOutputs);
        in
        {
          paths = cleanOutputs;
          priority = drv.meta.priority or lib.meta.defaultPriority;
        };
    in
    paths: outputs: map (pickOutputsOne outputs) paths;

  # Usually only used to calculate 'paths'
  basePaths = [
      etcProfile
      # ldconfig wrapper must come first so it overrides the original ldconfig
      ldconfig
      # magic package that just creates a directory, to ensure that
      # the entire directory can't be a symlink, as we will write
      # compiled schemas to it
      ensureGsettingsSchemasIsDirectory
    ]
    ++ baseTargetPaths
    ++ targetPaths;

  paths =
    pickOutputs basePaths [
      "out"
      "lib"
      "bin"
    ];

  # Usually only used to calculate 'paths32'
  basePaths32 = baseMultiPaths ++ multiPaths;

  paths32 = lib.optionals isMultiBuild (
    pickOutputs basePaths32 [
      "out"
      "lib"
    ]
  );

  allPaths = paths ++ paths32;

  rootfs-builder = pkgs.buildPackages.rustPlatform.buildRustPackage {
    name = "fhs-rootfs-bulder";
    src = nixpkgs + "/pkgs/build-support/build-fhsenv-bubblewrap/rootfs-builder";
    cargoLock.lockFile = nixpkgs + "/pkgs/build-support/build-fhsenv-bubblewrap/rootfs-builder/Cargo.lock";
    doCheck = false;
  };

  # Compute all store paths needed by the target packages
#  pkgsToBind = targetPkgs pkgs;
  pkgsToBind = basePaths ++ (lib.optionals isMultiBuild basePaths32);
  closureStorePathsFile = (pkgs.closureInfo { rootPaths = pkgsToBind; }) + "/store-paths";
  closureStorePaths = builtins.readFile closureStorePathsFile;

  storePathsFile = pkgs.writeText "${name}-storePathsFile" (
    # Combine derivation paths with root paths for the final store paths file
    builtins.concatStringsSep "\n" (pkgsToBind ++ [closureStorePaths])
  );

  rootfs =
    pkgs.runCommand "${name}-fhsenv-rootfs"
      {
        __structuredAttrs = true;
        exportReferencesGraph.graph = lib.concatMap (p: p.paths) allPaths;
        inherit
          paths
          paths32
          isMultiBuild
          includeClosures
          ;
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        ${rootfs-builder}/bin/rootfs-builder

        # create a bunch of symlinks for usrmerge
        ln -s /usr/bin $out/bin
        ln -s /usr/sbin $out/sbin
        ln -s /usr/lib $out/lib
        ln -s /usr/lib32 $out/lib32
        ln -s /usr/lib64 $out/lib64
        ln -s /usr/lib64 $out/usr/lib
        ln -s /usr/libexec $out/libexec

        # symlink 32-bit ld-linux so it's visible in /lib
        if [ -e $out/usr/lib32/ld-linux.so.2 ]; then
          ln -s /usr/lib32/ld-linux.so.2 $out/usr/lib64/ld-linux.so.2
        fi

        # symlink /etc/mtab -> /proc/mounts (compat for old userspace progs)
        ln -s /proc/mounts $out/etc/mtab

        if [[ -d $out/usr/share/gsettings-schemas/ ]]; then
          for d in $out/usr/share/gsettings-schemas/*; do
            # Force symlink, in case there are duplicates
            ln -fsr $d/glib-2.0/schemas/*.xml $out/usr/share/glib-2.0/schemas
            ln -fsr $d/glib-2.0/schemas/*.gschema.override $out/usr/share/glib-2.0/schemas
          done
          ${pkgs.pkgsBuildBuild.glib.dev}/bin/glib-compile-schemas $out/usr/share/glib-2.0/schemas
        fi

        ${extraBuildCommands}
        ${lib.optionalString isMultiBuild extraBuildCommandsMulti}
      '';
in
  pkgs.writeShellScriptBin "${name}" ''
    ${wrap-sh}/bin/wrap.sh --root-fs ${rootfs} --store-paths ${storePathsFile} ${wrapArgs} ${runScript} $@
  ''
