{
  lib
, fhs ? throw "fhs not provided to the python executor"
, python3
, python3Packages
}:

let
  python-executor = ps: ps.callPackage ./python-executor.nix { };

  # This python3 contains all the packages we need to run the python executor
  executor-python = python3.withPackages(ps: with ps; [
    (python-executor ps)
  ]);
in
  executor-python