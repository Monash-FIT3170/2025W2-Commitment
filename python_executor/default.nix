{
  pkgs ? import <nixpkgs> { }
, lib ? pkgs.lib
, ...
}:

let
  wrap = import ./nix/wrap { inherit pkgs lib; };
  fhs = pkgs.callPackage ./nix/fhs.nix { inherit pkgs wrap; };
  executor-python = pkgs.callPackage ./executor { inherit lib fhs; };
  python-executor = pkgs.callPackage ./executor/python-executor.nix { inherit lib; };
in
  python-executor