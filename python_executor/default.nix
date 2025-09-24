{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
  ...
}:

let
  wrap = import ./wrap { inherit pkgs lib; };
  fhs = pkgs.callPackage ./fhs.nix { inherit pkgs wrap; };
in
  fhs