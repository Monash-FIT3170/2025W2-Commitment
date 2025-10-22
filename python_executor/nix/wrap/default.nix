# This produces a function which wraps any given package in an extremely restrictive FHS container.
# You can treat wrap exactly like pkgs.buildFHSEnv
# https://ryantm.github.io/nixpkgs/builders/special/fhs-environments/
{
  pkgs,
  lib,
  ...
}:

let
  wrap-sh = import ./wrap-sh.nix { inherit pkgs; };
  wrap = import ./wrap.nix { inherit pkgs wrap-sh lib; };
in
  wrap
