# This produces a function which wraps any given package in an extremely restrictive FHS container.
{
  pkgs,
  ...
}:

let
  wrap-sh = import ./wrap-sh.nix { inherit pkgs; };
  wrap = import ./wrap.nix { inherit pkgs wrap-sh; };
in
  wrap
