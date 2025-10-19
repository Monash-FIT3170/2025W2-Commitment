{
  pkgs ? import <nixpkgs> { }
, lib ? pkgs.lib
, ...
}:

let
  wrap = import ./nix/wrap { inherit pkgs lib; };
  fhs = pkgs.callPackage ./nix/fhs.nix { inherit pkgs wrap; };
  python-executor = pkgs.callPackage ./executor { inherit lib fhs; };
in
  python-executor