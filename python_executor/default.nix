{
  pkgs ? import <nixpkgs> { },
  ...
}:

let
  wrap = import ./wrap { inherit pkgs; };
  fhs = pkgs.callPackage ./fhs.nix { inherit pkgs wrap; };
in
  fhs