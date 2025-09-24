{
  pkgs ? import <nixpkgs> { },
  ...
}:

let
  wrap = import ./wrap.nix { inherit pkgs; };
  fhs = pkgs.callPackage ./fhs.nix { inherit wrap; };
in
  fhs