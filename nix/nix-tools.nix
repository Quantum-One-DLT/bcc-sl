{ ... }@args:

let
  commonLib = import ./tbco-common.nix;

in commonLib.nix-tools.default-nix ./pkgs.nix args
