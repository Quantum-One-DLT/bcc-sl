{ ... }@args:

let
  commonLib = import ./bcccoin-common.nix;

in commonLib.nix-tools.default-nix ./pkgs.nix args
