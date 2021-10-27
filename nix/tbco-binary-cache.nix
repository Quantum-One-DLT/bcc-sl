# This is a sample NixOS configuration file which you can import into
# your own configuration.nix in order to enable the TBCO binary cache.

{ config, lib, pkgs, ... }:

{
  nix = {
    binaryCachePublicKeys = [ "hydra.bcccoin.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    binaryCaches = [ "https://hydra.bcccoin.io" ];
  };
}
