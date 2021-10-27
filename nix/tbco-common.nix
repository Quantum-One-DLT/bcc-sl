# Imports the bcccoin-nix library.
# The version can be overridden for debugging purposes by setting
# NIX_PATH=tbco_nix=/path/to/bcccoin-nix
import (
  let try = builtins.tryEval <tbco_nix>;
  in if try.success
  then builtins.trace "using host <tbco_nix>" try.value
  else
    let
      spec = builtins.fromJSON (builtins.readFile ./bcccoin-nix-src.json);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    }) {}
