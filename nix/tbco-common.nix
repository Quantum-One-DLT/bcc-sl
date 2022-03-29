# Imports the tbco-nix library.
# The version can be overridden for debugging purposes by setting
# NIX_PATH=tbco_nix=/path/to/tbco-nix
import (
  let try = builtins.tryEval <tbco_nix>;
  in if try.success
  then builtins.trace "using host <tbco_nix>" try.value
  else
    let
      spec = builtins.fromJSON (builtins.readFile ./tbco-nix-src.json);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    }) {}
