{ pkgs ? import <nixpkgs> {}
, tbco-extras ? {}
, tbco-module ? {}
, haskell
, ...
}:
let

  # our packages
  stack-pkgs = import ./.stack.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras haskell.hackage).compiler;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-extras = [
      tbco-extras.${compiler.nix-name}
    ];
    modules = [
      # the tbco-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      tbco-module
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        nonReinstallablePkgs =
            [ "rts" "ghc" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "array" "deepseq" "pretty" "ghc-boot-th" "template-haskell" "ghc-heap" ];
        doHaddock = false;
        doExactConfig = true;
      }
      {
        packages.bcc-sl.patches = [ ./patches/bcc-sl.patch ];
        packages.libiserv.patches = [ ./patches/libiserv-network-3.patch ];
        packages.ekg-wai.components.library.enableSeparateDataOutput = true;
      }
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
