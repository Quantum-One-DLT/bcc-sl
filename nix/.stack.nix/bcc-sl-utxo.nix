{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-utxo"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2017-2021 TBCO";
      maintainer = "operations@bcccoin.io";
      author = "TBCO Engineering Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-sl/#readme";
      url = "";
      synopsis = "Abstract definitions of UTxO based accounting";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-binary)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-chain-test)
          (hsPkgs.bcc-sl-client)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-core-test)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.reflection)
          (hsPkgs.safecopy)
          (hsPkgs.serokell-util)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././utxo; }