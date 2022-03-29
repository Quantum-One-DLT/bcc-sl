{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-chain-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "rupert.horlick@blockchain-company.io";
      author = "Rupert Horlick";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - arbitrary instances for bcc-sl-chain";
      description = "Bcc SL - arbitrary instances for bcc-sl-chain";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-crypto)
          (hsPkgs.bcc-sl-binary-test)
          (hsPkgs.bcc-sl-binary)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-core-test)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-crypto-test)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.bcc-sl-util-test)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.pvss)
          (hsPkgs.QuickCheck)
          (hsPkgs.random)
          (hsPkgs.reflection)
          (hsPkgs.serokell-util)
          (hsPkgs.time-units)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././chain/test; }