{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-infra-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "TBCO <support@bcccoin.io>";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - generators for bcc-sl-infra";
      description = "This package contains generators for the infrastructural data types used in Bcc SL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-sl-binary-test)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-chain-test)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-core-test)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-crypto-test)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-networking)
          (hsPkgs.bcc-sl-util-test)
          (hsPkgs.containers)
          (hsPkgs.dns)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.iproute)
          (hsPkgs.universum)
          (hsPkgs.yaml)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././infra/test; }