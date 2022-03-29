{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-db-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "TBCO <support@blockchain-company.io>";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - arbitrary instances for bcc-sl-db";
      description = "Bcc SL - arbitrary instances for bcc-sl-db";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bcc-sl-binary)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-chain-test)
          (hsPkgs.bcc-sl-core-test)
          (hsPkgs.bcc-sl-crypto-test)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-util-test)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././db/test; }