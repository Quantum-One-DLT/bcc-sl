{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-mnemonic"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@blockchain-company.io";
      author = "TBCO Engineering Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-sl/mnemonic/README.md";
      url = "";
      synopsis = "TODO";
      description = "See README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.basement)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-crypto)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.swagger2)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.universum)
          ];
        };
      exes = {
        "bcc-generate-mnemonic" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bcc-sl-mnemonic)
            (hsPkgs.bytestring)
            (hsPkgs.text)
            (hsPkgs.universum)
            ];
          };
        };
      tests = {
        "bcc-sl-mnemonic-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hspec)
            (hsPkgs.universum)
            (hsPkgs.bcc-sl-mnemonic)
            (hsPkgs.bytestring)
            (hsPkgs.QuickCheck)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.data-default)
            (hsPkgs.aeson)
            (hsPkgs.bcc-crypto)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././mnemonic; }