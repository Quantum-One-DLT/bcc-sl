{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-crypto-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - arbitrary instances for bcc-sl-crypto";
      description = "This package contains arbitrary instances for the cryptography primitives used in Bcc SL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-crypto)
          (hsPkgs.bcc-sl-binary)
          (hsPkgs.bcc-sl-binary-test)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.bcc-sl-util-test)
          (hsPkgs.cryptonite)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.memory)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.universum)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././crypto/test; }