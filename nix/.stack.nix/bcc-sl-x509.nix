{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-x509"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@bcccoin.io";
      author = "TBCO Engineering Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-sl/x509/README.md";
      url = "";
      synopsis = "Tool-suite for generating x509 certificates specialized for RSA with SHA-256";
      description = "See README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.asn1-encoding)
          (hsPkgs.asn1-types)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default-class)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.hourglass)
          (hsPkgs.ip)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          (hsPkgs.x509-validation)
          (hsPkgs.yaml)
          ];
        };
      tests = {
        "bcc-sl-x509-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.bcc-sl-x509)
            (hsPkgs.universum)
            (hsPkgs.hedgehog)
            (hsPkgs.bcc-sl-util-test)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././x509; }