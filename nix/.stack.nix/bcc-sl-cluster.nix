{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-cluster"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@bcccoin.io";
      author = "TBCO Engineering Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-sl/cluster/README.md";
      url = "";
      synopsis = "Utilities to generate and run cluster of nodes";
      description = "See README";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-networking)
          (hsPkgs.bcc-sl-node)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.bcc-sl-x509)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.attoparsec)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.iproute)
          (hsPkgs.lens)
          (hsPkgs.optparse-applicative)
          (hsPkgs.megaparsec)
          (hsPkgs.safe)
          (hsPkgs.servant-client)
          (hsPkgs.temporary)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.tls)
          (hsPkgs.universum)
          ];
        };
      exes = {
        "bcc-sl-cluster-demo" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-cluster)
            (hsPkgs.bcc-sl-node)
            (hsPkgs.ansi-terminal)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.docopt)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.universum)
            ];
          };
        "bcc-sl-cluster-prepare-environment" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-cluster)
            (hsPkgs.containers)
            (hsPkgs.docopt)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.universum)
            ];
          };
        };
      tests = {
        "bcc-sl-cluster-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-cluster)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.lens)
            (hsPkgs.QuickCheck)
            (hsPkgs.time)
            (hsPkgs.universum)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cluster; }