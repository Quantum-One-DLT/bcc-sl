{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-script-runner"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@blockchain-company.io";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - Script Runner";
      description = "Bcc SL - ScriptRunner";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.brick)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-client)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-networking)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.dns)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.lifted-async)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.process)
          (hsPkgs.resourcet)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.turtle)
          (hsPkgs.universum)
          (hsPkgs.unix)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.vty)
          (hsPkgs.yaml)
          ];
        };
      exes = {
        "testcases" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-binary)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-db)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-script-runner)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.cborg)
            (hsPkgs.constraints)
            (hsPkgs.data-default)
            (hsPkgs.formatting)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.turtle)
            (hsPkgs.universum)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././script-runner; }