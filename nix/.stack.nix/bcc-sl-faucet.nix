{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-faucet"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "ben.ford@tweag.io";
      author = "Ben Ford";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Bcc SL - faucet";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-sl-client)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.bcc-sl-mnemonic)
          (hsPkgs.bcc-wallet)
          (hsPkgs.connection)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.ekg-core)
          (hsPkgs.ekg-statsd)
          (hsPkgs.filepath)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.random)
          (hsPkgs.safe-exceptions)
          (hsPkgs.servant)
          (hsPkgs.servant-client-core)
          (hsPkgs.servant-server)
          (hsPkgs.servant-swagger)
          (hsPkgs.servant-swagger-ui)
          (hsPkgs.stm)
          (hsPkgs.swagger2)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.tls)
          (hsPkgs.universum)
          (hsPkgs.wai)
          (hsPkgs.wai-app-static)
          (hsPkgs.wreq)
          ];
        };
      exes = {
        "bcc-faucet" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl-faucet)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.ekg)
            (hsPkgs.ekg-statsd)
            (hsPkgs.lens)
            (hsPkgs.log-warper)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.servant-server)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.warp)
            ];
          };
        };
      tests = {
        "faucet-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl-faucet)
            (hsPkgs.hspec)
            (hsPkgs.time)
            (hsPkgs.universum)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././faucet; }