{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-node"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company";
      maintainer = "Serokell <hi@serokell.io>";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Bcc SL simple node executable";
      description = "Provides a 'bcc-node-simple' executable which can\nconnect to the Bcc network and act as a full node\nbut does not have any wallet capabilities.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-networking)
          (hsPkgs.bcc-sl-node-ipc)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.bcc-sl-x509)
          (hsPkgs.connection)
          (hsPkgs.data-default)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-media)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.serokell-util)
          (hsPkgs.servant-client)
          (hsPkgs.servant-server)
          (hsPkgs.servant-swagger)
          (hsPkgs.servant-swagger-ui)
          (hsPkgs.stm)
          (hsPkgs.swagger2)
          (hsPkgs.text)
          (hsPkgs.time-units)
          (hsPkgs.tls)
          (hsPkgs.universum)
          (hsPkgs.wai)
          (hsPkgs.warp)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          ];
        };
      exes = {
        "bcc-node-simple" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.bcc-sl-node)
            (hsPkgs.bcc-sl)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      tests = {
        "property-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.HUnit)
            (hsPkgs.QuickCheck)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-utxo)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.hashable)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.validation)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././node; }