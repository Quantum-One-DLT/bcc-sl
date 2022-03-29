{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-explorer"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "TBCO <support@blockchain-company.io>";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Bcc explorer";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.free)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.resourcet)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-binary)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-chain-test)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-crypto-test)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-generator)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.mtl)
          (hsPkgs.servant)
          (hsPkgs.servant-server)
          (hsPkgs.http-types)
          (hsPkgs.socket-io)
          (hsPkgs.engine-io)
          (hsPkgs.engine-io-wai)
          (hsPkgs.wai)
          (hsPkgs.wai-extra)
          (hsPkgs.wai-cors)
          (hsPkgs.warp)
          ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
          ];
        };
      exes = {
        "bcc-explorer" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        "bcc-explorer-hs2purs" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.purescript-bridge)
            (hsPkgs.universum)
            (hsPkgs.optparse-simple)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        "bcc-explorer-swagger" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.servant-multipart)
            (hsPkgs.servant-server)
            (hsPkgs.servant-swagger)
            (hsPkgs.swagger2)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        "bcc-explorer-mock" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      tests = {
        "bcc-explorer-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-binary-test)
            (hsPkgs.bcc-sl-chain-test)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-core-test)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.engine-io)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.universum)
            (hsPkgs.warp)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      benchmarks = {
        "bcc-explorer-bench" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-chain-test)
            (hsPkgs.bcc-sl-explorer)
            (hsPkgs.criterion)
            (hsPkgs.universum)
            (hsPkgs.weigh)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././explorer; }