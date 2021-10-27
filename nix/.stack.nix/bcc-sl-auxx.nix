{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-auxx"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - Auxx";
      description = "Bcc SL - Auxx";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.Earley)
          (hsPkgs.MonadRandom)
          (hsPkgs.ansi-wl-pprint)
          (hsPkgs.async)
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.bcc-sl)
          (hsPkgs.bcc-sl-chain)
          (hsPkgs.bcc-sl-chain-test)
          (hsPkgs.bcc-sl-client)
          (hsPkgs.bcc-sl-core)
          (hsPkgs.bcc-sl-core-test)
          (hsPkgs.bcc-sl-crypto)
          (hsPkgs.bcc-sl-db)
          (hsPkgs.bcc-sl-generator)
          (hsPkgs.bcc-sl-infra)
          (hsPkgs.bcc-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.haskeline)
          (hsPkgs.lens)
          (hsPkgs.loc)
          (hsPkgs.megaparsec)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parser-combinators)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.serokell-util)
          (hsPkgs.split)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.validation)
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs.unix);
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
          ];
        };
      exes = {
        "bcc-auxx" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-auxx)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-db)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-networking)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.temporary)
            (hsPkgs.network-transport-tcp)
            (hsPkgs.safe-exceptions)
            (hsPkgs.universum)
            (hsPkgs.formatting)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      tests = {
        "bcc-auxx-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.bcc-sl-auxx)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.hspec)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././auxx; }