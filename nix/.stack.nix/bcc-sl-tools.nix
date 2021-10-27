{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { for-installer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-sl-tools"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - Tools";
      description = "Bcc SL - Tools";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.network-transport-tcp)
          (hsPkgs.parsers)
          (hsPkgs.text)
          (hsPkgs.trifecta)
          (hsPkgs.universum)
          ];
        };
      exes = {
        "bcc-genupdate" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.cryptonite)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.process)
            (hsPkgs.tar)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.unix-compat)
            ];
          };
        "bcc-keygen" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.base58-bytestring)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.Glob)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
            (hsPkgs.universum)
            ];
          };
        "bcc-launcher" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.aeson-options)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.bcc-sl-db)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-tools)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.lifted-async)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.safe-exceptions)
            (hsPkgs.silently)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.yaml)
            ] ++ (if !system.isWindows
            then [ (hsPkgs.unix) ]
            else [ (hsPkgs.Win32) ]);
          };
        "bcc-addr-convert" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.text)
            (hsPkgs.universum)
            ];
          };
        "bcc-cli-docs" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.text)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        "bcc-blockchain-analyser" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.base)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-binary)
            (hsPkgs.bcc-sl-chain)
            (hsPkgs.bcc-sl-core)
            (hsPkgs.bcc-sl-crypto)
            (hsPkgs.bcc-sl-db)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serokell-util)
            (hsPkgs.tabl)
            (hsPkgs.text)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        "bcc-x509-certificates" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bcc-sl-x509)
            (hsPkgs.filepath)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
            ];
          };
        "genesis-hash" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.universum)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.canonical-json)
            ];
          };
        "wallet-extractor" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.universum)
            (hsPkgs.text)
            (hsPkgs.bcc-sl)
            (hsPkgs.bcc-sl-util)
            ];
          };
        };
      tests = {
        "bcc-sl-tools-test" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bcc-sl-tools)
            (hsPkgs.bcc-sl-util-test)
            (hsPkgs.directory)
            (hsPkgs.hspec)
            (hsPkgs.temporary)
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././tools; }