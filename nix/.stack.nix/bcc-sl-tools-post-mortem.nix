{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { for-installer = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "bcc-sl-tools-post-mortem";
        version = "3.2.0";
        };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@blockchain-company.io";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Bcc SL - post-mortem tool";
      description = "Bcc SL - post-mortem tool";
      buildType = "Simple";
      };
    components = {
      exes = {
        "bcc-post-mortem" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs.Chart)
            (hsPkgs.Chart-diagrams)
            (hsPkgs.MonadRandom)
            (hsPkgs.aeson)
            (hsPkgs.attoparsec)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.bcc-sl-infra)
            (hsPkgs.bcc-sl-util)
            (hsPkgs.containers)
            (hsPkgs.cassava)
            (hsPkgs.directory)
            (hsPkgs.fgl)
            (hsPkgs.filepath)
            (hsPkgs.foldl)
            (hsPkgs.graphviz)
            (hsPkgs.optparse-applicative)
            (hsPkgs.pipes)
            (hsPkgs.pipes-bytestring)
            (hsPkgs.pipes-interleave)
            (hsPkgs.pipes-safe)
            (hsPkgs.process)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././tools/post-mortem; }