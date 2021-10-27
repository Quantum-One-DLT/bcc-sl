{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { golden-tests = false; golden-tests-exe = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-crypto"; version = "1.1.0"; };
      license = "MIT";
      copyright = "2016-2021 TBCO";
      maintainer = "contact@typed.io";
      author = "Vincent Hanquez";
      homepage = "https://github.com/The-Blockchain-Company/bcc-crypto#readme";
      url = "";
      synopsis = "Cryptography primitives for bcc";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.memory)
          (hsPkgs.deepseq)
          (hsPkgs.bytestring)
          (hsPkgs.basement)
          (hsPkgs.foundation)
          (hsPkgs.cryptonite)
          (hsPkgs.cryptonite-openssl)
          (hsPkgs.hashable)
          (hsPkgs.integer-gmp)
          ];
        };
      exes = {
        "golden-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.basement)
            (hsPkgs.foundation)
            (hsPkgs.memory)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.bcc-crypto)
            ] ++ (pkgs.lib).optional (flags.golden-tests-exe) (hsPkgs.inspector);
          };
        };
      tests = {
        "bcc-crypto-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.memory)
            (hsPkgs.cryptonite)
            (hsPkgs.bcc-crypto)
            (hsPkgs.basement)
            (hsPkgs.foundation)
            ];
          };
        "bcc-crypto-golden-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.basement)
            (hsPkgs.foundation)
            (hsPkgs.memory)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.bcc-crypto)
            ] ++ (pkgs.lib).optional (flags.golden-tests) (hsPkgs.inspector);
          };
        };
      benchmarks = {
        "bcc-crypto-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.memory)
            (hsPkgs.cryptonite)
            (hsPkgs.bcc-crypto)
            (hsPkgs.gauge)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-crypto";
      rev = "4590efa638397e952a51a8994b5543e4ea3c1ecd";
      sha256 = "0hl2n3bba5v2j0lmxhs7hs01z3aznh2bwf9cb434icq2g0bl8ms3";
      });
    }