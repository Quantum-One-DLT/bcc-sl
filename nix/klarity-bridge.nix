{ runCommand, stdenv
, nixTools, bccConfig
, version ? "unstable", gitrev, buildId ? null }:

with stdenv.lib;

let
  bccWallet = nixTools.nix-tools.exes.bcc-wallet;
  bccTools = nixTools.nix-tools.exes.bcc-sl-tools;

in runCommand "bcc-klarity-bridge-${version}" {
  inherit version gitrev buildId;
} ''
  # Generate klarity-bridge
  mkdir -p $out/bin
  cd $out
  ${optionalString (buildId != null) "echo ${buildId} > build-id"}
  echo ${gitrev} > commit-id
  # this comes from bcc-sl.cabal, via an inherit in default.nix
  echo ${version} > version

  cp --no-preserve=mode -R ${bccConfig}/lib config
  cp ${bccConfig}/log-configs/klarity.yaml $out/config/log-config-prod.yaml
  cp ${bccTools}/bin/bcc-launcher* bin/
  cp ${bccTools}/bin/wallet-extractor* bin/
  cp ${bccTools}/bin/bcc-x509-certificates* bin/
  cp ${bccWallet}/bin/bcc-node* bin/

  ${optionalString (stdenv.targetPlatform.libc != "msvcrt") ''
    # test that binaries exit with 0
    ./bin/bcc-node --help > /dev/null
    HOME=$TMP ./bin/bcc-launcher --help > /dev/null
  ''}
''
