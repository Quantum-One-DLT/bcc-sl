{
  commonLib,
  customConfig,
  bccConfig,
  nixTools
}:

let
  pkgs = commonLib.pkgs;
  connect = let
    walletConfigFile = ../custom-wallet-config.nix;
    walletConfig = if builtins.pathExists walletConfigFile then import walletConfigFile else {};
    in
      args: pkgs.callPackage ./launch/connect-to-cluster (args // {
        inherit bccConfig;
        inherit (nixTools.nix-tools.cexes.bcc-wallet) bcc-node;
        inherit (nixTools.nix-tools.cexes.bcc-sl-explorer) bcc-explorer;
        inherit (nixTools.nix-tools.cexes.bcc-sl-tools) bcc-x509-certificates;
      } // walletConfig);

  demoCluster = pkgs.callPackage ./launch/demo-cluster {
    inherit bccConfig;
    inherit (nixTools.nix-tools.cexes.bcc-sl-cluster) bcc-sl-cluster-demo bcc-sl-cluster-prepare-environment;
    inherit (nixTools.nix-tools.cexes.bcc-wallet) bcc-node;
  };
in {
  # connect function required for acceptanceTests
  inherit connect;
  connectScripts = commonLib.forEnvironments (environment:
  {
    wallet = connect { environment = environment.name; };
    explorer = connect { environment = environment.name; executable = "explorer"; };
    proposal-ui = pkgs.callPackage ./launch/proposal-ui {
      inherit bccConfig;
      inherit (nixTools.nix-tools.cexes.bcc-sl-script-runner) testcases;
      environment = environment.name;
    };

  });
  inherit demoCluster;
  demo-function = { disableClientAuth, numImportedWallets, runWallet, customConfigurationFile }: demoCluster.override {
    inherit disableClientAuth numImportedWallets runWallet customConfigurationFile;
  };
  dockerImages = let
    build = args: pkgs.callPackage ./docker.nix ({
      inherit bccConfig connect;
      inherit (nixTools.nix-tools.cexes.bcc-sl-node) bcc-node-simple;
    } // args);
    makeDockerImage = environment:
      build { environment = environment.name; } // {
        wallet   = build { environment = environment.name; type = "wallet"; };
        explorer = build { environment = environment.name; type = "explorer"; };
        node     = build { environment = environment.name; type = "node"; };
      };
  in commonLib.forEnvironments makeDockerImage;
}
