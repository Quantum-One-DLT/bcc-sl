let
  commonLib = import ./lib.nix;
  disabled = [];
in { bcc ? { outPath = ./.; rev = "abcdef"; }, ... }@args:
let
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { target = "x86_64-linux"; gitrev = bcc.rev; };
      x86_64-darwin = import ./. { target = "x86_64-darwin"; gitrev = bcc.rev; };
      x86_64-windows = import ./. { target = "x86_64-windows"; gitrev = bcc.rev; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeConnectScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").connectScripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").connectScripts.${cluster}.${name};
    };
  in {
    explorer = getScript "explorer";
    proposal-ui = getScript "proposal-ui";
    wallet = getScript "wallet";
  };
  wrapDockerImage = cluster: let
    images = (getArchDefault "x86_64-linux").dockerImages;
    wrapImage = image: commonLib.pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage ${image}
      EOF
    '';
  in {
    wallet = wrapImage images.${cluster}.wallet;
    explorer = wrapImage images.${cluster}.explorer;
  };
  makeRelease = cluster: {
    name = cluster;
    value = {
      connectScripts = makeConnectScripts cluster;
      dockerImage = wrapDockerImage cluster;
    };
  };
in
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  _this = bcc;
  package-set-path = ./nix/nix-tools.nix;
  packages = [
    "bcc-sl"
    "bcc-sl-auxx"
    "bcc-sl-chain"
    "bcc-sl-core"
    "bcc-sl-crypto"
    "bcc-sl-db"
    "bcc-sl-generator"
    "bcc-sl-infra"
    "bcc-sl-faucet"
    "bcc-sl-networking"
    "bcc-sl-node"
    "bcc-sl-tools"
    "bcc-sl-util"
    "bcc-sl-x509"
    "bcc-wallet"
    "bcc-sl-explorer"
    "bcc-sl-cluster"
  ];
  extraBuilds = {
    inherit (default) tests demoCluster explorerFrontend faucetFrontend explorerPythonAPI;
    klarity-bridge = commonLib.pkgs.lib.mapAttrs (k: v: (getArchDefault k).klarity-bridge) {
      x86_64-linux = 1;
      x86_64-darwin = 1;
      x86_64-windows = 1;
    };
  } // (builtins.listToAttrs (map makeRelease [ "mainnet" "staging" "demo" "testnet" ]));
  required-targets = jobs: [
    jobs.nix-tools.exes.bcc-sl-node.x86_64-linux
    jobs.nix-tools.exes.bcc-sl-auxx.x86_64-linux
    jobs.nix-tools.exes.bcc-sl-faucet.x86_64-linux
    jobs.nix-tools.exes.bcc-sl-explorer.x86_64-linux
    jobs.nix-tools.exes.bcc-wallet.x86_64-linux
    jobs.nix-tools.exes.bcc-wallet.x86_64-darwin
  ] ++ (builtins.attrValues jobs.tests)
  ++ (builtins.attrValues jobs.klarity-bridge);
} (builtins.removeAttrs args ["bcc"]))
