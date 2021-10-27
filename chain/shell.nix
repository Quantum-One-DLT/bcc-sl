let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-chain";
  packages = ps: [ ps.bcc-sl-chain ];
})
