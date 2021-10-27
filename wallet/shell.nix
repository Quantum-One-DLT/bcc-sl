let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-wallet";
  packages = ps: [ ps.bcc-wallet ];
})
