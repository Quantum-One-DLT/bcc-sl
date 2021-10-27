let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-db";
  packages = ps: [ ps.bcc-sl-db ];
})
