let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-infra";
  packages = ps: [ ps.bcc-sl-infra ];
})
