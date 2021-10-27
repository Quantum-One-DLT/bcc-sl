let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-lib";
  packages = ps: [ ps.bcc-sl ];
})
