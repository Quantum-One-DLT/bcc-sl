let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "bcc-sl-crypto";
  packages = ps: [ ps.bcc-sl-crypto ps.cabal-install ];
})
