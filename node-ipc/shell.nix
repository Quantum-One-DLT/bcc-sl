{ commonLib ? import ../lib.nix
, tbcoPkgs ? import ../. {}
, pkgs ? commonLib.pkgs
}:

pkgs.mkShell {
  name = "node-ipc-env";
  buildInputs = [ pkgs.nodejs tbcoPkgs.nix-tools.exes.bcc-wallet ];
}
