{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ./nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "safe-coloured-text-shell";
  buildInputs = with pkgs; [
    zlib
    (import sources.niv { }).niv
  ] ++ pre-commit.tools;
  shellHook = ''
    ${pre-commit.check.shellHook}
  '';
}
