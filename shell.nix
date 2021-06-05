let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
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
