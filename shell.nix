let
  pkgs = import ./nix/pkgs.nix { };
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "safe-coloured-text-shell";
  buildInputs = with pkgs; [
    zlib
    niv
  ] ++ pre-commit.tools;
  shellHook = ''
    ${pre-commit.check.shellHook}
  '';
}
