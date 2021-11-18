{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
}:
let
  versions = {
    "lts-16_11" = "89db531aea80df58584c9a9e3504ffd9617e6b48";
  };


  mkReleaseForVersion = version: rev:
    let
      pkgsf = import (builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      });
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
    p.safeColouredTextRelease.overrideAttrs (old: { name = "safe-coloured-text-release-${version}"; });
in
{
  release = pkgs.safeColouredTextRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).check;
} // builtins.mapAttrs mkReleaseForVersion versions 
