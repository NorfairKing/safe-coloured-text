{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import ./nix/pkgs.nix { inherit sources nixpkgs system; }
}:
let
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-21_05" = sources.nixpkgs-21_05;
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-22_05" = sources.nixpkgs-22_05;
  };


  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs;
      };
    in
    p.safeColouredTextRelease.overrideAttrs (old: {
      name = "safe-coloured-text-release-${version}";
    });
in
{
  release = pkgs.safeColouredTextRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).check;
  hoogle = pkgs.buildEnv {
    name = "safe-coloured-text-hoogle";
    paths = [ (pkgs.haskellPackages.ghcWithHoogle (ps: pkgs.lib.attrValues pkgs.safeColouredTextPackages)) ];
  };
  shell = pkgs.symlinkJoin {
    name = "safe-coloured-text-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
} // builtins.mapAttrs mkReleaseForVersion versions
