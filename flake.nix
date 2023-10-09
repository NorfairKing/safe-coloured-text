{
  description = "safe-coloured-text";
  nixConfig = {
    extra-substituters = "https://safe-coloured-text.cachix.org";
    extra-trusted-public-keys = "safe-coloured-text.cachix.org-1:BriaRVzOr8kxni3hMCw/BRhKx85SkltChBw6PVdt884=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    nixpkgs-22_11.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_11
    , nixpkgs-22_05
    , nixpkgs-21_11
    , pre-commit-hooks
    , autodocodec
    , validity
    , fast-myers-diff
    , sydtest
    }:
    let
      system = "x86_64-linux";
      overlays = [
        self.overlays.${system}
        (import (validity + "/nix/overlay.nix"))
        (import (autodocodec + "/nix/overlay.nix"))
        (import (fast-myers-diff + "/nix/overlay.nix"))
        (import (sydtest + "/nix/overlay.nix"))
      ];
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        inherit overlays;
      };
      pkgs = pkgsFor nixpkgs;
      overrides = pkgs.callPackage ./nix/overrides { };
    in
    {
      overrides.${system} = overrides;
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = pkgs.haskellPackages.safeColouredTextPackages;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.safeColouredTextRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_11
              nixpkgs-22_05
              nixpkgs-21_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          release = pkgs.haskellPackages.safeColouredTextRelease;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "safe-coloured-text-shell";
        packages = p: builtins.attrValues p.safeColouredTextPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
