{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
}:
import nixpkgs {
  inherit system;
  overlays =
    [
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (final: previous: { niv = (import sources.niv { pkgs = final; }).niv; })
      (import (sources.autodocodec + "/nix/overlay.nix"))
      (import (sources.sydtest + "/nix/overlay.nix"))
      (import (sources.validity + "/nix/overlay.nix"))
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
