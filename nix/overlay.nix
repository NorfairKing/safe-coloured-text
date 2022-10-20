final: prev:
with final.haskell.lib;
{

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            safeColouredTextPkg = name:
              doBenchmark (buildStrictly (self.callPackage (../${name}/default.nix) { }));
            safeColouredTextPackages =
              final.lib.genAttrs [
                "safe-coloured-text"
                "safe-coloured-text-gen"
                "safe-coloured-text-layout"
                "safe-coloured-text-layout-gen"
                "safe-coloured-text-terminfo"

              ]
                safeColouredTextPkg;
          in
          {
            inherit safeColouredTextPackages;

            safeColouredTextRelease =
              final.symlinkJoin {
                name = "safe-coloured-text-release";
                paths = final.lib.attrValues self.safeColouredTextPackages;
              };
          } // safeColouredTextPackages
      );
  });
}
