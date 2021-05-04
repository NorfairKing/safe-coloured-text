final: previous:
with final.haskell.lib;

{
  safeColouredTextPackages =
    let
      safeColouredTextPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
              )
            )
            (final.haskellPackages.autoexporter)
        );
    in
    {
      "safe-coloured-text" = safeColouredTextPkg "safe-coloured-text";
      "safe-coloured-text-gen" = safeColouredTextPkg "safe-coloured-text-gen";
      "safe-coloured-text-layout" = safeColouredTextPkg "safe-coloured-text-layout";
      "safe-coloured-text-layout-gen" = safeColouredTextPkg "safe-coloured-text-layout-gen";
      "safe-coloured-text-terminfo" = safeColouredTextPkg "safe-coloured-text-terminfo";
    };

  safeColouredTextRelease =
    final.symlinkJoin {
      name = "safe-coloured-text-release";
      paths = final.lib.attrValues final.safeColouredTextPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.safeColouredTextPackages // {
                  envparse = envparsePkg;
                }
            );
      }
    );
}
