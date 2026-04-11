{ mkDerivation, base, genvalidity, genvalidity-sydtest
, genvalidity-text, lib, safe-coloured-text, safe-coloured-text-gen
, safe-coloured-text-parsing, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "safe-coloured-text-parsing-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity safe-coloured-text-gen safe-coloured-text-parsing
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-text safe-coloured-text
    safe-coloured-text-gen safe-coloured-text-parsing sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
