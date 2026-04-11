{ mkDerivation, attoparsec, base, lib, safe-coloured-text, text
, validity
}:
mkDerivation {
  pname = "safe-coloured-text-parsing";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base safe-coloured-text text validity
  ];
  license = "unknown";
}
