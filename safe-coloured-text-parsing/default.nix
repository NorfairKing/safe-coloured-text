{ mkDerivation, attoparsec, base, lib, safe-coloured-text, text
, validity
}:
mkDerivation {
  pname = "safe-coloured-text-parsing";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base safe-coloured-text text validity
  ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Parse coloured text";
  license = lib.licenses.mit;
}
