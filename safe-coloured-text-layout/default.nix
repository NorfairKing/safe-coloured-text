{ mkDerivation, base, lib, safe-coloured-text, text, validity }:
mkDerivation {
  pname = "safe-coloured-text-layout";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [ base safe-coloured-text text validity ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely layout output coloured text";
  license = lib.licenses.mit;
}
