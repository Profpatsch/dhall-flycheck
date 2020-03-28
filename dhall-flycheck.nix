{ mkDerivation, aeson, base, bytestring, containers, dhall
, exceptions, lens-family-core, megaparsec, mtl, prettyprinter
, stdenv, text, transformers
}:
mkDerivation {
  pname = "dhall-flycheck";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers dhall exceptions lens-family-core
    megaparsec mtl prettyprinter text transformers
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
