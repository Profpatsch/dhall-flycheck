{ mkDerivation, aeson, base, bytestring, containers, dhall
, megaparsec, stdenv, text
}:
mkDerivation {
  pname = "dhall-flycheck";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers dhall megaparsec text
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
