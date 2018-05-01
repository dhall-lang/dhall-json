{ mkDerivation, aeson, aeson-pretty, base, bytestring, dhall
, optparse-generic, stdenv, text, unordered-containers, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dhall text unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall optparse-generic text yaml
  ];
  description = "Compile Dhall to JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
