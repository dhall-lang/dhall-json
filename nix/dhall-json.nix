{ mkDerivation, aeson, aeson-pretty, base, bytestring, dhall
, insert-ordered-containers, optparse-applicative, stdenv, tasty
, tasty-hunit, text, unordered-containers, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.2.2";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dhall insert-ordered-containers optparse-applicative
    text unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall optparse-applicative text
    yaml
  ];
  testHaskellDepends = [ aeson base dhall tasty tasty-hunit text ];
  description = "Compile Dhall to JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
