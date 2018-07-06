{ mkDerivation, stdenv, buildPackages
, base, bytestring, hspec, hspec-expectations-lifted, lifted-base, mtl, text
}:
mkDerivation {
  pname = "hs-kiwi";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring mtl text ];
  testHaskellDepends = [
    base hspec hspec-expectations-lifted lifted-base
  ];
  homepage = "https://github.com/reflex-frp/reflex-native";
  description = "Bindings to the Kiwi layout constraint solver";
  license = stdenv.lib.licenses.bsd3;
}
