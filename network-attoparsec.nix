{ mkDerivation, attoparsec, base, bytestring, enclosed-exceptions
, exceptions, hspec, lifted-base, monad-control, mtl, network
, network-simple, stdenv, transformers
}:
mkDerivation {
  pname = "network-attoparsec";
  version = "0.12.2";
  src = ./.;
  buildDepends = [
    attoparsec base bytestring enclosed-exceptions exceptions
    lifted-base monad-control mtl network transformers
  ];
  testDepends = [
    attoparsec base bytestring exceptions hspec mtl network
    network-simple transformers
  ];
  homepage = "http://github.com/solatis/haskell-network-attoparsec";
  description = "Utility functions for running a parser against a socket";
  license = stdenv.lib.licenses.mit;
}
