{ mkDerivation, base, pure, pure-cond, pure-json, pure-lifted, pure-prop, stdenv }:
mkDerivation {
  pname = "pure-intersection";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-cond pure-json pure-lifted pure-prop ];
  homepage = "github.com/grumply/pure-intersection";
  description = "IntersectionObserver API integration";
  license = stdenv.lib.licenses.bsd3;
}
