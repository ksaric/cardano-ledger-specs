{ pkgs ? (import ../../../../nix/lib.nix).pkgs19_10
}:


with pkgs;

stdenv.mkDerivation {
  name = "issabelleEnv";

  buildInputs = [
    isabelle
  ];

  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Isabelle environment"  ;
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}