{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  structures = (
    with rec {
      structuresSource = pkgs.lib.cleanSource ../.;
      structuresBasic  = self.callCabal2nix "structures" structuresSource { };
    };
    overrideCabal structuresBasic (old: {
    })
  );
}
