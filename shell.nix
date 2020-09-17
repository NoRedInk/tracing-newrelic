{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell { buildInputs = [ pkgs.cabal-install pkgs.ghc pkgs.pcre ]; }
