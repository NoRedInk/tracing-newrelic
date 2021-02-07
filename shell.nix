let pkgs = import nix/packages.nix;
in pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install pkgs.ghc pkgs.pcre pkgs.niv ];
}
