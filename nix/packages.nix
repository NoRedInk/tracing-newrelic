let
  sources = import ./sources.nix { };
  nivOverlay = _: pkgs: {
    niv = (import sources.niv { }).niv; # use the sources :)
  };
in import (sources.nixpkgs) { overlays = [ nivOverlay ]; }
