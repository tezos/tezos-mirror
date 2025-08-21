{sources ? import ./nix/sources.nix {}}: let
  pkgs = sources.pkgs;

  overlays = pkgs.callPackage ./overlays.nix {};
in
  pkgs.opamPackages.overrideScope (pkgs.lib.composeManyExtensions [
    # Set the opam-repository which has the package descriptions.
    (final: prev: {
      repository = prev.repository.override {
        src = sources.opam-repository;
      };
    })

    # First overlay simply picks the package versions from Tezos'
    # opam-repository.
    overlays.pick-latest-packages

    # Tweak common packages.
    overlays.common-overlay

    # Overlays for MacOS
    (
      if pkgs.stdenv.isDarwin
      then overlays.darwin-overlay
      else final: prev: {}
    )

    # Tweak the dependencies.
    overlays.fix-rust-packages
  ])
