let
  default-opam-nix-integration-src = fetchTarball {
    url = "https://github.com/vapourismo/opam-nix-integration/archive/646431dec7cd75fb79101be4e6ce3ef07896d972.tar.gz";
    sha256 = "0k4p0sdikvd8066x17xarvspqhcgnhm9mij4xvs058sqm1797sl1";
  };

  default-rust-overlay-src = fetchTarball {
    url = "https://github.com/oxalica/rust-overlay/archive/b037d65c988421b54024e62691eace4f2fe623bc.tar.gz";
    sha256 = "05jcfpa42kqc4h7bf9ymgps5abjnn7b4327maxki0d53y17959aa";
  };

  default-pkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/cd07839e2e61f8b7c467f20a896c3f9e63a04918.tar.gz";
    sha256 = "1xr250f9z72v560pkvi25iwclnysjn8h9mw8cdnjl4izq0milmzi";
  };
in
  {
    opam-nix-integration-src ? default-opam-nix-integration-src,
    pkgs-src ? default-pkgs-src,
    rust-overlay-src ? default-rust-overlay-src,
  }: let
    opam-nix-integration = import opam-nix-integration-src;

    rust-overlay = import rust-overlay-src;

    pkgs = import pkgs-src {
      overlays = [
        opam-nix-integration.overlay
        rust-overlay
      ];
    };

    riscv64Pkgs = import pkgs-src {
      crossSystem.config = "riscv64-unknown-linux-gnu";
    };

    opam-repository = pkgs.callPackage ./opam-repo.nix {};

    tezos-opam-repository = pkgs.callPackage ./tezos-opam-repo.nix {};
  in {
    inherit pkgs riscv64Pkgs opam-repository tezos-opam-repository;
  }
