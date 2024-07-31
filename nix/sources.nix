let
  default-opam-nix-integration-src = fetchTarball {
    url = "https://github.com/vapourismo/opam-nix-integration/archive/0f98236c75cdb436be7669ccaa249264456baa37.tar.gz";
    sha256 = "0m9v7s8zgkr280f7l8qy12dnjmi7pf0mza16b5xral9fsqi9j1sa";
  };

  default-rust-overlay-src = fetchTarball {
    url = "https://github.com/oxalica/rust-overlay/archive/38c2f156fca1868c8be7195ddac150522752f6ab.tar.gz";
    sha256 = "05jcfpa42kqc4h7bf9ymgps5abjnn7b4327maxki0d53y17959aa";
  };

  default-pkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    sha256 = "1lr1h35prqkd1mkmzriwlpvxcb34kmhc9dnr48gkm8hh089hifmx";
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
  in {
    inherit pkgs riscv64Pkgs opam-repository;
  }
