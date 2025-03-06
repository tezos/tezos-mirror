# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
{sources ? import ./nix/sources.nix {}}: let
  pkgs = sources.pkgs;

  overlays = pkgs.callPackage ./nix/overlays.nix {};

  kernelPackageSet =
    [
      # Packages required to build & develop kernels
      pkgs.rustup
      pkgs.wabt

      # Cross-compilation for RISC-V
      pkgs.pkgsCross.riscv64.pkgsStatic.stdenv.cc
      pkgs.autoconf

      # Formatter/LSP for Cargo manifests (and TOML in general)
      pkgs.taplo
    ]
    # On Mac, Rust's standard library needs libiconv
    ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.libiconv;

  mainPackage = (import ./default.nix {inherit sources;}).overrideAttrs (old: {
    # This makes the shell load faster.
    # Usually Nix will try to load the package's source, which in this case
    # is the entire repository. Given the repository is fairly large, and we
    # don't actually need the source to build the development dependencies,
    # we just remove the dependency on the source entirely.
    src = null;
  });

  devPackageSet = pkgs.opamPackages.overrideScope (
    pkgs.lib.composeManyExtensions [
      # Set the opam-repository which has the package descriptions.
      (final: prev: {
        repository = prev.repository.override {
          src = sources.opam-repository;
        };
      })

      # Specify the constraints we have.
      (final: prev:
        prev.repository.select {
          opams = [
            {
              name = "stdcompat";
              opam = ./opam/virtual/stdcompat.opam.locked;
              version = "19";
            }
            {
              name = "octez-deps";
              opam = ./opam/virtual/octez-deps.opam.locked;
            }
            {
              name = "octez-dev-deps";
              opam = ./opam/virtual/octez-dev-deps.opam;
            }
          ];

          packageConstraints = [
            "ocamlformat-rpc"
          ];
        })

      # Tweak common packages.
      overlays.common-overlay

      # Overlays for MacOS
      (
        if pkgs.stdenv.isDarwin
        then overlays.darwin-overlay
        else final: prev: {}
      )
    ]
  );
in
  pkgs.mkShell {
    name = "tezos-shell";

    hardeningDisable = ["stackprotector" "zerocallusedregs"];

    inherit (mainPackage) NIX_LDFLAGS NIX_CFLAGS_COMPILE TEZOS_WITHOUT_OPAM OPAM_SWITCH_PREFIX;

    inputsFrom = [mainPackage];

    buildInputs = with pkgs;
      kernelPackageSet
      ++ [
        nodejs
        cacert
        curl
        shellcheck
        shfmt
        poetry
        kaitai-struct-compiler
        devPackageSet.ocaml-lsp-server
        devPackageSet.ocamlformat-rpc
        devPackageSet.ocp-indent
        devPackageSet.merlin
        devPackageSet.utop
        devPackageSet.odoc
      ]
      ++ (
        if pkgs.stdenv.isDarwin
        then [
          fswatch
        ]
        else [
          inotify-tools
        ]
      );
  }
