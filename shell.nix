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
      sources.riscv64Pkgs.clangStdenv.cc

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

  devPackageSet = pkgs.opamPackages.overrideScope' (
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

  clangNoArch =
    if pkgs.stdenv.isDarwin
    then
      pkgs.clang.overrideAttrs (old: {
        postFixup = ''
          ${old.postFixup or ""}

          # On macOS this contains '-march' and '-mcpu' flags. These flags
          # would be used for any invocation of Clang.
          # Removing those makes the resulting Clang wrapper usable when
          # cross-compiling where passing '-march' and '-mcpu' would not
          # make sense.
          echo > $out/nix-support/cc-cflags-before
        '';
      })
    else pkgs.clang;
in
  pkgs.mkShell {
    name = "tezos-shell";

    hardeningDisable = ["stackprotector"];

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

    # This tells the 'cc' Rust crate to build using this C compiler when
    # targeting other architectures.
    CC_wasm32_unknown_unknown = "${clangNoArch}/bin/clang";
    CC_riscv64gc_unknown_linux_gnu = "${clangNoArch}/bin/clang";
    CC_riscv64gc_unknown_none_elf = "${clangNoArch}/bin/clang";
    CC_riscv64gc_unknown_hermit = "${clangNoArch}/bin/clang";
  }
