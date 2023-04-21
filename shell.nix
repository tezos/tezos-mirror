# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
let
  sources = import ./nix/sources.nix;
  pkgs = sources.pkgs;

  overlays = pkgs.callPackage ./nix/overlays.nix {};

  kernelPackageSet = [
    # Packages required to build & develop kernels
    (pkgs.rust-bin.stable."1.66.0".default.override {
      targets = ["wasm32-unknown-unknown"];
    })
    pkgs.rust-analyzer
    pkgs.wabt
    pkgs.clang
  ];

  mainPackage = (import ./default.nix).overrideAttrs (old: {
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
          src = pkgs.callPackage ./nix/opam-repo.nix {};
        };
      })

      # Specify the constraints we have.
      (final: prev:
        prev.repository.select {
          packageConstraints = [
            "ocaml=${mainPackage.passthru.ocamlVersion}"
            "utop=2.9.0"
            "ocaml-lsp-server>=1.9.0"
            "merlin"
            "odoc"
            "ocp-indent"
            "js_of_ocaml-compiler"
            "ocamlformat-rpc"
            "merge-fmt"
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

    hardeningDisable =
      pkgs.lib.optionals
      (pkgs.stdenv.isAarch64 && pkgs.stdenv.isDarwin)
      ["stackprotector"];

    inherit (mainPackage) NIX_LDFLAGS NIX_CFLAGS_COMPILE TEZOS_WITHOUT_OPAM OPAM_SWITCH_PREFIX;

    buildInputs = with pkgs;
      kernelPackageSet
      ++ mainPackage.buildInputs
      ++ [
        nodejs
        cacert
        curl
        shellcheck
        poetry
        devPackageSet.ocaml-lsp-server
        devPackageSet.ocamlformat-rpc
        devPackageSet.ocp-indent
        devPackageSet.merlin
        devPackageSet.utop
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
