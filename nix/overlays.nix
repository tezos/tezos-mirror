{
  lib,
  stdenv,
  libiconv,
  pkg-config,
  darwin,
  rust-bin,
}: {
  pick-latest-packages = final: prev:
    prev.repository.select {
      opams = [
        {
          name = "stdcompat";
          opam = ../opam/virtual/stdcompat.opam.locked;
          version = "19";
        }
        {
          name = "octez-deps";
          opam = ../opam/virtual/octez-deps.opam.locked;
        }
      ];
    };

  common-overlay = final: prev:
    lib.optionalAttrs (lib.hasAttr "ocaml-base-compiler" prev) {
      ocaml-base-compiler = prev.ocaml-base-compiler.override {
        # Compile faster!
        jobs = "$NIX_BUILD_CORES";
      };
    }
    // {
      conf-pkg-config = final.lib.overrideNativeDepends prev.conf-pkg-config [pkg-config];

      ocamlformat-lib = prev.ocamlformat-lib.overrideAttrs (old: {
        propagatedBuildInputs = old.propagatedBuildInputs ++ [prev.ocp-indent];
      });
    };

  darwin-overlay = final: prev: {
    hacl-star-raw = prev.hacl-star-raw.overrideAttrs (old: {
      # Uses unsupported command-line flags
      NIX_CFLAGS_COMPILE = ["-Wno-unused-command-line-argument"];
    });

    class_group_vdf = prev.class_group_vdf.overrideAttrs (old: {
      hardeningDisable =
        (old.hardeningDisable or [])
        ++ ["stackprotector"];
    });

    caqti = prev.caqti.overrideAttrs (old: {
      buildInputs = (old.buildInputs or []) ++ [darwin.sigtool];
    });

    # This package makes no sense to build on MacOS. Some OPAM package
    # incorrectly depends on it universally.
    inotify = null;
  };

  fix-rust-packages = final: prev: {
    conf-rust = prev.lib.overrideNativeDepends prev.conf-rust [
      (rust-bin.fromRustupToolchainFile ../rust-toolchain)
    ];
  };
}
