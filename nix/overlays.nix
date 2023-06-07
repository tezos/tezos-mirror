{
  lib,
  stdenv,
  libiconv,
}: {
  pick-latest-packages = final: prev:
    builtins.mapAttrs
    (name: versions: versions.latest)
    prev.repository.packages;

  common-overlay = final: prev:
    lib.optionalAttrs (lib.hasAttr "ocaml-base-compiler" prev) {
      ocaml-base-compiler = prev.ocaml-base-compiler.override {
        # Compile faster!
        jobs = "$NIX_BUILD_CORES";
      };
    };

  darwin-overlay = final: prev: {
    hacl-star-raw = prev.hacl-star-raw.overrideAttrs (old: {
      # Uses unsupported command-line flags
      NIX_CFLAGS_COMPILE = ["-Wno-unused-command-line-argument"];
    });

    class_group_vdf = prev.class_group_vdf.overrideAttrs (old: {
      hardeningDisable =
        (old.hardeningDisable or [])
        ++ lib.optionals stdenv.isAarch64 ["stackprotector"];
    });

    # This package makes no sense to build on MacOS. Some OPAM package
    # incorrectly depends on it universally.
    inotify = null;
  };

  fix-rust-packages = final: prev: {
    conf-rust-2021 = prev.conf-rust.overrideAttrs (old: {
      propagatedNativeBuildInputs =
        (old.propagatedNativeBuildInputs or [])
        ++
        # Upstream conf-rust* packages don't request libiconv
        [libiconv];
    });
  };
}
