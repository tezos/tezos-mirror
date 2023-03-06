# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue work in the future.

let
  opam-nix-integration = import (
    fetchTarball {
      url = "https://github.com/vapourismo/opam-nix-integration/archive/80284cbf47b5829e3f08679f755f58730149839c.tar.gz";
      sha256 = "0crbjmcr7m29pkkqa63nmc0nnx7g243agkngi0ggyy2sbpy15ydp";
    }
  );

  rust-overlay = import (
    fetchTarball {
      url = "https://github.com/oxalica/rust-overlay/archive/b91706f9d5a68fecf97b63753da8e9670dff782b.tar.gz";
      sha256 = "1c34aihrnwv15l8hyggz92rk347z05wwh00h33iw5yyjxkvb8mqc";
    }
  );

  pkgs =
    import
      (fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/6025d713d198ec296eaf27a1f2f78983eccce4d8.tar.gz";
        sha256 = "0fa6nd1m5lr4fnliw21ppc4qdd4s85x448967333dvmslnvj35xi";
      })
      { overlays = [ opam-nix-integration.overlay rust-overlay ]; };

  mkFrameworkFlags = frameworks:
    pkgs.lib.concatStringsSep " " (
      pkgs.lib.concatMap
        (framework:
          [
            "-F${pkgs.darwin.apple_sdk.frameworks.${framework}}/Library/Frameworks"
            "-framework ${framework}"
          ]
        )
        frameworks
    );

  addFrameworksInputs = frameworks: drv:
    let
      frameworkPackages =
        pkgs.lib.lists.map
          (framework: pkgs.darwin.apple_sdk.frameworks.${framework})
          frameworks;
    in
    drv.overrideAttrs (old: {
      propagatedBuildInputs =
        (old.propagatedBuildInputs or [ ])
        ++
        frameworkPackages;
    });

  addFrameworksFlags = frameworks: drv:
    let frameworkFlags = mkFrameworkFlags frameworks; in
    drv.overrideAttrs (old: {
      NIX_LDFLAGS = "${old.NIX_LDFLAGS or ""} ${frameworkFlags}";
    });

  addFrameworks = frameworks: drv:
    addFrameworksFlags frameworks (
      addFrameworksInputs frameworks drv
    );

  tezos-opam-repository-rev = builtins.readFile (
    pkgs.runCommand
      "tezos-opam-repo-rev"
      {
        src = ./scripts/version.sh;
      }
      ''
        . $src
        echo -n $opam_repository_tag > $out
      ''
  );

  tezos-opam-repository = fetchTarball "https://gitlab.com/tezos/opam-repository/-/archive/${tezos-opam-repository-rev}/opam-repository-${tezos-opam-repository-rev}.tar.gz";

  common-overlay = final: prev:
    pkgs.lib.optionalAttrs (pkgs.lib.hasAttr "ocaml-base-compiler" prev) {
      ocaml-base-compiler = prev.ocaml-base-compiler.override {
        # Compile faster!
        jobs = "$NIX_BUILD_CORES";
      };
    };

  darwin-overlay = final: prev: {
    hacl-star-raw = prev.hacl-star-raw.overrideAttrs (old: {
      # Uses unsupported command-line flags
      NIX_CFLAGS_COMPILE = [ "-Wno-unused-command-line-argument" ];
    });

    class_group_vdf = addFrameworks [ "CoreServices" "CoreFoundation" ] (
      prev.class_group_vdf.overrideAttrs (old: {
        hardeningDisable =
          (old.hardeningDisable or [ ])
          ++
          pkgs.lib.optionals pkgs.stdenv.isAarch64 [ "stackprotector" ];
      })
    );

    # This package makes no sense to build on MacOS. Some OPAM package
    # incorrectly depends on it universally.
    inotify = null;

    dune = addFrameworksInputs [ "CoreServices" "Foundation" ] prev.dune;
  };

  packageSet = pkgs.opamPackages.overrideScope' (pkgs.lib.composeManyExtensions [
    # Set the opam-repository which has the package descriptions.
    (final: prev: {
      repository = prev.repository.override { src = tezos-opam-repository; };
    })

    # First overlay simply picks the package versions from Tezos'
    # opam-repository.
    (final: prev:
      builtins.mapAttrs
        (name: versions: versions.latest)
        prev.repository.packages
    )

    # Tweak common packages.
    common-overlay

    # Overlays for MacOS
    (if pkgs.stdenv.isDarwin then darwin-overlay else final: prev: { })

    # Tweak the dependencies.
    (final: prev: {
      conf-rust-2021 = prev.conf-rust.overrideAttrs (old: {
        propagatedNativeBuildInputs =
          (old.propagatedNativeBuildInputs or [ ])
          ++
          # Upstream conf-rust* packages don't request libiconv
          [ pkgs.libiconv ];
      });
    })
  ]);

  packages =
    builtins.filter
      pkgs.lib.attrsets.isDerivation
      (builtins.attrValues packageSet);

  packageLibDirs =
    builtins.filter builtins.pathExists (
      builtins.map (package: "${package}/lib/${package.pname}") packages
    );

  packageIncludeArgs = builtins.map (dir: "-I${dir}") packageLibDirs;

  fakeOpamSwitchPrefix =
    pkgs.runCommand
      "fake-opam-switch-prefix"
      { }
      ''
        mkdir -p $out/share/zcash-params
        cp ${tezos-opam-repository}/zcash-params/sapling-output.params $out/share/zcash-params
        cp ${tezos-opam-repository}/zcash-params/sapling-spend.params $out/share/zcash-params
      '';

  opam-repository-rev = builtins.readFile (
    pkgs.runCommand
      "opam-repo-rev"
      {
        src = ./scripts/version.sh;
      }
      ''
        . $src
        echo -n $full_opam_repository_tag > $out
      ''
  );

  kernelPackageSet = [
    # Packages required to build & develop kernels
    (pkgs.rust-bin.stable."1.66.0".default.override {
      targets = [ "wasm32-unknown-unknown" ];
    })
    pkgs.rust-analyzer
    pkgs.wabt
  ];

  devPackageSet = pkgs.opamPackages.overrideScope' (
    pkgs.lib.composeManyExtensions [
      # Set the opam-repository which has the package descriptions.
      (final: prev: {
        repository = prev.repository.override {
          src = fetchTarball "https://github.com/ocaml/opam-repository/archive/${opam-repository-rev}.tar.gz";
        };
      })

      # Specify the constraints we have.
      (final: prev: prev.repository.select {
        packageConstraints = [
          "ocaml=${packageSet.ocaml.version}"
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
      common-overlay

      # Overlays for MacOS
      (if pkgs.stdenv.isDarwin then darwin-overlay else final: prev: { })
    ]
  );

in

pkgs.mkShell {
  name = "tezos-shell";

  NIX_LDFLAGS = pkgs.lib.optional pkgs.stdenv.isDarwin (
    mkFrameworkFlags [
      "CoreFoundation"
      "IOKit"
      "AppKit"
    ]
  );

  NIX_CFLAGS_COMPILE =
    # Silence errors (-Werror) for unsupported flags on MacOS.
    pkgs.lib.optionals
      pkgs.stdenv.isDarwin
      [ "-Wno-unused-command-line-argument" ]
    ++
    # Make sure headers files are in scope.
    packageIncludeArgs;

  hardeningDisable =
    pkgs.lib.optionals
      (pkgs.stdenv.isAarch64 && pkgs.stdenv.isDarwin)
      [ "stackprotector" ];

  buildInputs = with pkgs;
    kernelPackageSet
    ++
    packages
    ++
    [
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
    ++
    (
      if pkgs.stdenv.isDarwin then [
        fswatch
      ] else [
        inotify-tools
      ]
    );

  # Disable OPAM usage in Makefile.
  TEZOS_WITHOUT_OPAM = true;

  # $OPAM_SWITCH_PREFIX is used to find the ZCash parameters.
  OPAM_SWITCH_PREFIX = fakeOpamSwitchPrefix;
}
