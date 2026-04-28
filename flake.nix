# SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT
#
# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    # Latest nixpkgs that provides foundry 1.5.0
    nixpkgs-foundry.url = "github:NixOS/nixpkgs/c4a64f7682aadb3986b592032e30e5d76deb74fb";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

    # The Opam repository is one way how dependencies are pinned for Octez.
    # Must match $opam_repository_tag in scripts/version.sh — the
    # ci-check-version-sh-lock app enforces this.
    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository/8a528d6bb48e4be260fb670a1754df39a1192147";
    };

    # This library helps us build OCaml packages and dependencies.
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-foundry,
      flake-utils,
      opam-repository,
      opam-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgsFoundry = nixpkgs-foundry.legacyPackages.${system};
        opam = opam-nix.lib.${system};

        # We use this as a fake Opam switch. Some applications will look in the Opam switch to find
        # files.
        fakeOpamSwitchPrefix =
          pkgs.runCommand "fake-opam-switch-prefix"
            {
              inherit (pkgs.callPackage ./nix/dal-files.nix { }) g1 g2;
            }
            ''
              mkdir -p $out/share/zcash-params $out/share/dal-trusted-setup
              cp ${./images/ci/zcash-params}/* $out/share/zcash-params
              cp $g1 $out/share/dal-trusted-setup/srsu_zcash_g1
              cp $g2 $out/share/dal-trusted-setup/srsu_zcash_g2
            '';

        # The repository contains the pinned OCaml dependencies which are expected to be in scope
        # when compiling stuff for Octez. These particular dependencies are not upstreamed yet, and
        # hence not part of the `opam-repository`.
        pinnedRepo = opam.getPinDepends (opam.importOpam ./opam/virtual/octez-deps.opam.locked) { };

        # We want to use the locked Opam file for dependencies to ensure the versions are exactly as
        # expected. To get access to them, we need to put them into a package repository.
        virtualRepoSrc = pkgs.runCommand "virtual-repo" { } ''
          mkdir -p $out
          cp ${./opam/virtual/octez-deps.opam.locked} $out/octez-deps.opam
          cp ${./opam/virtual/octez-dev-deps.opam} $out/octez-dev-deps.opam
        '';

        virtualRepo = opam.makeOpamRepo virtualRepoSrc;

        # Collection of repositories that are in scope for the OCaml dependency version resolver.
        repos = [
          virtualRepo
          opam-repository
        ]
        ++ pinnedRepo;

        # This scope is used for building in a development shell. It contains all the right
        # development dependencies.
        depsScope =
          (opam.queryToScope { inherit repos pkgs; } {
            octez-deps = "dev";
            octez-dev-deps = "dev";
            ocamlformat-rpc = "*";
          }).overrideScope
            (
              final: prev:
              let
                # GitHub regenerated the source tarball for ambient-context v0.1.0,
                # invalidating the MD5 hash in opam-repository. Use fetchFromGitHub
                # which is content-addressed (hashes the git tree, not the tarball)
                # and therefore stable across GitHub tarball regenerations.
                ambientContextSrc = pkgs.fetchFromGitHub {
                  owner = "ELLIOTTCABLE";
                  repo = "ocaml-ambient-context";
                  rev = "v0.1.0";
                  hash = "sha256-d7xoncentvuYSqiwkJqXtaA1ddNIcg/5BjnjV9zW3MA=";
                };
              in
              {
                ambient-context = prev.ambient-context.overrideAttrs { src = ambientContextSrc; };
                ambient-context-lwt = prev.ambient-context-lwt.overrideAttrs { src = ambientContextSrc; };
              }
              // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (
                let
                  # opam-nix's debian.nix overlay adds a broken postInstall to hidapi
                  # on Linux: it creates symlinks libhidapi.la -> libhidapi-hidraw.la
                  # (doesn't exist, .la files are stripped) and libhidapi.so ->
                  # libhidapi-hidraw.so.0.0.0 (wrong version, actual is 0.15.0).
                  # This causes the noBrokenSymlinks check to fail.
                  # Fix by replacing the postInstall with correct symlinks.
                  fixedHidapi = pkgs.hidapi.overrideAttrs {
                    postInstall = ''
                      mv $out/include/hidapi/* $out/include
                      rm -d $out/include/hidapi
                      target=$(find $out/lib -name 'libhidapi-*.so' ! -name 'libhidapi.so' -print -quit)
                      if [ -z "$target" ]; then
                        echo "fixedHidapi: no libhidapi-*.so found in $out/lib" >&2
                        exit 1
                      fi
                      ln -s "$(basename "$target")" $out/lib/libhidapi.so
                    '';
                  };
                  replaceHidapi =
                    deps: [ fixedHidapi ] ++ (builtins.filter (d: !(d ? pname && d.pname == "hidapi")) deps);
                in
                {
                  conf-hidapi = prev.conf-hidapi.overrideAttrs (old: {
                    buildInputs = replaceHidapi (old.buildInputs or [ ]);
                    nativeBuildInputs = replaceHidapi (old.nativeBuildInputs or [ ]);
                  });
                }
              )
            );

        # Environment for developing everything in Octez.
        mainShell = pkgs.mkShell {
          name = "tezos-shell";

          inputsFrom = [
            depsScope.octez-deps
            depsScope.octez-dev-deps
          ];

          packages =
            with pkgs;
            [
              nixfmt
              autoconf
              cacert
              curl
              nodejs
              poetry
              pkgsFoundry.foundry
              jq
              rustup
              shellcheck
              shfmt
              taplo
              wabt
              xxd
              # For RISC-V kernel cross-compilation — use symlinkJoin to
              # strip nix-support setup hooks so the cross-compiler's CC
              # doesn't override the native one.
              (pkgs.symlinkJoin {
                name = "riscv64-cross-cc";
                paths = [ pkgsCross.riscv64.pkgsStatic.stdenv.cc ];
                postBuild = "rm -rf $out/nix-support";
              })
            ]
            ++ (
              if stdenv.isDarwin then
                [
                  fswatch
                  libiconv
                ]
              else
                [
                  inotify-tools
                ]
            );

          # $OPAM_SWITCH_PREFIX is used to find the ZCash parameters.
          OPAM_SWITCH_PREFIX = fakeOpamSwitchPrefix;

          # TODO: https://linear.app/tezos/issue/TZX-127/upgrade-rust-to-194
          # Allow duplicate symbols when linking multiple Rust static
          # libraries (liboctez_rust_deps + liboctez_libcrux_ml_dsa) that
          # each embed the Rust standard library's rust_eh_personality.
          NIX_LDFLAGS = pkgs.lib.optionalString pkgs.stdenv.isLinux "-z muldefs";

          # Clang with wasm32 target support for WASM kernel compilation.
          CC_wasm32_unknown_unknown = "${pkgs.llvmPackages.clang-unwrapped}/bin/clang";
        };

        # This scope contains all Opam packages defined in this repository.
        repoScope =
          opam.buildOpamProject'
            {
              inherit repos;
              recursive = false;
            }
            ./opam
            {
              octez-deps = "dev";
            };
      in
      {
        packages = repoScope;

        devShells.default = mainShell;

        apps.ci-check-version-sh-lock = {
          type = "app";
          program =
            let
              jq = "${pkgs.jq}/bin/jq";

              checkFlakeLock = pkgs.writeShellScript "check-flake-lock.sh" ''
                set -e

                inputPath=$1
                inputSpec=$2
                inputName=$3

                wantedPath=$(nix flake prefetch --quiet --json $inputSpec | ${jq} -r .storePath)

                # Compare the two Nix store paths, if they're different then our lock file
                # is out of date.
                if [[ "$wantedPath" != "$inputPath" ]]; then
                  echo "Input '$inputName' is out-of-date!"
                  echo

                  echo "Run the following to update your lock file."
                  echo
                  echo -e "\t nix flake lock --override-input '$inputName' '$inputSpec'"
                  echo

                  exit 1
                fi
              '';

              script = pkgs.writeShellScript "check-version.sh" ''
                set -e

                source ${self}/scripts/version.sh

                opam_repo_flake="github:ocaml/opam-repository/$opam_repository_tag"

                if ! ( ${checkFlakeLock} ${opam-repository} $opam_repo_flake opam-repository );
                then
                  nix flake lock \
                    --override-input opam-repository $opam_repo_flake \
                    2> /dev/null > /dev/null

                  echo Or copy the 'flake.lock' from CI artifacts.
                  echo

                  exit 1
                fi
              '';
            in
            builtins.toString script;
        };

        formatter = pkgs.nixfmt-tree;
      }
    )
    // {
      hydraJobs = {
        inherit (self) packages devShells;
      };
    };
}
