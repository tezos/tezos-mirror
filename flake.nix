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
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

    # The Opam repository is one way how dependencies are pinned for Octez.
    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository";
    };

    # This library helps us build OCaml packages and dependencies.
    opam-nix = {
      # This is a fork with includes a fix for the nixpkgs overrides.
      url = "github:vapourismo/opam-nix/fix/no-broken-symlinks-hidapi";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      opam-repository,
      opam-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
        depsScope = opam.queryToScope { inherit repos; } {
          octez-deps = "dev";
          octez-dev-deps = "dev";
          ocamlformat-rpc = "*";
        };

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
              kaitai-struct-compiler
              nodejs
              poetry
              rustup
              shellcheck
              shfmt
              taplo
              wabt
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
