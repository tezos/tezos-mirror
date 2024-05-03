# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cd07839e2e61f8b7c467f20a896c3f9e63a04918";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix-integration.url = "github:vapourismo/opam-nix-integration";
    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository";
    };
    tezos-opam-repository = {
      flake = false;
      url = "gitlab:tezos/opam-repository";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    opam-nix-integration,
    opam-repository,
    tezos-opam-repository,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          overlays = [opam-nix-integration.overlays.default];
          inherit system;
        };

        sources = {
          inherit pkgs opam-repository tezos-opam-repository;

          riscv64Pkgs = import nixpkgs {
            crossSystem.config = "riscv64-unknown-linux-gnu";
            inherit system;
          };
        };
      in {
        packages.default = import ./default.nix {inherit sources;};

        devShells.default = import ./shell.nix {inherit sources;};

        apps.ci-check-version-sh-lock = {
          type = "app";
          program = let
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

              opam_repo_flake="github:ocaml/opam-repository/$full_opam_repository_tag"
              tezos_opam_repo_flake="gitlab:tezos/opam-repository/$opam_repository_tag"

              if ! ( ${checkFlakeLock} ${opam-repository} $opam_repo_flake opam-repository && \
                     ${checkFlakeLock} ${tezos-opam-repository} $tezos_opam_repo_flake tezos-opam-repository );
              then
                nix flake lock \
                  --override-input opam-repository $opam_repo_flake \
                  --override-input tezos-opam-repository $tezos_opam_repo_flake \
                  2> /dev/null > /dev/null

                echo Or copy the 'flake.lock' from CI artifacts.
                echo

                exit 1
              fi
            '';
          in
            builtins.toString script;
        };
      }
    )
    // {
      hydraJobs = {
        inherit (self) packages devShells;
      };
    };

  nixConfig = {
    extra-substituters = ["https://nix.cache.hwlium.com"];
    extra-trusted-public-keys = [
      "nix.cache.ole.run-1:4r2pTlyRkYixibZKLNlSbHL1tbxHZGAnsKMFbz1Oz3s="
      "nix.cache.hwlium.com:M57rk9haJRNFiNUA+6sF6ogbIVg4k8XrKpf5QSohBEA="
      "nix.cache.hwlium.com-2:mFFtk/Pvh/mrCJ7DHOY9mf769A/Nth97WFXMPMy6BGw="
    ];
  };
}
