# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix-integration = {
      url = "github:vapourismo/opam-nix-integration";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository";
    };
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    opam-nix-integration,
    opam-repository,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          overlays = [
            opam-nix-integration.overlays.default
            rust-overlay.overlays.default
          ];
          inherit system;
        };

        sources = {
          inherit pkgs opam-repository;
        };
      in {
        ocamlDependencies = import ./nix/package-set.nix {inherit sources;};

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
      }
    )
    // {
      hydraJobs = {
        inherit (self) packages devShells;
      };
    };
}
