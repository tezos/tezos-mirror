# SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT
#
# This file makes the Nix Flake available to non-Flake environments.
# See https://github.com/edolstra/flake-compat for more.
let
  endsWith =
    suffix: str:
    let
      found = builtins.substring (
        builtins.stringLength str - builtins.stringLength suffix
      ) (builtins.stringLength suffix) str;
    in
    found == suffix;

  flakeLock = builtins.fromJSON (builtins.readFile ../flake.lock);

  flakeCompat = (
    fetchTarball {
      url =
        flakeLock.nodes.flake-compat.locked.url
          or "https://github.com/edolstra/flake-compat/archive/${flakeLock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = flakeLock.nodes.flake-compat.locked.narHash;
    }
  );
in
import flakeCompat {
  src = builtins.filterSource (
    path: type:
    # Nix stuff
    endsWith "/nix" path
    || endsWith ".nix" path
    || endsWith "/flake.lock" path
    # Opam files
    || endsWith "/opam" path
    || endsWith "/opam/virtual" path
    || endsWith ".opam.locked" path
    || endsWith ".opam" path
    # ZCash parameters
    || endsWith "/images" path
    || endsWith "/images/ci" path
    || endsWith "/images/ci/zcash-params" path
    || endsWith ".params" path
  ) ../.;
}
