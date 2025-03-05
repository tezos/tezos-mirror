let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell.override {
  inherit (pkgs.swift) stdenv;
} {
  buildInputs = [
    # Rust
    pkgs.rustup

    # Swift
    pkgs.swift
    pkgs.swiftpm
    pkgs.swift-format
    pkgs.swiftPackages.Foundation
    pkgs.swiftPackages.XCTest

    # Python
    pkgs.python39
    pkgs.python39Packages.pytest
    pkgs.yapf

    # Kotlin
    pkgs.kotlin
    pkgs.ktlint
    pkgs.gradle
  ];

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.swiftPackages.Dispatch
    pkgs.swiftPackages.Foundation
    pkgs.swiftPackages.XCTest
  ];
}
