let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  buildInputs = [
    # Rust
    pkgs.rustup

    # Python
    pkgs.python39
    pkgs.yapf

    # Kotlin
    pkgs.kotlin
    pkgs.ktlint
    pkgs.gradle
  ];
}
