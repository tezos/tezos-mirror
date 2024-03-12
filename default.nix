# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
let
  sources = import ./nix/sources.nix;
  pkgs = sources.pkgs;

  overlays = pkgs.callPackage ./nix/overlays.nix {};
  tezos-opam-repository = pkgs.callPackage ./nix/tezos-opam-repo.nix {};
  opam-repository = pkgs.callPackage ./nix/opam-repo.nix {};

  packageSet = pkgs.opamPackages.overrideScope' (pkgs.lib.composeManyExtensions [
    # Set the opam-repository which has the package descriptions.
    (final: prev: {
      repository = prev.repository.override {src = opam-repository;};
    })

    # First overlay simply picks the package versions from Tezos'
    # opam-repository.
    overlays.pick-latest-packages

    # Tweak common packages.
    overlays.common-overlay

    # Overlays for MacOS
    (
      if pkgs.stdenv.isDarwin
      then overlays.darwin-overlay
      else final: prev: {}
    )

    # Tweak the dependencies.
    overlays.fix-rust-packages
  ]);

  packages =
    builtins.filter
    pkgs.lib.attrsets.isDerivation
    (builtins.attrValues packageSet);

  packageLibDirs = builtins.filter builtins.pathExists (
    builtins.map (package: "${package}/lib/${package.pname}") packages
  );

  packageIncludeArgs = builtins.map (dir: "-I${dir}") packageLibDirs;

  fakeOpamSwitchPrefix =
    pkgs.runCommand
    "fake-opam-switch-prefix"
    {}
    ''
      mkdir -p $out/share/zcash-params
      cp ${tezos-opam-repository}/zcash-params/sapling-output.params $out/share/zcash-params
      cp ${tezos-opam-repository}/zcash-params/sapling-spend.params $out/share/zcash-params
    '';

  mkFrameworkFlags = frameworks:
    pkgs.lib.concatStringsSep " " (
      pkgs.lib.concatMap
      (
        framework: [
          "-F${pkgs.darwin.apple_sdk.frameworks.${framework}}/Library/Frameworks"
          "-framework ${framework}"
        ]
      )
      frameworks
    );
in
  pkgs.stdenv.mkDerivation {
    name = "tezos";

    NIX_LDFLAGS = pkgs.lib.optional pkgs.stdenv.isDarwin (
      mkFrameworkFlags [
        "CoreFoundation"
        "IOKit"
        "AppKit"
        "Security"
      ]
    );

    NIX_CFLAGS_COMPILE =
      # Silence errors (-Werror) for unsupported flags on MacOS.
      pkgs.lib.optionals
      pkgs.stdenv.isDarwin
      ["-Wno-unused-command-line-argument"]
      ++
      # Make sure headers files are in scope.
      packageIncludeArgs;

    hardeningDisable = ["stackprotector"];

    buildInputs = packages ++ [pkgs.makeWrapper];

    # Disable OPAM usage in Makefile.
    TEZOS_WITHOUT_OPAM = true;

    # $OPAM_SWITCH_PREFIX is used to find the ZCash parameters.
    OPAM_SWITCH_PREFIX = fakeOpamSwitchPrefix;

    src = pkgs.lib.sources.cleanSourceWith {
      filter = name: type:
        if type == "directory"
        then name != "_build" && name != "target"
        else true;
      src = pkgs.lib.sources.cleanSource ./.;
    };

    dontConfigure = true;
    dontCheck = true;

    buildPhase = ''
      make experimental-release
    '';

    installPhase = ''
      mkdir -p $out/bin
      find . -maxdepth 1 -iname 'octez-*' -type f -executable -exec cp {} $out/bin \;
    '';

    postFixup = ''
      for file in $(find $out/bin -type f); do
        wrapProgram $file --set OPAM_SWITCH_PREFIX ${fakeOpamSwitchPrefix}
      done
    '';

    passthru = {
      ocamlVersion = packageSet.ocaml.version;
    };
  }
