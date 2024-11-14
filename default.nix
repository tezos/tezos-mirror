# WARNING!
# This file is provided as a courtesy and comes with no guarantees that it will
# continue to work in the future.
{sources ? import ./nix/sources.nix {}}: let
  pkgs = sources.pkgs;

  overlays = pkgs.callPackage ./nix/overlays.nix {};

  packageSet = pkgs.opamPackages.overrideScope' (pkgs.lib.composeManyExtensions [
    # Set the opam-repository which has the package descriptions.
    (final: prev: {
      repository = prev.repository.override {src = sources.opam-repository;};
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
    {
      buildInputs = with pkgs; [
        curl
        cacert
      ];
    }
    ''
      mkdir -p $out/share/zcash-params
      cp ${./images/ci/zcash-params}/* $out/share/zcash-params

      OPAM_SWITCH_PREFIX="$out" ${./scripts}/install_dal_trusted_setup.sh
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

  llvmPackages = pkgs.llvmPackages_16;

  libtoolAliasDarwin =
    # On Darwin we need a little help to bring `libtool` into scope.
    pkgs.runCommand "libtool-alias" {} ''
      mkdir -p $out/bin
      ln -s ${llvmPackages.bintools-unwrapped}/bin/llvm-libtool-darwin $out/bin/libtool
    '';
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

    buildInputs =
      packages
      ++ (with pkgs; [
        makeWrapper
        cacert

        # Bring Clang into scope in case the stdenv doesn't come with it already.
        llvmPackages.clang

        # This brings in things like llvm-ar which are needed for Rust WebAssembly
        # compilation on Mac. It isn't used by default. Configure the AR environment variable to
        # make rustc use it.
        # This package also brings objcopy, libtoool and ranlib which are used.
        llvmPackages.bintools
      ])
      ++ pkgs.lib.optional pkgs.stdenv.isDarwin libtoolAliasDarwin;

    # Disable OPAM usage in Makefile.
    TEZOS_WITHOUT_OPAM = true;

    # $OPAM_SWITCH_PREFIX is used to find the ZCash parameters.
    OPAM_SWITCH_PREFIX = fakeOpamSwitchPrefix;

    src = pkgs.lib.sources.cleanSourceWith {
      filter = name: type:
        if type == "directory"
        then name != "_build" && name != "target" && name != ".direnv"
        else true;
      src = pkgs.lib.sources.cleanSource ./.;
    };

    dontConfigure = true;
    dontCheck = true;

    buildPhase = ''
      CARGO_HOME=$TMPDIR/.cargo make experimental-release
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
