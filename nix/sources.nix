let
  opam-nix-integration = import (
    fetchTarball {
      url = "https://github.com/vapourismo/opam-nix-integration/archive/ec0f82a78958461e9ab16eb87c4ae1873c4d9f77.tar.gz";
      sha256 = "1kh6lz9cr5158m1mrl8hyj1brhi450hil65faqsrcw6nd3a1dang";
    }
  );

  rust-overlay = import (
    fetchTarball {
      url = "https://github.com/oxalica/rust-overlay/archive/e054ca37ee416efe9d8fc72d249ec332ef74b6d4.tar.gz";
      sha256 = "0vj6k40qqlrxqdr33xpha57mq6ajdzscd8wradzbg4p3mpy7j52c";
    }
  );

  pkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/cd07839e2e61f8b7c467f20a896c3f9e63a04918.tar.gz";
    sha256 = "1xr250f9z72v560pkvi25iwclnysjn8h9mw8cdnjl4izq0milmzi";
  };

  pkgs = import pkgsSrc {
    overlays = [opam-nix-integration.overlay rust-overlay];
  };

  riscv64Pkgs = import pkgsSrc {
    crossSystem.config = "riscv64-unknown-linux-gnu";
  };
in {
  inherit opam-nix-integration rust-overlay pkgs riscv64Pkgs;
}
