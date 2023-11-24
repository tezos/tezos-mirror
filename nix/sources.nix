let
  opam-nix-integration = import (
    fetchTarball {
      url = "https://github.com/vapourismo/opam-nix-integration/archive/567369e90513019423db3462b20a01333cee2a04.tar.gz";
      sha256 = "0siwzi71bbjkzqdnpl8pqz8zyfzkyw908bbqjdmafm344p9vnckh";
    }
  );

  pkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/cd07839e2e61f8b7c467f20a896c3f9e63a04918.tar.gz";
    sha256 = "1xr250f9z72v560pkvi25iwclnysjn8h9mw8cdnjl4izq0milmzi";
  };

  pkgs = import pkgsSrc {
    overlays = [opam-nix-integration.overlay];
  };

  riscv64Pkgs = import pkgsSrc {
    crossSystem.config = "riscv64-unknown-linux-gnu";
  };
in {
  inherit opam-nix-integration pkgs riscv64Pkgs;
}
