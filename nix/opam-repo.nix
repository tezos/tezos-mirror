{runCommand}: let
  opam-repository-rev = builtins.readFile (
    runCommand
    "opam-repo-rev"
    {
      src = ../scripts/version.sh;
    }
    ''
      . $src
      echo -n $full_opam_repository_tag > $out
    ''
  );
in
  fetchTarball "https://github.com/ocaml/opam-repository/archive/${opam-repository-rev}.tar.gz"
