(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Files = struct
  let rst = ["**/*.rst"]

  let python_install_script =
    [
      "docs/developer/install-python-debian-ubuntu.sh";
      "pyproject.toml";
      "poetry.lock";
    ]

  (* Copied from changeset_octez_docs in common.ml,
     which will be removed once all documentation jobs are migrated to Cacio.
     In particular, the TODO was copy-pasted as-is, it does not come from this MR. *)
  let odoc =
    [
      (* TODO: refine scripts. *)
      "scripts/**/*/";
      "script-inputs/**/*/";
      "src/**/*.ml*";
      "tezt/**/*.ml*";
      "brassaia/**/*.ml*";
      "irmin/**/*.ml*";
      "client-libs/**/*.ml*";
      "etherlink/**/*.ml*";
      "data-encoding/**/*.ml*";
      "vendors/**/*.ml*";
      "dune";
      "dune-project";
      "dune-workspace";
      "**/*.rst";
      (* Nota: stays as it is, many non-rst files in this folder *)
      "docs/**/*";
      "grafazos/doc/**/*";
    ]

  let all = rst @ python_install_script @ odoc
end

module CI = Cacio.Make (struct
  let name = "documentation"

  let paths = Files.all
end)

let job_rst_check =
  CI.job
    "rst-check"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test_master
    ~stage:Test
    ~description:"Check ReStructured Text files."
    ~only_if_changed:Files.rst
    [". $HOME/.venv/bin/activate"; "make --silent -C docs sphinx-check"]

let job_install_python =
  Cacio.parameterize @@ fun os_distribution ->
  Cacio.parameterize @@ fun branch ->
  let os_distribution_name, image =
    match os_distribution with
    | `ubuntu_noble -> ("noble", Tezos_ci.Images.ubuntu_noble)
    | `ubuntu_jammy -> ("jammy", Tezos_ci.Images.ubuntu_jammy)
    | `debian_bookworm -> ("bookworm", Tezos_ci.Images.debian_bookworm)
  in
  let project, branch =
    match branch with
    | `master -> ("tezos/tezos", "master")
    | `current_branch ->
        ( "${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}",
          "${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}" )
  in
  CI.job
    ("install_python_" ^ os_distribution_name)
    ~__POS__
    ~image
    ~stage:Test
    ~description:
      "Check that the Python installation script from the documentation \
       actually work."
    ~only_if_changed:Files.python_install_script
    ~force_if_label:["ci--docs"]
    [sf "./docs/developer/install-python-debian-ubuntu.sh %s %s" project branch]

let job_odoc =
  Cacio.parameterize @@ fun mode ->
  let target = match mode with `lite -> "odoc-lite" | `full -> "odoc" in
  CI.job
    "odoc"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Build
    ~description:
      ("Build the documentation of our OCaml libraries (make -C docs " ^ target
     ^ ").")
    ~only_if_changed:Files.odoc
    ~force_if_label:["ci--docs"]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~when_:Always
         ~expire_in:(Duration (Hours 4))
         (* Path must be terminated with / to expose artifact (gitlab-org/gitlab#/36706) *)
         ["docs/_build/api/odoc/"; "docs/odoc.log"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "eval $(opam env)";
      "export OPAMFETCH='wget'";
      "opam remote add default https://opam.ocaml.org/";
      "opam repo add archive \
       git+https://github.com/ocaml/opam-repository-archive";
      "opam update";
      "opam install --yes odoc.2.4.4";
      "make -C docs " ^ target;
    ]

let register () =
  CI.register_before_merging_jobs
    [
      (Immediate, job_rst_check);
      (Auto, job_install_python `debian_bookworm `current_branch);
      (Auto, job_odoc `lite);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for the documentation."
    [
      (Auto, job_rst_check);
      (Auto, job_install_python `ubuntu_noble `master);
      (Auto, job_install_python `ubuntu_jammy `master);
      (Auto, job_install_python `debian_bookworm `master);
      (Auto, job_odoc `lite);
    ] ;
  CI.register_scheduled_pipeline
    "update"
    ~description:
      "Generate and push the documentation to octez.com/docs without being \
       interrupted."
    [(Auto, job_odoc `full)] ;
  ()
