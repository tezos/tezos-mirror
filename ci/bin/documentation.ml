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

  let all = rst @ python_install_script
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

let register () =
  CI.register_before_merging_jobs
    [
      (Immediate, job_rst_check);
      (Auto, job_install_python `debian_bookworm `current_branch);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for the documentation."
    [
      (Auto, job_rst_check);
      (Auto, job_install_python `ubuntu_noble `master);
      (Auto, job_install_python `ubuntu_jammy `master);
      (Auto, job_install_python `debian_bookworm `master);
    ] ;
  ()
