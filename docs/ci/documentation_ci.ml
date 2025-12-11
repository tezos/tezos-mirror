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
    let open Tezos_ci in
    match os_distribution with
    | `ubuntu_noble -> ("noble", Images.Base_images.ubuntu_noble)
    | `ubuntu_jammy -> ("jammy", Images.Base_images.ubuntu_jammy)
    | `debian_bookworm -> ("bookworm", Images.Base_images.debian_bookworm)
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
    ~cpu:Tezos_ci.Runner.CPU.Very_high
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
    ["eval $(opam env)"; "make -C docs " ^ target]

let job_manuals =
  Cacio.parameterize @@ fun executables_to_use ->
  CI.job
    "manuals"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Build
    ~description:
      "Build the command-line interface manuals (man pages) of Octez \
       executables."
    ~needs:
      (match executables_to_use with
      | `dynamic -> [(Artifacts, Tezos_ci_jobs.Kernels.job_build_kernels)]
      | `static -> [])
    ~needs_legacy:
      (match executables_to_use with
      | `dynamic ->
          (* It's ok to assume Before_merging here because we only care about the job name. *)
          [
            ( Artifacts,
              Tezos_ci_jobs.Code_verification.job_build_x86_64_release
                Before_merging );
            ( Artifacts,
              Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_dev
                Before_merging );
            ( Artifacts,
              Tezos_ci_jobs.Code_verification.job_build_x86_64_exp
                Before_merging );
          ]
      | `static -> [(Artifacts, Tezos_ci_jobs.Master_branch.job_static_x86_64)])
    ~only_if_changed:Files.odoc
    ~force_if_label:["ci--docs"]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Weeks 1))
         [
           "docs/*/octez-*.html";
           "docs/api/octez-*.txt";
           "docs/developer/metrics.csv";
           "docs/developer/rollup_metrics.csv";
           "docs/user/node-config.json";
         ])
    ("eval $(opam env)"
    ::
    (match executables_to_use with
    | `dynamic -> ["make -C docs -j octez-gen"]
    | `static -> ["scripts/ci/documentation:manuals_static.sh"]))

let job_docgen =
  CI.job
    "docgen"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Build
    ~description:
      "Build various generated reference material. This includes the RPC, P2P \
       and error reference."
    ~cpu:Tezos_ci.Runner.CPU.Very_high
    ~only_if_changed:Files.odoc
    ~force_if_label:["ci--docs"]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Weeks 1))
         [
           "docs/alpha/rpc.rst";
           "docs/shell/rpc.rst";
           "docs/user/default-acl.json";
           "docs/api/errors.rst";
           "docs/shell/p2p_api.rst";
         ])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["eval $(opam env)"; "make -C docs -j docexes-gen"]

let job_build_all =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun executables_to_use ->
  CI.job
    "build_all"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Build
    ~description:"Build the RSTs. Include material from previous build jobs."
    ~only_if_changed:Files.odoc
    ~force_if_label:["ci--docs"]
    ~needs:
      [
        (Artifacts, job_odoc mode);
        (Artifacts, job_manuals executables_to_use);
        (Artifacts, job_docgen);
      ]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expose_as:"Documentation - excluding old protocols"
         ~expire_in:(Duration (Weeks 1))
         (* Path must be terminated with / to expose artifact (gitlab-org/gitlab#/36706) *)
         ["docs/_build/"])
    [
      "eval $(opam env)";
      ". $HOME/.venv/bin/activate";
      "make -C docs -j sphinx";
      "make -C docs -j _build/octezdoc.txt";
    ]

let job_linkcheck =
  Cacio.parameterize @@ fun mode executables_to_use ->
  CI.job
    "linkcheck"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Test
    ~description:"Check links in the documentation."
    ~only_if_changed:Files.odoc
    ~force_if_label:["ci--docs"]
    ~needs:
      [
        (Artifacts, job_manuals executables_to_use);
        (Artifacts, job_docgen);
        (Artifacts, job_build_all mode executables_to_use);
      ]
    ~allow_failure:Yes
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      ". $HOME/.venv/bin/activate";
      "make -C docs redirectcheck";
      "make -C docs linkcheck";
    ]

let job_publish =
  CI.job
    "publish"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test
    ~stage:Publish
    ~description:"Publish the documentation to octez.com/docs."
    ~needs:[(Artifacts, job_build_all `full `dynamic)]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "eval $(opam env)";
      ". $HOME/.venv/bin/activate";
      {|echo "${CI_PK_GITLAB_DOC}" > ~/.ssh/id_ed25519|};
      {|echo "${CI_KH}" > ~/.ssh/known_hosts|};
      "chmod 400 ~/.ssh/id_ed25519";
      "./scripts/ci/doc_publish.sh";
    ]

let register () =
  CI.register_merge_request_jobs
    [
      (Immediate, job_rst_check);
      (Auto, job_install_python `debian_bookworm `current_branch);
      (Auto, job_build_all `lite `dynamic);
      (Manual, job_linkcheck `lite `dynamic);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for the documentation."
    ~legacy_jobs:
      [
        Tezos_ci_jobs.Code_verification.job_build_x86_64_release
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_dev
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_exp
          Schedule_extended_test;
      ]
    [
      (Auto, job_rst_check);
      (Auto, job_install_python `ubuntu_noble `master);
      (Auto, job_install_python `ubuntu_jammy `master);
      (Auto, job_install_python `debian_bookworm `master);
      (Auto, job_build_all `lite `dynamic);
      (Auto, job_linkcheck `lite `dynamic);
    ] ;
  CI.register_scheduled_pipeline
    "update"
    ~description:
      "Generate and push the documentation to octez.com/docs without being \
       interrupted."
    ~legacy_jobs:
      [
        Tezos_ci_jobs.Code_verification.job_build_x86_64_release
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_extra_dev
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_exp
          Schedule_extended_test;
      ]
    [(Auto, job_publish)] ;
  ()
