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
    ~needs_legacy:
      (match executables_to_use with
      | `dynamic ->
          (* The legacy build jobs are defined deep in code_verification.ml
             and it is not easy to extract them.
             For now, we thus define a dummy job with the same name,
             since CIAO only cares about the name of the dependency.
             Note that CIAO will still check that those dependencies are in the pipeline.
             -
             This trick only works for global pipelines that already have those jobs.
             Scheduled pipelines registered by Cacio
             (such as documentation.daily and documentation.update)
             would need to include them with [~legacy_jobs].
             Which would bring us back to the original issue which is that those jobs
             are defined deep in code_verification.ml.
             -
             Fortunately, for this job (documentation.manuals), we can depend on
             the job that builds static executables instead.
             This job is already defined at toplevel as [Master_branch.job_static_x86_64].
             So it is easy to include it with [~legacy_jobs].
             -
             However, the static build job is slower than the regular build jobs.
             This is not an issue in scheduled pipelines, but for merge request pipelines,
             being fast matters. Hence the dummy job trick, which works because
             merge request pipelines already include the regular build jobs. *)
          let dummy name =
            Tezos_ci.job ~name ~stage:Tezos_ci.Stages.build ~__POS__ []
          in
          [
            (Artifacts, dummy "oc.build_x86_64-released");
            (Artifacts, dummy "oc.build_amd64-extra-dev");
            (Artifacts, dummy "oc.build_amd64-extra-exp");
            (Artifacts, dummy "oc.build_kernels");
          ]
      | `static -> [(Artifacts, Master_branch.job_static_x86_64)])
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
    ~needs:[(Artifacts, job_build_all `full `static)]
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
  CI.register_before_merging_jobs
    [
      (Immediate, job_rst_check);
      (Auto, job_install_python `debian_bookworm `current_branch);
      (Auto, job_build_all `lite `dynamic);
      (Manual, job_linkcheck `lite `dynamic);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for the documentation."
    ~legacy_jobs:[Master_branch.job_static_x86_64]
    [
      (Auto, job_rst_check);
      (Auto, job_install_python `ubuntu_noble `master);
      (Auto, job_install_python `ubuntu_jammy `master);
      (Auto, job_install_python `debian_bookworm `master);
      (Auto, job_build_all `lite `static);
      (Auto, job_linkcheck `lite `static);
    ] ;
  CI.register_scheduled_pipeline
    "update"
    ~description:
      "Generate and push the documentation to octez.com/docs without being \
       interrupted."
    ~legacy_jobs:[Master_branch.job_static_x86_64]
    [(Auto, job_publish)] ;
  ()
