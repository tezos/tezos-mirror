(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [debian_repository] child
   pipeline.

   This pipeline builds the old and old Debian (and Ubuntu)
   packages. *)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache
open Common.Packaging
open Common.Helpers

let build_debian_packages_image =
  Image.mk_external
    ~image_path:"$DEP_IMAGE:${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"

(* the name of the image depends on the variables in the parallel matrix.
   we delcare it once, but we use it in two different contexts *)
let systemd_test_debian_packages_image = build_debian_packages_image

let tag_amd64 ~ramfs =
  if ramfs then Runner.Tag.show Gcp_very_high_cpu_ramfs
  else Runner.Tag.show Gcp_very_high_cpu

let tag_arm64 = Runner.Tag.show Gcp_arm64

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job
    [job_build_debian_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let debian_package_release_matrix ?(ramfs = false) = function
  | Partial ->
      [[("RELEASE", ["bookworm"; "trixie"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full ->
      [
        [
          ("RELEASE", ["unstable"; "bookworm"; "trixie"]);
          ("TAGS", [tag_amd64 ~ramfs; tag_arm64]);
        ];
      ]
  | Release ->
      [
        [
          ("RELEASE", ["bookworm"; "trixie"]);
          ("TAGS", [tag_amd64 ~ramfs; tag_arm64]);
        ];
      ]

(** These are the set of Ubuntu release-architecture combinations for
    which we build deb packages in the job
    [job_build_ubuntu_package]. See {!debian_package_release_matrix}
    for more information.

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let ubuntu_package_release_matrix ?(ramfs = false) = function
  | Partial -> [[("RELEASE", ["jammy"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full | Release ->
      [
        [
          ("RELEASE", ["noble"; "jammy"]);
          ("TAGS", [tag_amd64 ~ramfs; tag_arm64]);
        ];
      ]

(* Push .deb artifacts to storagecloud apt repository. *)
let make_job_apt_repo ?rules ~__POS__ ~name ?(stage = Stages.publish)
    ?dependencies ~prefix ~variables ?retry ~image script : tezos_job =
  let variables =
    variables @ [("GNUPGHOME", "$CI_PROJECT_DIR/.gnupg")] @ [("PREFIX", prefix)]
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage
    ~name
    ~id_tokens:Tezos_ci.id_tokens
    ~image
    ~tag:Gcp_not_interruptible
    ?retry
    ~before_script:
      (before_script
         ~source_version:true
         ["apt-get install -y --update apt-utils debsigs"])
    ~variables
    script

(* The entire Debian packages pipeline. When [pipeline_type] is [Before_merging]
   we test only on Debian stable. Returns a triplet, the first element is
   the list of all jobs, the second is the job building ubuntu packages artifats
   and the third debian packages artifacts *)
let jobs ?(limit_dune_build_jobs = false) pipeline_type =
  let variables ?(kind = "build") add =
    ("FLAVOUR", kind)
    :: ( "DEP_IMAGE",
         "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/$FLAVOUR-$DISTRIBUTION-$RELEASE"
       )
       (* this second variable is for a read only registry and we want it to be
          tezos/tezos *)
    :: ( "DEP_IMAGE_PROTECTED",
         "${GCP_PROTECTED_REGISTRY}/tezos/tezos/$FLAVOUR-$DISTRIBUTION-$RELEASE"
       )
    :: add
  in
  let make_job_docker_systemd_tests ~__POS__ ~name ~matrix ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:
        (variables
           ~kind:"systemd-tests"
           [
             ("DISTRIBUTION", distribution);
             ( "BASE_IMAGE",
               Images.Base_images.(
                 sf
                   "%s/$DISTRIBUTION:$RELEASE-%a"
                   path_prefix
                   version_pp
                   debian_version) );
           ])
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
      [
        "./scripts/ci/build-packages-dependencies.sh \
         images/packages/debian-systemd-tests.Dockerfile";
      ]
  in
  let job_docker_systemd_test_debian_dependencies : tezos_job =
    make_job_docker_systemd_tests
      ~__POS__
      ~name:"oc.docker-systemd_tests_debian"
      ~distribution:"debian"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_docker_systemd_test_ubuntu_dependencies : tezos_job =
    make_job_docker_systemd_tests
      ~__POS__
      ~name:"oc.docker-systemd_tests_ubuntu"
      ~distribution:"ubuntu"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in

  let make_job_docker_build_debian_dependencies ~__POS__ ~name ~matrix
      ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:
        (variables
           [
             ("DISTRIBUTION", distribution);
             ( "BASE_IMAGE",
               Images.Base_images.(
                 sf
                   "%s/$DISTRIBUTION:$RELEASE-%a"
                   path_prefix
                   version_pp
                   debian_version) );
           ])
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
      [
        "./scripts/ci/build-packages-dependencies.sh \
         images/packages/debian-deps-build.Dockerfile";
      ]
  in
  let job_docker_build_debian_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-debian-dependencies"
      ~distribution:"debian"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_docker_build_ubuntu_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-ubuntu-dependencies"
      ~distribution:"ubuntu"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in
  let make_job_build_debian_packages ~__POS__ ?timeout ~name ~matrix
      ~distribution ~script ~dependencies () =
    job
      ~__POS__
      ~name
      ~image:build_debian_packages_image
      ~stage:Stages.build
      ~variables:
        (variables
           (("DISTRIBUTION", distribution)
           ::
           (if limit_dune_build_jobs then [("DUNE_BUILD_JOBS", "-j 12")] else [])
           ))
      ~parallel:(Matrix matrix)
      ~dependencies
      ?timeout
      ~tag:Dynamic
      ~artifacts:(artifacts ["packages/$DISTRIBUTION/$RELEASE"])
      [
        (* This is an hack to enable Cargo networking for jobs in child pipelines.

           There is an weird gotcha with how variables are passed to
           child pipelines. Global variables of the parent pipeline
           are passed to the child pipeline. Inside the child
           pipeline, variables received from the parent pipeline take
           precedence over job-level variables. It's bit strange. So
           to override the default [CARGO_NET_OFFLINE=true], we cannot
           just set it in the job-level variables of this job.

           [enable_sccache] adds the cache directive for [$CI_PROJECT_DIR/_sccache].

           See
           {{:https://docs.gitlab.com/ee/ci/variables/index.html#cicd-variable-precedence}here}
           for more info. *)
        "export CARGO_NET_OFFLINE=false";
        script;
      ]
    |> enable_sccache
  in

  (* data packages. we build them once *)
  let job_build_data_packages : tezos_job =
    job
      ~__POS__
      ~name:"oc.build-data_packages"
      ~image:build_debian_packages_image
      ~stage:Stages.build
      ~variables:
        (variables
           [("DISTRIBUTION", "debian"); ("RELEASE", "trixie"); ("TAGS", "gcp")])
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~tag:Dynamic
      ~artifacts:(artifacts ["packages/$DISTRIBUTION/$RELEASE"])
      [
        "export CARGO_NET_OFFLINE=false";
        "./scripts/ci/build-debian-packages.sh zcash";
      ]
  in
  (* These jobs build the packages in a matrix using the
     build dependencies images *)
  let job_build_debian_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-debian"
      ~distribution:"debian"
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~script:"./scripts/ci/build-debian-packages.sh binaries"
      ~matrix:(debian_package_release_matrix ~ramfs:true pipeline_type)
      ()
  in
  let job_build_ubuntu_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-ubuntu"
      ~distribution:"ubuntu"
      ~dependencies:(Dependent [Job job_docker_build_ubuntu_dependencies])
      ~script:"./scripts/ci/build-debian-packages.sh binaries"
      ~matrix:(ubuntu_package_release_matrix ~ramfs:true pipeline_type)
      ()
  in

  (* These jobs create the apt repository for the packages *)
  let job_apt_repo_debian =
    make_job_apt_repo
      ~__POS__
      ~name:"apt_repo_debian"
      ~prefix:""
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_debian_package;
             Artifacts job_build_data_packages;
           ])
      ~variables:(archs_variables pipeline_type)
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~image:Images.Base_images.debian_trixie
      ["./scripts/ci/create_debian_repo.sh debian bookworm trixie"]
  in
  let job_apt_repo_ubuntu =
    make_job_apt_repo
      ~__POS__
      ~name:"apt_repo_ubuntu"
      ~prefix:""
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_ubuntu_package;
             Artifacts job_build_data_packages;
           ])
      ~variables:(archs_variables pipeline_type)
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~image:Images.Base_images.ubuntu_noble
      ["./scripts/ci/create_debian_repo.sh ubuntu noble jammy"]
  in
  (* These test the installability of the old packages *)
  let job_install_bin ~__POS__ ~name ~dependencies ~image ?(variables = [])
      ?allow_failure ?before_script script =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~variables
      ~stage:Stages.publishing_tests
      ?before_script
      script
  in
  let job_install_systemd_bin ~__POS__ ~name ~dependencies ?(variables = [])
      ?allow_failure script =
    job_docker_authenticated
      ?allow_failure
      ~__POS__
      ~name
      ~dependencies
      ~variables
      ~stage:Stages.publishing_tests
      script
  in

  let job_lintian ~__POS__ ~name ~dependencies ?(variables = []) ~image
      ?allow_failure script =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~stage:Stages.publishing_tests
      ~variables
      ~before_script:
        (before_script
           ~source_version:true
           [
             "export DEBIAN_FRONTEND=noninteractive";
             "apt-get update";
             "apt-get install lintian parallel -y";
           ])
      script
  in
  let test_ubuntu_packages_jobs =
    (* in merge pipelines we tests only debian. ubuntu packages
       are built and tested in the scheduled pipelines*)
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_ubuntu"
        ~dependencies:(Dependent [Artifacts job_build_ubuntu_package])
        ~image:Images.Base_images.ubuntu_noble
        ["./scripts/ci/lintian_debian_packages.sh ubuntu jammy noble"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubunty_jammy"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.ubuntu_jammy
        ["./docs/introduction/install-bin-deb.sh ubuntu jammy"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubunty_noble"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.ubuntu_noble
        ["./docs/introduction/install-bin-deb.sh ubuntu noble"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_noble_systemd"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_ubuntu_dependencies;
               Job job_apt_repo_ubuntu;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [("PREFIX", ""); ("DISTRIBUTION", "ubuntu"); ("RELEASE", "noble")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/install-bin-deb.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_ubuntu_jammy_systemd_test"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_ubuntu_dependencies;
               Job job_apt_repo_ubuntu;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [("PREFIX", ""); ("DISTRIBUTION", "ubuntu"); ("RELEASE", "jammy")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_ubuntu_noble_systemd_test"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_ubuntu_dependencies;
               Job job_apt_repo_ubuntu;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [("PREFIX", ""); ("DISTRIBUTION", "ubuntu"); ("RELEASE", "noble")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
    ]
  in
  let test_debian_packages_jobs =
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_debian"
        ~dependencies:(Dependent [Artifacts job_build_debian_package])
        ~image:Images.Base_images.debian_bookworm
        ["./scripts/ci/lintian_debian_packages.sh debian bookworm"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm"
        ~dependencies:(Dependent [Job job_apt_repo_debian])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.debian_bookworm
        ["./docs/introduction/install-bin-deb.sh debian bookworm"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm_systemd_test"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_debian_dependencies;
               Job job_apt_repo_debian;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [
               ("PREFIX", "");
               ("DISTRIBUTION", "debian");
               ("RELEASE", "bookworm");
             ])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/install-bin-deb.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_debian_bookworm-systemd"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_debian_dependencies;
               Job job_apt_repo_debian;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [
               ("PREFIX", "");
               ("DISTRIBUTION", "debian");
               ("RELEASE", "bookworm");
             ])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
    ]
  in
  let debian_jobs =
    [
      job_docker_build_debian_dependencies;
      job_build_debian_package;
      job_build_data_packages;
      job_apt_repo_debian;
    ]
  in
  let ubuntu_jobs =
    [
      job_docker_build_ubuntu_dependencies;
      job_build_ubuntu_package;
      job_apt_repo_ubuntu;
    ]
  in
  match pipeline_type with
  | Partial ->
      (job_docker_systemd_test_debian_dependencies :: debian_jobs)
      @ test_debian_packages_jobs
  | Full ->
      job_docker_systemd_test_debian_dependencies
      :: job_docker_systemd_test_ubuntu_dependencies :: debian_jobs
      @ ubuntu_jobs @ test_debian_packages_jobs @ test_ubuntu_packages_jobs
  | Release -> debian_jobs @ ubuntu_jobs

let register ~auto ~description pipeline_type =
  let pipeline_name =
    match (pipeline_type, auto) with
    | Partial, false -> "debian_repository_partial"
    | Partial, true -> "debian_repository_partial_auto"
    | Full, _ -> "debian_repository_full"
    | Release, _ -> "debian_repository_release"
  in
  let jobs = jobs pipeline_type in
  Pipeline.register_child
    pipeline_name
    ~description
    ~jobs:(job_datadog_pipeline_trace :: jobs)

let child_pipeline_partial =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Debian stable .deb packages."
    ~auto:false
    Partial

let child_pipeline_partial_auto =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Debian stable .deb packages. Starts automatically on certain \
       conditions."
    ~auto:true
    Partial
