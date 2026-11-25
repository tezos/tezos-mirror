(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Publish the 'master' branch, and only the 'master' branch,
   from the private project to the public project.

   This replaces a GitLab push mirror, because push mirrors always push
   all tags.

   Authentication: [PUBLIC_MIRROR_TOKEN] is a project access token of the
   public project (Maintainer role, [write_repository] scope only), stored as
   a masked, hidden and protected variable of the private project, so that only
   pipelines of protected branches ('master') can read it.

   Never print environment variables in this job: group variables are visible
   to it too. *)

module CI = Cacio.Shared

let job_mirror_master =
  CI.job
    "public_mirror:master"
    ~__POS__
    ~stage:Publish
    ~description:
      (sf
         "Push the 'master' branch, and only this branch, to %s."
         Tezos_ci_pipelines.public_mirror_project_path)
    ~image:Tezos_ci.Images.Base_images.alpine_docker_ci
      (* Serialize pushes so that an older pipeline can never run after a
         newer one. *)
    ~resource_group:"public-mirror"
    ~variables:
      [
        (* Full history: a shallow clone cannot be pushed to a remote that
           lacks the missing commits. A fresh clone, because fetching into a
           shallow repository left by a previous job would keep it shallow. *)
        ("GIT_STRATEGY", "clone");
        ("GIT_DEPTH", "0");
      ]
      (* The token is passed through a credential helper so that it never
         appears in the remote URL, and thus in any git error message.
         The URL includes a user name on purpose: the runner rewrites
         https://gitlab.com/ (url.<...>.insteadOf) to use the job token of the
         private project, which cannot push to the public project.
         The empty credential.helper resets helpers configured by the runner.
         The explicit refspec pushes exactly one ref: no other branch, no tag.
         No --force: if the public 'master' has diverged, the push fails. *)
    ~script:
      [
        sf
          "git -c credential.helper= -c credential.helper='!f() { test \"$1\" \
           = get && echo username=mirror && echo \
           \"password=${PUBLIC_MIRROR_TOKEN}\"; }; f' push --no-follow-tags \
           https://mirror@gitlab.com/%s.git \
           \"${CI_COMMIT_SHA}:refs/heads/margebot-test\""
          Tezos_ci_pipelines.public_mirror_project_path;
      ]

let () =
  Cacio.register_jobs
    Tezos_ci_pipelines.master_branch_mirror
    [(Auto, job_mirror_master)]
