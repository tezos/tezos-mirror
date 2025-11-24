(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type for resources managed by a deployment. *)
type t

(** [deploy ?dockerbuild_args ~configurations ()] deploys the expected number of
    vms.  For each vm, we can specify a [base_port] which is the first port to
    be opened and [ports_per_vm] specifies the number of opened ports from the
    [base_port]. [dockerbuild_args] can be used to provide arguments that should
    be used to build images for all VMs. The promise returned by this function
    is fulfilled when all the vms are deployed. Consequently, it can take some
    time. *)
val deploy :
  ?dockerbuild_args:(string * string) list ->
  configurations:Agent.Configuration.t list ->
  unit ->
  t Lwt.t

(** [get_agents t] returns the list of agents deployed. *)
val agents : t -> Agent.t list

(** [terminate ?exn env t] should be called to tear down the machine. Do note
    that this call may or may not destroy the machine depending on
    what the user has chosen (see [Cli.destroy]. *)
val terminate : ?exn:exn -> t -> unit Lwt.t

val of_agents : Agent.t list -> t
