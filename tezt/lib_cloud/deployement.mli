(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type for resources managed by a deployment. *)
type t

(** [deploy ?base_port ?ports_per_vm ~max_run_duration ~configuration ~localhost ()] deploys the
      expected number of vms. For each vm, we can specify a [base_port]
      which is the first port to be opened and [ports_per_vm] specify the
      number of opened port from the [base_port]. The promise returned by this
      function is fulfilled when all the vms are deployed. Consequently,
      it can take some times. *)
val deploy :
  ?base_port:int ->
  ?ports_per_vm:int ->
  configurations:Configuration.t list ->
  localhost:bool ->
  unit ->
  t Lwt.t

(** [get_agents t] returns the list of agents deployed. *)
val agents : t -> Agent.t list

val get_configuration : t -> Agent.t -> Configuration.t

(** [run_vm_command t ~address cmd args] can run a command on the vm located at
    [address]. This is different from running a commend on the agent directly
    since the agent runs on a docker image. *)
val run_vm_command :
  t -> Agent.t -> string -> string list -> (Process.t, string) Runnable.t

(** [terminate ?exn t] should be called to tear down the machine. Do note
      that this call may or may not destroy the machine depending on
      what the user has chosen (see [Cli.destroy].*)
val terminate : ?exn:exn -> t -> unit Lwt.t
