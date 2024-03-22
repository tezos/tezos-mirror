(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type for resources managed by a deployment. *)
type t

(** [deploy ?base_port ?ports_per_vm ~number_of_vms ~machine_type ()] deploys the
      expected number of vms. For each vm, we can specify a [base_port]
      which is the first port to be opened and [ports_per_vm] specify the
      number of opened port from the [base_port]. The promise returned by this
      function is fulfilled when all the vms are deployed. Consequently,
      it can take some times. *)
val deploy :
  ?base_port:int ->
  ?ports_per_vm:int ->
  number_of_vms:int ->
  localhost:bool ->
  unit ->
  t Lwt.t

(** [run_command t ~address cmd args] can run a command on the vm located at
        [address]. *)
val run_command :
  t -> address:string -> string -> string list -> (Process.t, string) Runnable.t

(** [get_points t] returns the points associated to the deployed machines. *)
val get_points : t -> (string * int) list Lwt.t

(** [next_port t] returns the next available port for the given point. *)
val next_port : t -> string * int -> int

(** [terminate ?exn t] should be called to tear down the machine. Do note
      that this call may or may not destroy the machine depending on
      what the user has chosen (see [Cli.destroy].*)
val terminate : ?exn:exn -> t -> unit Lwt.t
