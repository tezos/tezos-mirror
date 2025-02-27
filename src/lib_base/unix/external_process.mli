(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Functor used to create a module that can launch and communicate with an
    external process dedicated to handling certain requests.  *)
module Make (P : External_process_parameters.S) : sig
  (** The type of external processes. *)
  type t

  (** [restart_hypervisee p] asks the hypervisor to restart the hypervisee
      process. *)
  val restart_hypervisee : t -> unit tzresult Lwt.t

  (** [init params ~process_path] launches an external process using
      [process_path] as the path for the executable. It also performs the
      handshake and sends the initialization parameters [params] to the
      process. At the end of the [init] function, the external process is ready
      to receive and answer requests. *)
  val init : P.parameters -> process_path:string -> t tzresult Lwt.t

  (** [send_request p req] sends the request [req] to the external process [p],
      waits for the answer from the process and returns the result.  *)
  val send_request :
    t ->
    'a P.request ->
    ('a * (Profiler.report option * Profiler.report option) option) tzresult
    Lwt.t

  (** [reconfigure_event_logging p config] sends the request to the external
      process [p] to reconfigure the event logging system using the new
      configuration [config]. *)
  val reconfigure_event_logging :
    t -> Internal_event_config.t -> unit tzresult Lwt.t

  (** [close p] sends a termination request to the external process [p] and
      waits for [p] to stop. *)
  val close : t -> unit Lwt.t
end
