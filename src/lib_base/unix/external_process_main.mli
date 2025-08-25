(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Module type for argument of functor {!Make} which provides the handler code
    for each request.  *)
module type EXTERNAL_PROCESSING = sig
  type parameters

  type 'response request

  (** The type of state for the processing code of the external process. *)
  type state

  (** The initial state for the external processing. *)
  val initial_state : parameters -> state tzresult Lwt.t

  (** [handle_request params state acc req] handles the request [req] and
      returns [`Continue (res, state)] if the external process should continue
      handling requests normally, where [res] is the result and [state] the new
      state. It returns [`Stop] if the external process should stop after
      processing [req]. *)
  val handle_request :
    parameters ->
    state ->
    'response request ->
    [ `Continue of
      ('response * (Profiler.report option * Profiler.report option) option)
      tzresult
      * state
    | `Stop ]
    Lwt.t
end

module Make
    (Params : External_process_parameters.S)
    (Processing :
      EXTERNAL_PROCESSING
        with type parameters := Params.parameters
         and type 'response request := 'response Params.request) : sig
  (** Main entry point of the external process. *)
  val main : socket_dir:string -> unit tzresult Lwt.t
end
