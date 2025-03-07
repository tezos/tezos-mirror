(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** The signature to instantiate the external process functor
    {!External_process.Make}. *)
module type S = sig
  (** The type of parameters, which contains the necessary information to
      initialize the external process. *)
  type parameters

  (** The type of requests understood by the external process. The external
      injector answers with responses of type [`response] for requests of type
      [`response request]. *)
  type 'response request

  (** The name of the external process as it should appear in events. Can
      contain alphanumerical characters and underscores.  *)
  val name : string

  (** A pretty printer for requests. *)
  val request_pp : Format.formatter -> 'response request -> unit

  (** An accessor for the internal events configuration from the parameters. *)
  val internal_events : parameters -> Internal_event_config.t

  (** Magic bytes used for the handshake between the launcher and the external
      process.  *)
  val magic : Bytes.t

  (** Encoding for the parameters. *)
  val parameters_encoding : parameters Data_encoding.t

  (** Existential type of requests, used for encoding. *)
  type packed_request = Erequest : _ request -> packed_request

  (** Encoding for (packed) requests. *)
  val request_encoding : packed_request Data_encoding.t

  (** [result_encoding request] is the encoding for results in response to the
      request [request]. *)
  val result_encoding : 'response request -> 'response Data_encoding.t

  (** The request to reconfigure the event logging system of the external
      process. *)
  val reconfigure_event_logging_request :
    Internal_event_config.t -> unit request

  (** The request to terminate the external process. *)
  val terminate_request : packed_request

  (** The prefix for the external process socket filename.

    Do not use it directly except for documentation purposes; use
    [socket_path] instead. *)
  val socket_path_prefix : string

  (** Get the external process socket path.

    [socket_dir] is the directory where the file should be put.
    [pid] is the process ID of the external process. *)
  val socket_path : socket_dir:string -> pid:int -> string

  (** [command_line_args params ~socket_dir] is [(name, args)] where [name] is
      the new name for the external process (zeroth argument) and [args] are the
      arguments that should be given to the external process executable to
      start. *)
  val command_line_args : socket_dir:string -> string * string list

  val hypervisor_name : string

  val share_sink : bool
end
