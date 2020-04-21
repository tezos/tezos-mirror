(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Client calls to services. *)

(** [Make(Encoding)(Client)] is a module that allows you to make calls to
    various [Resto] (or [EzResto]) services.

    The calls are type safe: you must provide the correct parameters to the
    services which are automatically encoded according to [Encoding], the answer
    is automatically decoded according to [Encoding]. The scheduling (waiting on
    answers, etc.) is provided by [Client]. *)
module Make (Encoding : Resto.ENCODING) (Client : Cohttp_lwt.S.Client) : sig

  module Service : (module type of (struct include Resto.MakeService(Encoding) end))

  (** Content-Type header specifications:
      https://tools.ietf.org/html/rfc7231#section-3.1.1.5
      and additional information:
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type *)
  type content_type = (string * string)
  type raw_content = Cohttp_lwt.Body.t * content_type option
  type content =
    Cohttp_lwt.Body.t * content_type option * Media_type.Make(Encoding).t option

  (** The type for possible results when calling over HTTP. Some results
      correspond to an HTTP return code, other results correspond to internal
      issues with resto. *)
  type ('o, 'e) generic_rest_result =
    [ `Ok of 'o option (** 200 *)
    | `Conflict of 'e (** 409 *)
    | `Error of 'e (** 500 *)
    | `Forbidden of 'e (** 403 *)
    | `Not_found of 'e (** 404 *)
    | `Gone of 'e (** 410 *)
    | `Unauthorized of 'e (** 401 *)
    | `Bad_request of string (** 400 *)
    | `Method_not_allowed of string list (** 405 *)
    | `Unsupported_media_type (** 415 *)
    | `Not_acceptable of string (** 406 *)
    | `Unexpected_status_code of Cohttp.Code.status_code * content
    (** HTTP status code set by server is invalid or unsupported by resto *)
    | `Connection_failed of string
    (** Failure at one of the levels lower than HTTP (e.g., network) *)
    | `OCaml_exception of string
    (** Exception raised whilst decoding the result. *)
    | `Unauthorized_host of string option (** CORS-related error *) ]

  (** A [LOGGER] module is used for logging only *)
  module type LOGGER = sig
    type request
    val log_empty_request: Uri.t -> request Lwt.t
    val log_request:
      ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Uri.t -> string -> request Lwt.t
    val log_response:
      request -> ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Cohttp.Code.status_code -> string Lwt.t Lazy.t -> unit Lwt.t
  end

  type logger = (module LOGGER)

  val null_logger: logger
  val timings_logger: gettimeofday:(unit -> float) -> Format.formatter -> logger
  val full_logger: Format.formatter -> logger

  (** Low-level call primitive: use only for making calls for which there is no
      service defined, prefer making call to a defined service. *)
  val generic_call:
    [< Resto.meth ] ->
    ?headers:(string * string) list ->
    ?accept:Media_type.Make(Encoding).t list ->
    ?body:Cohttp_lwt.Body.t ->
    ?media:Media_type.Make(Encoding).t ->
    Uri.t -> (content, content) generic_rest_result Lwt.t

  (** The type for possible results when calling a service. This includes the
      possible HTTP results (see [generic_rest_result] and other
      service-specific errors. *)
  type ('o, 'e) service_result =
    [ ('o, 'e option) generic_rest_result
    | `Unexpected_content_type of raw_content
    | `Unexpected_content of (string * Media_type.Make(Encoding).t) * string
    | `Unexpected_error_content_type of raw_content
    | `Unexpected_error_content of (string * Media_type.Make(Encoding).t) * string ]

  (** [call_service media_types ?logger ?headers ?base service path_params
      query_params input] makes a call to [service] with the parameters
      [path_params], [query_params], and [input]. It returns a result (or an
      error).

      The OCaml type system guarantees that the parameters are as expected by
      the service. *)
  val call_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    'p -> 'q -> 'i -> (Resto.meth * Uri.t * ('o, 'e) service_result) Lwt.t


  (** [call_streamed_service media_types ?logger ?headers ?base service
      ~on_chunk ~on_close path_params query_params input] makes a call to
      [service] with the parameters [path_params], [query_params], and [input].
      The values returned by the service are passed to the [on_chunk] callback,
      and when the server closes the connection the [on_close] callback is
      called.

      The function returns a [unit -> unit] function that consumes the remainder
      of the input without side-effects. Call this function when you want to
      discard all the queued-up chunks.

      The OCaml type system guarantees that the parameters are as expected by
      the service. *)
  val call_streamed_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:(string * string) list ->
    ?base:Uri.t ->
    ([< Resto.meth ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i ->
    (Resto.meth * Uri.t * (unit -> unit, 'e) service_result) Lwt.t

end
