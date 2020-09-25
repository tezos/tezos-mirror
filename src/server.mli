(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Serving a directory of registered services. *)

module type LOGGING = sig
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_warn : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end

module Make (Encoding : Resto.ENCODING) : sig
  module Media_type : module type of struct
    include Media_type.Make (Encoding)
  end

  module Directory : module type of struct
    include Resto_directory.Make (Encoding)
  end

  module Make (Log : LOGGING) : sig
  (** A handle on the server worker. *)
  type server


  (** Promise a running RPC server.*)
  val launch :
    ?host:string ->
    ?cors:Cors.t ->
    ?agent:string ->
    ?acl:Acl.t ->
    media_types:Media_type.t list ->
    Conduit_lwt_unix.server ->
    unit Directory.t ->
    server Lwt.t

  (* configure the access list for this server *)
  val set_acl : server -> Acl.t -> unit

  (** Kill an RPC server. *)
  val shutdown : server -> unit Lwt.t

  end

  module Internal : sig
    type medias = {
      media_types : Media_type.t list;
      default_media_type : string * Media_type.t;
    }

    val default_agent : string

    val default_media_type : Media_type.t list -> string * Media_type.t

    val invalid_cors : Resto_cohttp.Cors.t -> Cohttp.Header.t -> bool

    val invalid_cors_response :
      string -> (Cohttp.Response.t * Cohttp_lwt.Body.t, 'a) Result.result Lwt.t

    val input_media_type :
      ?headers:Cohttp.Header.t ->
      medias ->
      (Media_type.t, [> `Unsupported_media_type of string]) result

    val output_content_media_type :
      ?headers:Cohttp.Header.t ->
      medias ->
      (string * Media_type.t, [> `Not_acceptable]) Result.result

    val handle_error :
      medias ->
      [< `Cannot_parse_body of string
      | `Cannot_parse_path of string list * Resto.Arg.descr * string
      | `Cannot_parse_query of string
      | `Method_not_allowed of [< Resto.meth] list
      | `Not_acceptable
      | `Not_found
      | `Not_implemented
      | `Unsupported_media_type of 'a ] ->
      Cohttp.Response.t * Cohttp_lwt.Body.t

    val handle_rpc_answer :
      ?headers:Cohttp.Header.t ->
      ('o -> string) ->
      ('e -> Cohttp_lwt.Body.t * Cohttp.Transfer.encoding) ->
      [< `Conflict of 'e
      | `Created of string option
      | `Error of 'e
      | `Forbidden of 'e
      | `Gone of 'e
      | `No_content
      | `Not_found of 'e
      | `Ok of 'o
      | `Unauthorized of 'e ] ->
      (Cohttp.Response.t * Cohttp_lwt.Body.t, 'c) Result.result Lwt.t

    val handle_options :
      unit Directory.t ->
      Resto_cohttp.Cors.t ->
      Cohttp.Header.t ->
      string list ->
      ( Cohttp.Response.t * Cohttp_lwt.Body.t,
        [> Directory.lookup_error] )
      Result.result
      Lwt.t
  end
end
