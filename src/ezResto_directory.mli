(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*  Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>       *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzResto

(** Directories are sets of services. They are used to spin up servers (see
    [Server]) that reply to requests for all their registered services. *)

module Answer : sig
  (** Return type for service handler

      Note about the three distinct [200]-code constructor:
      - [`Ok] is for RPCs that return a value that the server should encode as
        one blob of data. This should be the most common answer for successful
        returns of simple, reasonably-sized values.
      - [`OkChunk] is for RPCs that return a value that the server should encode
        as chunks: multiple blobs, the concatenation of which represents the
        data. This should be reserved for values that can be fairly large
        (typically over 4Kb, but this threshold may vary depending on your
        setup). Data is then transferred using chunked transfer encoding.
      - [`OkStream] is for RPCs that return a stream of values, not all of which
        are determined at the time of call. *)
  type ('o, 'e) t =
    [ `Ok of 'o (* 200 *)
    | `OkChunk of 'o (* 200 *)
    | `OkStream of 'o stream (* 200 *)
    | `Created of string option (* 201 *)
    | `No_content (* 204 *)
    | `Unauthorized of 'e option (* 401 *)
    | `Forbidden of 'e option (* 403 *)
    | `Not_found of 'e option (* 404 *)
    | `Conflict of 'e option (* 409 *)
    | `Gone of 'e option (* 410 *)
    | `Error of 'e option (* 500 *) ]

  and 'a stream = {next : unit -> 'a option Lwt.t; shutdown : unit -> unit}
end

(** Possible error while registering services. *)
type step =
  | Static of string
  | Dynamic of Arg.descr
  | DynamicTail of Arg.descr

type conflict =
  | CService of meth
  | CDir
  | CBuilder
  | CTail
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

exception Conflict of step list * conflict

(** A set of services on which requests can be dispatched. *)
type directory

(** Empty tree *)
val empty : directory

(** [prefix p d] is a directory of services which includes a service registered
    on the path [p / q] for each service registered on the path [q] in [d].

    @raise [Invalid_argument] if [p] is a dynamic path. *)
val prefix : 'a Path.t -> directory -> directory

(** [merge d1 d2] is a directory which includes all the services of [d1] and
    [d2].

    @raise [Conflict] if one or more service from [d1] conflicts with one or
    more service from [d2]. *)
val merge : directory -> directory -> directory

(** Registered services (with existential types for parameters and such). *)

type 'input input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input

type ('q, 'i, 'o, 'e) types = {
  query : 'q Resto.Query.t;
  input : 'i input;
  output : 'o Json_encoding.encoding;
  error : 'e Json_encoding.encoding;
}

type registered_service =
  | Service : {
      types : ('q, 'i, 'o, 'e) types;
      handler : 'q -> 'i -> ('o, 'e) Answer.t Lwt.t;
    }
      -> registered_service

(** Resolve a service. *)

type lookup_error =
  [ `Not_found (* 404 *)
  | `Method_not_allowed of meth list (* 405 *)
  | `Cannot_parse_path of string list * Arg.descr * string (* 400 *) ]

(** [lookup d m p] is [Ok (Service _)] if there is a service [s] registered in
    [d] and both the method of [s] is [m] and the path of [s] matches [p]. It is
    [Error _] otherwise.

    If it is [Ok (Service _)] then the returned value corresponds to the
    registered service. *)
val lookup :
  directory ->
  meth ->
  string list ->
  (registered_service, [> lookup_error]) result Lwt.t

(** [allowed_methods d p] is the set of methods [m] such that [lookup d m p] is
    [Ok _]. In other words, it is the set of methods [m] such that a service has
    been registered in [d] for a path that matches [p]. *)
val allowed_methods :
  directory -> string list -> (meth list, [> lookup_error]) result Lwt.t

val transparent_lookup :
  directory ->
  ('meth, 'params, 'query, 'input, 'output, 'error) EzResto.service ->
  'params ->
  'query ->
  'input ->
  [> ('output, 'error) Answer.t] Lwt.t

(** Registering a handler to a service in a directory. *)

(** [register d s h] is a directory that contains all the services registered
    in [d] plus the service [s]. Requests to the service [s] are handled by the
    handler [h]. *)
val register :
  directory ->
  ('meth, 'params, 'query, 'input, 'output, 'error) EzResto.service ->
  ('params -> 'query -> 'input -> ('output, 'error) Answer.t Lwt.t) ->
  directory

(** Below are variants of the [register] function curryfied for specific arity
    of services. *)

val register0 :
  directory ->
  ('meth, unit, 'q, 'i, 'o, 'e) EzResto.service ->
  ('q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

val register1 :
  directory ->
  ('meth, unit * 'a, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

val register2 :
  directory ->
  ('meth, (unit * 'a) * 'b, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

val register3 :
  directory ->
  ('meth, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'c -> 'q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

val register4 :
  directory ->
  ('meth, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

val register5 :
  directory ->
  ( 'meth,
    ((((unit * 'a) * 'b) * 'c) * 'd) * 'e,
    'q,
    'i,
    'o,
    'e )
  EzResto.service ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> ('o, 'e) Answer.t Lwt.t) ->
  directory

(** Registring dynamic subtree. *)
val register_dynamic_directory :
  ?descr:string ->
  directory ->
  'params Path.t ->
  ('params -> directory Lwt.t) ->
  directory

(** Registring dynamic subtree. (Curryfied variant) *)
val register_dynamic_directory1 :
  ?descr:string ->
  directory ->
  (unit * 'a) Path.t ->
  ('a -> directory Lwt.t) ->
  directory

val register_dynamic_directory2 :
  ?descr:string ->
  directory ->
  ((unit * 'a) * 'b) Path.t ->
  ('a -> 'b -> directory Lwt.t) ->
  directory

val register_dynamic_directory3 :
  ?descr:string ->
  directory ->
  (((unit * 'a) * 'b) * 'c) Path.t ->
  ('a -> 'b -> 'c -> directory Lwt.t) ->
  directory

(** Registering a description service. *)
val register_describe_directory_service :
  directory -> EzResto.description_service -> directory
