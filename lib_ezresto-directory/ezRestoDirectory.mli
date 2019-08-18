(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzResto

module Answer : sig

  (** Return type for service handler *)
  type 'a answer =
    { code : int ;
      body : 'a output ;
    }

  and 'a output =
    | Empty
    | Single of 'a
    | Stream of 'a stream

  and 'a stream = {
    next: unit -> 'a option Lwt.t ;
    shutdown: unit -> unit ;
  }

  val ok: 'a -> 'a answer
  val return: 'a -> 'a answer Lwt.t

end

(** Possible error while registring services. *)
type step =
  | Static of string
  | Dynamic of Arg.descr
type conflict =
  | CService | CDir | CBuilder | CCustom
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list
exception Conflict of step list * conflict

exception Cannot_parse of Arg.descr * string * string list

module Make (Repr : Json_repr.Repr) : sig

  (** Dispatch tree *)
  type directory

  (** Empty tree *)
  val empty: directory

  val prefix: 'a Path.path -> directory -> directory
  val merge: directory -> directory -> directory

  (** Resolve a service. *)
  val lookup:
    directory -> string list ->
    (Repr.value option -> Repr.value Answer.answer Lwt.t) Lwt.t


  (** Registring handler in service tree. *)
  val register:
    directory ->
    ('params, 'input, 'output) service ->
    ('params -> 'input -> 'output Answer.answer Lwt.t) ->
    directory

  (** Registring handler in service tree. Curryfied variant.  *)
  val register0:
    directory ->
    (unit, 'i, 'o) service ->
    ('i -> 'o Answer.answer Lwt.t) ->
    directory

  val register1:
    directory ->
    (unit * 'a, 'i, 'o) service ->
    ('a -> 'i -> 'o Answer.answer Lwt.t) ->
    directory

  val register2:
    directory ->
    ((unit * 'a) * 'b, 'i, 'o) service ->
    ('a -> 'b -> 'i -> 'o Answer.answer Lwt.t) ->
    directory

  val register3:
    directory ->
    (((unit * 'a) * 'b) * 'c, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'i -> 'o Answer.answer Lwt.t) ->
    directory

  val register4:
    directory ->
    ((((unit * 'a) * 'b) * 'c) * 'd, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'd -> 'i -> 'o Answer.answer Lwt.t) ->
    directory

  val register5:
    directory ->
    (((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'i -> 'o Answer.answer Lwt.t) ->
    directory

  (** Registring dynamic subtree. *)
  val register_dynamic_directory:
    ?descr:string ->
    directory ->
    'params Path.path ->
    ('params -> directory Lwt.t) ->
    directory

  (** Registring dynamic subtree. (Curryfied variant) *)
  val register_dynamic_directory1:
    ?descr:string ->
    directory ->
    (unit * 'a) Path.path ->
    ('a -> directory Lwt.t) ->
    directory

  val register_dynamic_directory2:
    ?descr:string ->
    directory ->
    ((unit * 'a) * 'b) Path.path ->
    ('a -> 'b -> directory Lwt.t) ->
    directory

  val register_dynamic_directory3:
    ?descr:string ->
    directory ->
    (((unit * 'a) * 'b) * 'c) Path.path ->
    ('a -> 'b -> 'c -> directory Lwt.t) ->
    directory

  (** Registring dynamic subtree. (Curryfied variant) *)


  (** Registring custom directory lookup. *)
  type custom_lookup =
    | CustomService of Description.service_descr *
                       (Repr.value option -> Repr.value Answer.answer Lwt.t)
    | CustomDirectory of Description.directory_descr

  val register_custom_lookup:
    ?descr:string ->
    directory ->
    ('params) Path.path ->
    ('params -> string list -> custom_lookup Lwt.t) ->
    directory

  val register_custom_lookup1:
    ?descr:string ->
    directory ->
    (unit * 'a) Path.path ->
    ('a -> string list -> custom_lookup Lwt.t) ->
    directory

  val register_custom_lookup2:
    ?descr:string ->
    directory ->
    ((unit * 'a) * 'b) Path.path ->
    ('a -> 'b -> string list -> custom_lookup Lwt.t) ->
    directory

  val register_custom_lookup3:
    ?descr:string ->
    directory ->
    (((unit * 'a) * 'b) * 'c) Path.path ->
    ('a -> 'b -> 'c -> string list -> custom_lookup Lwt.t) ->
    directory


  (** Registring a description service. *)
  val register_describe_directory_service:
    directory ->
    (unit, bool option, Description.directory_descr) service ->
    directory

end

include (module type of Make (Json_repr.Ezjsonm))

