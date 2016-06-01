(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto

module Answer : sig

  (** Return type for service handler *)
  type 'a answer =
    { code : int ;
      body : 'a option }

  val ok: 'a -> 'a answer
  val return: 'a -> 'a answer Lwt.t

end

(** Possible error while registring services. *)
type step =
  | Static of string
  | Dynamic of Arg.descr
type conflict =
  | CService | CDir | CBuilder | CCustom
  | CTypes of Arg.descr *
              Arg.descr
  | CType of Arg.descr * string list
exception Conflict of step list * conflict
exception Cannot_parse of Arg.descr * string * string list

module Make (Repr : Json_repr.Repr) : sig

  (** Dispatch tree *)
  type 'prefix directory

  (** Empty tree *)
  val empty: 'prefix directory

  val map: ('a -> 'b) -> 'b directory -> 'a directory

  val prefix: ('pr, 'p) Path.path -> 'p directory -> 'pr directory
  val merge: 'a directory -> 'a directory -> 'a directory

  (** Resolve a service. *)
  val lookup:
    'prefix directory -> 'prefix -> string list ->
    (Repr.value -> Repr.value Answer.answer Lwt.t) Lwt.t


  (** Registring handler in service tree. *)
  val register:
    'prefix directory ->
    ('prefix, 'params, 'input, 'output) service ->
    ('params -> 'input -> 'output Answer.answer Lwt.t) ->
    'prefix directory

  (** Registring handler in service tree. Curryfied variant.  *)
  val register0:
    unit directory ->
    (unit, unit, 'i, 'o) service ->
    ('i -> 'o Answer.answer Lwt.t) ->
    unit directory

  val register1:
    'prefix directory ->
    ('prefix, unit * 'a, 'i, 'o) service ->
    ('a -> 'i -> 'o Answer.answer Lwt.t) ->
    'prefix directory

  val register2:
    'prefix directory ->
    ('prefix, (unit * 'a) * 'b, 'i, 'o) service ->
    ('a -> 'b -> 'i -> 'o Answer.answer Lwt.t) ->
    'prefix directory

  val register3:
    'prefix directory ->
    ('prefix, ((unit * 'a) * 'b) * 'c, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'i -> 'o Answer.answer Lwt.t) ->
    'prefix directory

  val register4:
    'prefix directory ->
    ('prefix, (((unit * 'a) * 'b) * 'c) * 'd, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'd -> 'i -> 'o Answer.answer Lwt.t) ->
    'prefix directory

  val register5:
    'prefix directory ->
    ('prefix, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'i, 'o) service ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'i -> 'o Answer.answer Lwt.t) ->
    'prefix directory

  (** Registring dynamic subtree. *)
  val register_dynamic_directory:
    ?descr:string ->
    'prefix directory ->
    ('prefix, 'a) Path.path -> ('a -> 'a directory Lwt.t) ->
    'prefix directory

  (** Registring dynamic subtree. (Curryfied variant) *)
  val register_dynamic_directory1:
    ?descr:string ->
    'prefix directory ->
    ('prefix, unit * 'a) Path.path ->
    ('a -> (unit * 'a) directory Lwt.t) ->
    'prefix directory

  val register_dynamic_directory2:
    ?descr:string ->
    'prefix directory ->
    ('prefix, (unit * 'a) * 'b) Path.path ->
    ('a -> 'b -> ((unit * 'a) * 'b) directory Lwt.t) ->
    'prefix directory

  val register_dynamic_directory3:
    ?descr:string ->
    'prefix directory ->
    ('prefix, ((unit * 'a) * 'b) * 'c) Path.path ->
    ('a -> 'b -> 'c -> (((unit * 'a) * 'b) * 'c) directory Lwt.t) ->
    'prefix directory

  (** Registring custom directory lookup. *)
  type custom_lookup =
    | CustomService of Description.service_descr *
                       (Repr.value -> Repr.value Answer.answer Lwt.t)
    | CustomDirectory of Description.directory_descr

  val register_custom_lookup:
    ?descr:string ->
    'prefix directory ->
    ('prefix, 'params) Path.path ->
    ('params -> string list -> custom_lookup Lwt.t) ->
    'prefix directory

  val register_custom_lookup1:
    ?descr:string ->
    'prefix directory ->
    ('prefix, unit * 'a) Path.path ->
    ('a -> string list -> custom_lookup Lwt.t) ->
    'prefix directory

  val register_custom_lookup2:
    ?descr:string ->
    'prefix directory ->
    ('prefix, (unit * 'a) * 'b) Path.path ->
    ('a -> 'b -> string list -> custom_lookup Lwt.t) ->
    'prefix directory

  val register_custom_lookup3:
    ?descr:string ->
    'prefix directory ->
    ('prefix, ((unit * 'a) * 'b) * 'c) Path.path ->
    ('a -> 'b -> 'c -> string list -> custom_lookup Lwt.t) ->
    'prefix directory


  (** Registring a description service. *)
  val register_describe_directory_service:
    'prefix directory ->
    ('prefix, 'prefix, bool option, Description.directory_descr) service ->
    'prefix directory

end

include (module type of Make (Json_repr.Ezjsonm))
