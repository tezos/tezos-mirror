(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Typed path argument. *)
module Arg : sig

  type 'a arg
  val make:
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit -> 'a arg

  type descr = {
    name: string ;
    descr: string option ;
  }
  val descr: 'a arg -> descr

  val int: int arg
  val int32: int32 arg
  val int64: int64 arg
  val float: float arg

end


(** Parametrized path to services. *)
module Path : sig

  type ('prefix, 'params) path
  type 'prefix context = ('prefix, 'prefix) path

  val root: 'a context

  val add_suffix:
    ('prefix, 'params) path -> string -> ('prefix, 'params) path
  val (/):
    ('prefix, 'params) path -> string -> ('prefix, 'params) path

  val add_arg:
    ('prefix, 'params) path -> 'a Arg.arg -> ('prefix, 'params * 'a) path
  val (/:):
    ('prefix, 'params) path -> 'a Arg.arg -> ('prefix, 'params * 'a) path

  val prefix:
    ('prefix, 'a) path -> ('a, 'params) path -> ('prefix, 'params) path

  val map:
    ('a -> 'b) -> ('b -> 'a) -> ('prefix, 'a) path -> ('prefix, 'b) path

end


(** Services. *)
type ('prefix, 'params, 'input, 'output) service

val service:
  ?description: string ->
  input: 'input Json_encoding.encoding ->
  output: 'output Json_encoding.encoding ->
  ('prefix, 'params) Path.path ->
  ('prefix, 'params, 'input, 'output) service

val prefix:
  ('prefix, 'inner_prefix) Path.path ->
  ('inner_prefix, 'params, 'input, 'output) service ->
  ('prefix, 'params, 'input, 'output) service


type json = Json_repr.Ezjsonm.value

val forge_request:
  (unit, 'params, 'input, 'output) service ->
  'params -> 'input -> string list * json

val read_answer:
  (unit, 'params, 'input, 'output) service ->
  json -> ('output, string) result

module Make (Repr : Json_repr.Repr) : sig

  val forge_request:
    (unit, 'params, 'input, 'output) service ->
    'params -> 'input -> string list * Repr.value

  val read_answer:
    (unit, 'params, 'input, 'output) service ->
    Repr.value -> ('output, string) result

end

(** Service directory description *)
module Description : sig

  type service_descr = {
    description: string option ;
    input: Json_schema.schema ;
    output: Json_schema.schema ;
  }

  type directory_descr =
    | Static of static_directory_descr
    | Dynamic of string option

  and static_directory_descr = {
    service: service_descr option ;
    subdirs: static_subdirectories_descr option ;
  }

  and static_subdirectories_descr =
    | Suffixes of directory_descr Map.Make(String).t
    | Arg of Arg.descr * directory_descr

  val service:
    ?description:string ->
    ('prefix, 'params) Path.path ->
    ('prefix, 'params, bool option, directory_descr) service

  val pp_print_directory_descr:
    Format.formatter -> directory_descr -> unit

end


(**/**)

module Internal : sig

  module Ty : sig

    exception Not_equal
    type (_, _) eq = Eq : ('a, 'a) eq

    type 'a id
    val eq : 'a id -> 'b id -> ('a, 'b) eq

  end

  type 'a arg = {
    id: 'a Ty.id;
    destruct: string -> ('a, string) result ;
    construct: 'a -> string ;
    descr: Arg.descr ;
  }

  val from_arg : 'a arg -> 'a Arg.arg
  val to_arg : 'a Arg.arg -> 'a arg

  type (_, _) rpath =
    | Root : ('rkey, 'rkey) rpath
    | Static : ('rkey, 'key) rpath * string -> ('rkey, 'key) rpath
    | Dynamic : ('rkey, 'key) rpath * 'a arg -> ('rkey, 'key * 'a) rpath

  type (_, _) path =
    | Path: ('prefix, 'params) rpath -> ('prefix, 'params) path
    | MappedPath:
        ('prefix, 'key) rpath * ('key -> 'params) * ('params -> 'key) ->
      ('prefix, 'params) path

  val from_path : ('a, 'b) path -> ('a, 'b) Path.path
  val to_path : ('a, 'b) Path.path -> ('a, 'b) path

  type ('prefix, 'params, 'input, 'output) iservice = {
    description : string option ;
    path : ('prefix, 'params) path ;
    input : 'input Json_encoding.encoding ;
    output : 'output Json_encoding.encoding ;
  }

  val from_service:
    ('prefix, 'params, 'input, 'output) iservice ->
     ('prefix, 'params, 'input, 'output) service
  val to_service:
    ('prefix, 'params, 'input, 'output) service ->
    ('prefix, 'params, 'input, 'output) iservice

end

(**/**)
