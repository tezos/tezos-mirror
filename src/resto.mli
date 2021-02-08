(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Utils : sig
  val split_path : string -> string list

  val decode_split_path : string -> string list
end

type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

val string_of_meth : [< meth] -> string

val meth_of_string : string -> [> meth] option

module MethMap : Map.S with type key = meth

module StringMap : Map.S with type key = string

(** [eq] is an equality witness type. It is returned by some non-trivial
    equality-testing functions in the rest of this module. In general, it is
    intended to be used as follows:

    [match are_equal foo bar with
     | None -> (* values are not equal *) ..
     | Some Eq -> (* values are equal *) ..] *)
type (_, _) eq = Eq : ('a, 'a) eq

(** Typed path argument. *)
module Arg : sig
  (** An argument to a service. *)
  type 'a t

  type 'a arg = 'a t

  (** [make ?descr ~name ~destruct ~construct ()] is an argument. The values of
      [descr] and [name] are used for documentation purpose only. The values of
      [destruct] and [construct] are used for conversion from/to [string]s. Note
      that it is expected that [destruct] and [construct] roundtrip (modulo the
      [result] error wrapping). *)
  val make :
    ?descr:string ->
    name:string ->
    destruct:(string -> ('a, string) result) ->
    construct:('a -> string) ->
    unit ->
    'a arg

  (** [descr] is a type for the documentation of a [t]. *)
  type descr = {name : string; descr : string option}

  (** [descr t] is the documentation of [t]. *)
  val descr : 'a arg -> descr

  (** [bool] is an argument for boolean values. The strings ["yes"], ["y"],
      ["true"], and ["t"], as well as all capitalisation variation thereof, all
      parsed to [true]. The strings ["no"], ["n"], ["false"], and ["f"], as well
      as all capitalisation variation thereof, all parsed to [false]. All other
      strings fail to parse. *)
  val bool : bool arg

  (** [int] is an argument for integer values. The parsing is identical to that
      of {!Stdlib.int_of_string} -- refer to that function's documentation. *)
  val int : int arg

  (** [int32] is an argument for 32-bit integer values. The parsing is identical
      to that of {!Int32.of_string} -- refer to that function's
      documentation. *)
  val int32 : int32 arg

  (** [int64] is an argument for 64-bit integer values. The parsing is identical
      to that of {!Int64.of_string} -- refer to that function's
      documentation. *)
  val int64 : int64 arg

  (** [float] is an argument for floating-point decimal values. The parsing is
      identical to that of {!Float.of_string} -- refer to that function's
      documentation. *)
  val float : float arg

  (** [string] is an argument for string values. No parsing is done. *)
  val string : string arg

  (** [like a ?descr name] is a new argument which carries the same type
      parameter and uses the same constructor/destructor functions as [a], but
      has a different name and description. It is intended to be used to give
      different meaning to isomorphic arguments. E.g.,

      [let date =
         make ~descr:"A date in YYYY-MM-DD format" ~name:"date"
              ~destruct:(fun s -> ..) ~construct:(fun d -> ..)
              () in
       let birth_date =
         like date
              ~descr:"A date of birth in the YYYY-MM-DD format" "birth-date" in
       ..]
    *)
  val like : 'a arg -> ?descr:string -> string -> 'a arg

  (** [eq a b] is [Some Eq] if [a] and [b] are the same and [None] otherwise.

      Note that [eq a (like a ..)] is always [None]. *)
  val eq : 'a arg -> 'b arg -> ('a, 'b) eq option
end

(** Parametrized path to services. *)
module Path : sig
  (** The type of a path. Here "path" is to be taken in the sense of a
      slash-separated sequence of segments of a URI/URL. *)
  type ('prefix, 'params) t

  type ('prefix, 'params) path = ('prefix, 'params) t

  type 'prefix context = ('prefix, 'prefix) path

  (** [root] is the basis to build paths upon. It is the "[Nil]" of path
      construction. *)
  val root : unit context

  val open_root : 'a context

  (** [add_suffix p s] is a path in which [s] has been appended to the sequence
      of segments described by [p]. *)
  val add_suffix : ('prefix, 'params) path -> string -> ('prefix, 'params) path

  (** [(/)] is an infix operator for [add_suffix]. *)
  val ( / ) : ('prefix, 'params) path -> string -> ('prefix, 'params) path

  (** [add_arg p a] is a path in which a segment representing a value of type
      [a] has been appended to the sequence of segments described by [p].

      This is intended for use by services. Specifically, a service that is
      parameterized over a value of type [ty] is attached to a path that includes
      an argument for a value of type [ty]. When the service is called, Resto
      decodes the argument and passes its value to the service. *)
  val add_arg :
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a) path

  (** [(/:)] is an infix operator for [add_arg]. *)
  val ( /: ) :
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a) path

  (** [add_final_args p a] is a path in which an arbitrary sequence of segments
      representing values of type [a] has been appended to the sequence of
      segments described by [p].

      A similar use to the [add_arg] is intended, but for a list of values
      rather than a single value.

      Note that, as the name suggests, [add_final_args] is final: you cannot add
      further suffixes or arguments to the resulting path. Attempting to do so
      raises [Invalid_arg]. Similarly, using the resulting path as a [prefix]
      (see below) raises the same exception. This is because paths built with
      [add_final_args] consume all the suffix as an unterminated list of
      arguments, there cannot be further suffixes or arguments. *)
  val add_final_args :
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a list) path

  (** [( /:* )] is an infix operator for [add_final_args]. *)
  val ( /:* ) :
    ('prefix, 'params) path -> 'a Arg.t -> ('prefix, 'params * 'a list) path

  (** [prefix p q] is a path in which the sequence of segments of [p] is
      followed by the sequence of segments of [q]. *)
  val prefix :
    ('prefix, 'a) path -> ('a, 'params) path -> ('prefix, 'params) path

  val subst0 : ('p, 'p) path -> ('p2, 'p2) path

  val subst1 : ('p, 'p * 'a) path -> ('p2, 'p2 * 'a) path

  val subst2 : ('p, ('p * 'a) * 'b) path -> ('p2, ('p2 * 'a) * 'b) path

  val subst3 :
    ('p, (('p * 'a) * 'b) * 'c) path -> ('p2, (('p2 * 'a) * 'b) * 'c) path
end

(** Service directory description *)
module Description : sig
  type request = {recurse : bool}

  [@@@ocaml.warning "-30"]

  type 'schema service = {
    description : string option;
    path : path_item list;
    meth : meth;
    query : query_item list;
    input : 'schema Lazy.t option;
    output : 'schema Lazy.t;
    error : 'schema Lazy.t;
  }

  and path_item =
    | PStatic of string
    | PDynamic of Arg.descr
    | PDynamicTail of Arg.descr

  and query_item = {
    name : string;
    description : string option;
    kind : query_kind;
  }

  and query_kind =
    | Single of Arg.descr
    | Optional of Arg.descr
    | Flag
    | Multi of Arg.descr

  type 'schema directory =
    | Empty
    | Static of 'schema static_directory
    | Dynamic of string option

  and 'schema static_directory = {
    services : 'schema service MethMap.t;
    subdirs : 'schema static_subdirectories option;
  }

  and 'schema static_subdirectories =
    | Suffixes of 'schema directory StringMap.t
    | Arg of Arg.descr * 'schema directory

  val pp_print_directory :
    (* ?pp_schema:(Format.formatter -> 'schema -> unit) -> *)
    (* TODO ?? *)
    Format.formatter ->
    'schema directory ->
    unit
end

module Query : sig
  type 'a t

  type 'a query = 'a t

  val empty : unit query

  type ('a, 'b) field

  val field :
    ?descr:string -> string -> 'a Arg.t -> 'a -> ('b -> 'a) -> ('b, 'a) field

  val opt_field :
    ?descr:string ->
    string ->
    'a Arg.t ->
    ('b -> 'a option) ->
    ('b, 'a option) field

  val flag : ?descr:string -> string -> ('b -> bool) -> ('b, bool) field

  val multi_field :
    ?descr:string ->
    string ->
    'a Arg.t ->
    ('b -> 'a list) ->
    ('b, 'a list) field

  type ('a, 'b, 'c) open_query

  val query : 'b -> ('a, 'b, 'b) open_query

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_query -> ('a, 'c) field -> ('a, 'b, 'd) open_query

  val seal : ('a, 'b, 'a) open_query -> 'a t

  type untyped = (string * string) list

  exception Invalid of string

  val parse : 'a query -> untyped -> 'a
end

(**/**)

module Internal : sig
  module Ty : sig
    exception Not_equal

    type 'a id

    val eq : 'a id -> 'b id -> ('a, 'b) eq
  end

  type 'a arg = {
    id : 'a Ty.id;
    destruct : string -> ('a, string) result;
    construct : 'a -> string;
    descr : Arg.descr;
  }

  val from_arg : 'a arg -> 'a Arg.t

  val to_arg : 'a Arg.t -> 'a arg

  type (_, _) path =
    | Root : ('rkey, 'rkey) path
    | Static : ('rkey, 'key) path * string -> ('rkey, 'key) path
    | Dynamic : ('rkey, 'key) path * 'a arg -> ('rkey, 'key * 'a) path
    | DynamicTail : ('rkey, 'key) path * 'a arg -> ('rkey, 'key * 'a list) path

  val from_path : ('a, 'b) path -> ('a, 'b) Path.t

  val to_path : ('a, 'b) Path.t -> ('a, 'b) path

  type 'a query = Fields : ('a, 'b) query_fields * 'b -> 'a query

  and ('a, 'b) query_fields =
    | F0 : ('a, 'a) query_fields
    | F1 :
        ('a, 'b) query_field * ('a, 'c) query_fields
        -> ('a, 'b -> 'c) query_fields

  and ('a, 'b) query_field =
    | Single : {
        name : string;
        description : string option;
        ty : 'b arg;
        default : 'b;
        get : 'a -> 'b;
      }
        -> ('a, 'b) query_field
    | Opt : {
        name : string;
        description : string option;
        ty : 'b arg;
        get : 'a -> 'b option;
      }
        -> ('a, 'b option) query_field
    | Flag : {
        name : string;
        description : string option;
        get : 'a -> bool;
      }
        -> ('a, bool) query_field
    | Multi : {
        name : string;
        description : string option;
        ty : 'b arg;
        get : 'a -> 'b list;
      }
        -> ('a, 'b list) query_field

  val from_query : 'a query -> 'a Query.t

  val to_query : 'a Query.t -> 'a query

  val field_name : ('a, 'b) query_field -> string

  val field_description : ('a, 'b) query_field -> string option

  val field_kind : ('a, 'b) query_field -> Description.query_kind
end

(**/**)

(** An [ENCODING] is a generic interface for modules that provide conversion
    from values to a different representation and back. This is used to abstract
    resto over specific representations of values.

    See {!Resto_json} for example of possible instantiation. *)
module type ENCODING = sig
  type 'a t

  type schema

  val unit : unit t

  val untyped : string t

  val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

  val schema : ?definitions_path:string -> 'a t -> schema

  val description_request_encoding : Description.request t

  val description_answer_encoding : schema Description.directory t
end

module MakeService (Encoding : ENCODING) : sig
  (** Services. *)
  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t
    constraint 'meth = [< meth]

  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t

  val meth :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service -> 'meth

  val query :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'query Query.t

  type _ input =
    | No_input : unit input
    | Input : 'input Encoding.t -> 'input input

  val input_encoding :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'input input

  val output_encoding :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'output Encoding.t

  val error_encoding :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    'error Encoding.t

  val get_service :
    ?description:string ->
    query:'query Query.t ->
    output:'output Encoding.t ->
    error:'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ([`GET], 'prefix, 'params, 'query, unit, 'output, 'error) service

  val post_service :
    ?description:string ->
    query:'query Query.t ->
    input:'input Encoding.t ->
    output:'output Encoding.t ->
    error:'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ([`POST], 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val delete_service :
    ?description:string ->
    query:'query Query.t ->
    output:'output Encoding.t ->
    error:'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ([`DELETE], 'prefix, 'params, 'query, unit, 'output, 'error) service

  val patch_service :
    ?description:string ->
    query:'query Query.t ->
    input:'input Encoding.t ->
    output:'output Encoding.t ->
    error:'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ([`PATCH], 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val put_service :
    ?description:string ->
    query:'query Query.t ->
    input:'input Encoding.t ->
    output:'output Encoding.t ->
    error:'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ([`PUT], 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val prefix :
    ('prefix, 'inner_prefix) Path.t ->
    ('meth, 'inner_prefix, 'params, 'query, 'input, 'output, 'error) service ->
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service

  val subst0 :
    (([< meth] as 'm), 'p, 'p, 'q, 'i, 'o, 'e) service ->
    ('m, 'p2, 'p2, 'q, 'i, 'o, 'e) service

  val subst1 :
    (([< meth] as 'm), 'p, 'p * 'a, 'q, 'i, 'o, 'e) service ->
    ('m, 'p2, 'p2 * 'a, 'q, 'i, 'o, 'e) service

  val subst2 :
    (([< meth] as 'm), 'p, ('p * 'a) * 'b, 'q, 'i, 'o, 'e) service ->
    ('m, 'p2, ('p2 * 'a) * 'b, 'q, 'i, 'o, 'e) service

  val subst3 :
    (([< meth] as 'm), 'p, (('p * 'a) * 'b) * 'c, 'q, 'i, 'o, 'e) service ->
    ('m, 'p2, (('p2 * 'a) * 'b) * 'c, 'q, 'i, 'o, 'e) service

  type ('prefix, 'params, 'error) description_service =
    ( [`GET],
      'prefix,
      'params * string list,
      Description.request,
      unit,
      Encoding.schema Description.directory,
      'error )
    service

  val description_service :
    ?description:string ->
    'error Encoding.t ->
    ('prefix, 'params) Path.t ->
    ('prefix, 'params, 'error) description_service

  type 'input request = {meth : meth; uri : Uri.t; input : 'input input}

  val forge_request :
    ('meth, unit, 'params, 'query, 'input, 'output, 'error) service ->
    ?base:Uri.t ->
    'params ->
    'query ->
    'input request

  val forge_partial_request :
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
    ?base:Uri.t ->
    'params ->
    'query ->
    'input request

  module Internal : sig
    include module type of struct
      include Internal
    end

    type ('query, 'input, 'output, 'error) types = {
      query : 'query Query.t;
      input : 'input input;
      output : 'output Encoding.t;
      error : 'error Encoding.t;
    }

    type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) iservice = {
      description : string option;
      meth : 'meth;
      path : ('prefix, 'params) path;
      types : ('query, 'input, 'output, 'error) types;
    }
      constraint 'meth = [< meth]

    exception Not_equal

    type (_, _) eq =
      | Eq
          : ( ('query, 'input, 'output, 'error) types,
              ('query, 'input, 'output, 'error) types )
            eq

    val eq :
      ('query1, 'input1, 'output1, 'error1) types ->
      ('query2, 'input2, 'output2, 'error2) types ->
      ( ('query1, 'input1, 'output1, 'error1) types,
        ('query2, 'input2, 'output2, 'error2) types )
      eq

    val from_service :
      ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) iservice ->
      ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service

    val to_service :
      ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service ->
      ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) iservice
  end
end
