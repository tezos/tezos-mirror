(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Miscellaneous definitions. *)

(** Same as [Printf.sprintf]. *)
val sf : ('a, Format.formatter, unit, string) format4 -> 'a

(** Same as [Filename.concat]. *)
val ( // ) : string -> string -> string

(** Give a default value to an optional value. *)
val default : 'a -> 'a option -> 'a

(** [Set.Make (String)]. *)
module String_set : Set.S with type elt = string

(** [Map.Make (String)]. *)
module String_map : Map.S with type key = string

(** Same as [Printf.ksprintf print_endline]. *)
val echo : ('a, unit, string, unit) format4 -> 'a

(** Memoize a function.

    If [g] is defined as [memoize f], the first time [g x] is called it returns [f x]
    and stores the result [y] in a [Hashtbl]. Calling [g x] again on the same [x] causes
    [g] to immediately return [y] without calling [f] again.

    The type of arguments ['a] must be compatible with [Hashtbl]. *)
val memoize : ('a -> 'b) -> 'a -> 'b

(** Split a string on a given character.

    Similar to [String.split_on_char], but splits only once.

    Examples:
    - [str_split_once "abc" ','] is [None].
    - [str_split_once "a,bc" ','] is [Some ("a", "bc")].
    - [str_split_once "a,b,c" ','] is [Some ("a", "b,c")]. *)
val str_split_once : string -> char -> (string * string) option

(** Quote a string using [sh] syntax.

    Similar to [Filename.quote], but the result is prettier for humans:
    - if the string only contains safe characters, it is not quoted;
    - if the string contains unsafe characters but no single quote character,
      it is quoted using a single pair of single quotes. *)
val quote_shell : string -> string

(** Quote a command using [sh] syntax.

    Similar to [Filename.quote_command], but the result is prettier for humans
    (see {!quote_shell}). *)
val quote_command : string -> string list -> string

(** Same as [Unix.close], but ignore errors. *)
val close : Unix.file_descr -> unit

(** Same as [Unix.closedir], but ignore errors. *)
val closedir : Unix.dir_handle -> unit

(** {2 Error Handling} *)

(** Errors.

    Errors have a [code], that is meant to be pattern-matched on;
    and a [message], which is meant to be displayed to the user,
    e.g. if the error cannot be recovered from.

    The type of [code]s is typically a polymorphic variant.
    This allows to merge the set of errors that can occur.

    It is recommended to:
    - use a [code] equal to [`failed] for errors that are not meant to be handled;
    - have the main entrypoint handle [`failed], and only [`failed];
    - never handle [`failed] in any other place.

    The [message] itself is a list of strings such as
    [["failed to read hello.txt"; "file not found"]].
    It is ordered from the outermost context to the innermost context.
    It is recommended to print it in this order, so that the user can see the
    fundamental error as the last line on its terminal, with the previous lines
    giving more context to understand this error. *)
type 'a error = {code : 'a; message : string list}

(** {3 Error Monad} *)

(** Result values that can be errors. *)
type ('a, 'e) r = ('a, 'e error) result

(** Return an error.

    For instance:
    [{
      error `failed "failed to read %s" "hello.txt"
        ~reason: [ "file not found" ]
    }]
    is:
    [{
      Error {
        code = `failed;
        message = [ "failed to read hello.txt"; "file not found" ];
    }] *)
val error :
  ?reason:string list ->
  'a ->
  ('b, unit, string, ('c, 'a error) result) format4 ->
  'b

(** Short-hand for [error `failed]. *)
val fail :
  ?reason:string list ->
  ('a, unit, string, ('b, [> `failed] error) result) format4 ->
  'a

(** Short-hand for [Ok ()].

    Avoids allocating everytime you want to return unit. *)
val unit : (unit, 'a) result

(** The bind operator of the error monad. *)
val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

(** {3 Transforming Errors} *)

(** Wrap errors to give them more context.

    For instance, you can write [wrap_errors "failed to read file" @@ ...]
    at the beginning of a function to prepend ["failed to read file"] to the [message]
    of all errors. *)
val wrap_errors : string -> ('a, 'b error) result -> ('a, 'b error) result

(** {3 Error-Monad Versions of Standard Library Functions} *)

(** Those functions are similar to their [Stdlib] counterpart, except that
    the function argument can return an error.

    Those functions stop at the first error.

    Another difference with the [Stdlib] counterparts is that function arguments are last.
    This allows to write e.g. [list_map_r list @@ fun item -> ...]. *)

(** Lift an iterator function into one that can handle errors. *)
val iter_r :
  (('a -> 'b) -> 'c -> 'd) ->
  'c ->
  ('a -> ('b, 'e error) result) ->
  ('d, 'e error) result

(** Error-monad version of [List.iter]. *)
val list_iter_r :
  'a list -> ('a -> (unit, 'b error) result) -> (unit, 'b error) result

(** Error-monad version of [List.map]. *)
val list_map_r : 'b list -> ('b -> ('a, 'c) result) -> ('a list, 'c) result

(** {2 Pretty-Printing} *)

module PP : sig
  (** Pretty-printing values using OCaml syntax, with indentation.

      This is an alternative to the [Format] module.
      With [PP] you only have to write functions to embed values;
      you do not have to think about opening boxes at all. *)

  (** Values. *)
  type t =
    | Bool of bool
    | Char of char
    | Int of int
    | Float of float
    | String of string
    | List of t list
    | Variant of string * t list
    | Tuple of t list
    | Record of (string * t) list

  (** Pretty-print a value.

      The result is valid OCaml code, which can be convenient when debugging. *)
  val pp : Format.formatter -> t -> unit
end
