(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** JSON decoding. *)

(** Errors that can happen when parsing or reading JSON. *)
type error

(** Convert a JSON parse error to a string. *)
val show_error : error -> string

(** Exception that can happen when parsing or reading JSON. *)
exception Error of error

(** JSON unannotated ASTs. *)
type u = Ezjsonm.value

(** JSON ASTs annotated with their origin.

    The origin is a string you give to {!annotate} or {!parse},
    such as ["RPC response"]. It denotes where the value comes from.
    It is used in error messages (see the comment in the Decoding section). *)
type t

(** Raise [Error].

    The [JSON.t] argument is used to annotate the error message with
    the location of the JSON value. *)
val error : t -> ('a, unit, string, 'b) format4 -> 'a

(** Annotate a JSON AST with its origin. *)
val annotate : origin:string -> u -> t

(** Remove annotation from an annotated JSON AST. *)
val unannotate : t -> u

(** {2 Encoding} *)

(** Encode a JSON annotated AST into a string. *)
val encode : t -> string

(** Encode a JSON unannotated AST into a string. *)
val encode_u : u -> string

(** {2 Decoding} *)

(** Parse a JSON file.

    @raise Error if the input is invalid JSON. *)
val parse_file : string -> t

(** Parse a JSON string.

    @raise Error if the input is invalid JSON. *)
val parse : origin:string -> string -> t

(** Same as [parse], but return [None] instead of raising [Error]. *)
val parse_opt : origin:string -> string -> t option

(** The general pattern for the accessors below is that only [as_x] functions can fail.
    Getters [get] and [geti] return [`Null] instead of failing.
    This allows to chain them and only test for errors at the end with [as_x],
    either by raising [Error] or by returning [None] (with the [as_x_opt] variant).

    Internally, the actual error which is printed is the correct one.
    For instance, with [json |-> "x" |> as_int], if [json] is not an object,
    the call to [as_int] causes the test to fail with ["<origin>: not an object"]
    where [<origin>] is the [~origin] you gave to {!parse}.
    If [json] is an object but ["x"] does not exist, [as_int] causes the test to fail
    with ["<origin>: missing field: x"]. If ["x"] exists but is not an integer, [as_int]
    causes the test to fail with ["<origin> at .x: expected an integer"]. *)

(** Get the value for a field of a JSON object.

    If the JSON value is not an object, or if the field does not exist, return [`Null]. *)
val get : string -> t -> t

(** Same as {!get}, with the arguments reversed. *)
val ( |-> ) : t -> string -> t

(** Get the value for a field of a JSON array.

    If the JSON value is not an array, or if the field does not exist, return [`Null]. *)
val geti : int -> t -> t

(** Same as {!geti}, with the arguments reversed. *)
val ( |=> ) : t -> int -> t

(** Updates an object with a [(key, value)] pair.

    [put (key, value) obj] puts [value] under [key] in [obj]. If the [key]
    already exists, it is overwritten. Otherwise a new key is added at the end
    of the object.

    @raise Error if [obj] is not a JSON object. *)
val put : string * t -> t -> t

(** Alters the value of a specific key in a JSON object by applying its value to a
    function. Returns updated object.

    [update key f obj] is equivalent to [put (key, f (get key obj)) obj].

    Note: if [key] is not present in [obj], [`Null] is passed to [f] instead.
    @raise Error if [obj] is not an object. *)
val update : string -> (t -> t) -> t -> t

(** Test whether a JSON value is [`Null]. *)
val is_null : t -> bool

(** Return [None] if a JSON value is [`Null], [Some] otherwise.

    Example: [JSON.(json |> as_opt |> Option.map read_record)] *)
val as_opt : t -> t option

(** Get the value from a [`Bool] node.

    @raise Error if the input is not a [`Bool]. *)
val as_bool : t -> bool

(** Same as [as_bool], but return [None] instead of raising [Error]. *)
val as_bool_opt : t -> bool option

(** Test whether [as_bool] would succeed. *)
val is_bool : t -> bool

(** Get the integer value from a [`Float] or [`String] node.

    @raise Error if:
    - the input is not a [`Float] nor a [`String];
    - the input is a [`Float] but is not an integer;
    - the input is a [`String] but does not denote a valid decimal integer. *)
val as_int : t -> int

(** Same as [as_int], but return [None] instead of raising [Error]. *)
val as_int_opt : t -> int option

(** Test whether [as_int] would succeed. *)
val is_int : t -> bool

(** Get the integer value from a [`Float] or [`String] node (64-bit version).

    @raise Error if:
    - the input is not a [`Float] nor a [`String];
    - the input is a [`Float] but is not an integer;
    - the input is a [`String] but does not denote a valid decimal integer. *)
val as_int64 : t -> int64

(** Same as [as_int64], but return [None] instead of raising [Error]. *)
val as_int64_opt : t -> int64 option

(** Test whether [as_int64] would succeed. *)
val is_int64 : t -> bool

(** Get the float value from a [`Float] or [`String] node.

    @raise Error if:
    - the input is not a [`Float] nor a [`String];
    - the input is a [`String] but does not denote a valid decimal float. *)
val as_float : t -> float

(** Same as [as_float], but return [None] instead of raising [Error]. *)
val as_float_opt : t -> float option

(** Test whether [as_float] would succeed. *)
val is_float : t -> bool

(** Get the value from a [`String] node.

    @raise Error if the input is not a [`String]. *)
val as_string : t -> string

(** Same as [as_string], but return [None] instead of raising [Error]. *)
val as_string_opt : t -> string option

(** Test whether [as_string] would succeed. *)
val is_string : t -> bool

(** Get the list of items from an [`Array] node.

    @raise Error if the input is not an [`Array] nor [`Null].
    Return the empty list if the input is [`Null]. *)
val as_list : t -> t list

(** Same as [as_list], but return [None] instead of raising [Error]. *)
val as_list_opt : t -> t list option

(** Test whether [as_list] would succeed. *)
val is_list : t -> bool

(** Get the list of fields from an [`Object] node.

    @raise Error if the input is not an [`Object] nor [`Null].
    Return the empty list if the input is [`Null]. *)
val as_object : t -> (string * t) list

(** Same as [as_object], but return [None] instead of raising [Error]. *)
val as_object_opt : t -> (string * t) list option

(** Test whether [as_object] would succeed. *)
val is_object : t -> bool
