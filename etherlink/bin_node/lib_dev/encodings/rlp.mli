(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** A simple RLP encoding library, see
    https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
    for the specification. *)

type error += Rlp_decoding_error of string

(** An RLP value is either a bytes value, or a list of RLP values. *)
type item = Value of bytes | List of item list

(** [encode_int i] encodes an integer in big endian in its smallest representation. *)
val encode_int : int -> bytes

(** [encode_z z] encodes a big integer in big endian in its smallest
    representation. *)
val encode_z : Z.t -> bytes

(** [encode item] takes an RLP [item] and returns its encoded form. *)
val encode : item -> bytes

(** [decode_int bytes] decodes an integer encoded in big endian from the given
    [bytes]. Returns an {!Rlp_decoding_error} if the bytes is not a valid RLP encoded
    integer. *)
val decode_int : bytes -> int tzresult

(** [decode_z bytes] decodes a big integer encoded in big endian from the given
    [bytes]. Returns an {!Rlp_decoding_error} if the bytes is not a valid RLP encoded
    big integer. *)
val decode_z : bytes -> Z.t tzresult

(** [decode bytes] decodes an RLP value from the given [bytes]. Returns an
    {!Rlp_decoding_error} if the bytes is not an RLP encoded value. *)
val decode : bytes -> item tzresult

(** [decode_exn bytes] calls {!decode} and raises [Invalid_argument] if it
    fails to decode. *)
val decode_exn : bytes -> item

(** [decode_option decode_value optional_value] decodes the option following
    Rust's RLP encoding. *)
val decode_option : (item -> 'a tzresult) -> item -> 'a option tzresult

(** [decode_result decode_ok decode_error value] decodes an encoded result type. *)
val decode_result :
  (item -> 'a tzresult) ->
  (item -> 'b tzresult) ->
  item ->
  ('a, 'b) result tzresult

(** [decode_value decode item] decodes an encapsulated value. *)
val decode_value : (bytes -> 'a tzresult) -> item -> 'a tzresult

(** [decode_list decode_item list_item] decodes a list using [decode_item] on 
   each list element. *)
val decode_list : (item -> 'a tzresult) -> item -> 'a list tzresult

(** [filter_decode_list filter_decode list] decodes and filter the item in a list:
    If [filter_decode item] is [None] then the item is filtered out, else it's 
    decoded. Based on [List.filter_map] so slightly faster than two steps. *)
val filter_decode_list : (item -> 'a option) -> item -> 'a list tzresult

(** [decode_as_bytes item] returns the bytes of [item] if it's a value, and 
    returns a {!Rlp_decoding_error} if it's a list. *)
val decode_as_bytes : item -> bytes tzresult

(** [pp ppf item] pretty-prints an item. *)
val pp : Format.formatter -> item -> unit
