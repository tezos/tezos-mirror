(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** [split delim ~limit str] splits [str] into a list of strings.
    Splitting occurs on [delim] characters (which are removed from output)
    at most [limit] times. Remaining [delim] characters are included in the
    last element of the resulting list.

    [limit] defaults to [max_int].

    This function obeys the invariant that for all [limit]s, [delim]s and [str]s:
    [String.concat (String.init 1 (fun _ -> delim)) (split delim ~limit str) = str].*)
val split : char -> ?limit:int -> string -> string list

(** Splits a string on a delimiter character. It strips delimiters at the
    beginning and at the end. It considers groups of delimiters as one. If
    [limit] is passed, stops after [limit] split(s). [limit] defaults to
    [max_int]. It's guaranteed to never produce empty strings in the output.
    Therefore, it's capable of producing an empty list as a result.

    For example,
    [split_no_empty ',' ",hello,,world,"] returns ["hello"; "world"] *)
val split_no_empty : char -> ?limit:int -> string -> string list

(** [chunk_bytes n b] chunks the sequence of bytes [b] into a list of strings,
    each of length [n]. The last chunk may be a non-empty string of length less
    than [n], in which case the behaviour of the function depends on whether
    [error_on_partial_chunk] is set:
      {ul
        {li If [error_on_partial_chunk] is set, then the function returns
        [Error error_on_partial_chunk],}
        {li Otherwise, the function return the list of chunks, where the
        last chunk is a non-empty string of length less than [n].}
      }

    @raise Invalid_argument if [n <= 0]. *)
val chunk_bytes :
  ?error_on_partial_chunk:'a -> int -> bytes -> (string list, 'a) result

(** [true] if input has prefix *)
val has_prefix : prefix:string -> string -> bool

(** Some (input with [prefix] removed), if string has [prefix], else [None] *)
val remove_prefix : prefix:string -> string -> string option

(** Some (input with [suffix] removed), if string has [suffix], else [None] *)
val remove_suffix : suffix:string -> string -> string option

(** Length of common prefix of input strings *)
val common_prefix : string -> string -> int

(** Test whether a string contains a given character *)
val mem_char : string -> char -> bool

(** Functional iteration over the characters of a string from first to last *)
val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a

(** Pretty print bytes as hexadecimal string. *)
val pp_bytes_hex : Format.formatter -> bytes -> unit
