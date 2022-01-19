(*
 * Copyright (c) 2015 Trevor Summers Smith <trevorsummerssmith@gmail.com>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Hexadecimal encoding.

[TzHex] defines hexadecimal encodings for {{!char}characters},
{{!string}strings} and {{!cstruct}Cstruct.t} buffers. *)
type t = [`Hex of string]
(** The type var hexadecimal values. *)

(** {1:char Characters} *)

(** [of_char c] is the the hexadecimal encoding of the character
    [c]. *)
val of_char : char -> char * char

(** [to_char x y] is the character corresponding to the [xy]
    hexadecimal encoding.

    Returns [None] if [x] or [y] are not in the ranges ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)
val to_char : char -> char -> char option

(** {1:string Strings} *)

(** [of_string s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_string ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)
val of_string : ?ignore:char list -> string -> t

(** [to_string t] is the binary string [s] such that [of_string s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)
val to_string : t -> string option

(** {1:byte Bytes} *)

(** [of_bytes s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_bytes ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)
val of_bytes : ?ignore:char list -> bytes -> t

(** [to_bytes t] is the binary string [s] such that [of_bytes s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)
val to_bytes : t -> bytes option

(** [to_bytes_exn t] is the binary string [s] such that [of_bytes s] is
    [t].

    @raise Invalid_argument instead of returning [None]. *)
val to_bytes_exn : t -> bytes

(** {1 Debugging} *)

(** Same as [hexdump] except returns a string. *)
val hexdump_s : ?print_row_numbers:bool -> ?print_chars:bool -> t -> string

(** {1 Pretty printing} *)

(** [pp fmt t] will output a human-readable hex representation of [t]
    to the formatter [fmt]. *)
val pp : Format.formatter -> t -> unit
  [@@ocaml.toplevel_printer]

(** [show t] will return a human-readable hex representation of [t] as
    a string. *)
val show : t -> string
