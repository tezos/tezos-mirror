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

    [Hex] defines hexadecimal encodings for {{!char}characters},
    {{!string}strings} and {{!cstruct}Cstruct.t} buffers. *)

type t = [`Hex of string]
(** The type var hexadecimal values. *)

(** {1:char Characters} *)

val of_char: char -> char * char
(** [of_char c] is the the hexadecimal encoding of the character
    [c]. *)

val to_char: char -> char -> char option
(** [to_char x y] is the character corresponding to the [xy]
    hexadecimal encoding.

    Returns [None] if [x] or [y] are not in the ranges ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1:string Strings} *)

val of_string: ?ignore:char list -> string -> t
(** [of_string s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_string ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_string: t -> string option
(** [to_string t] is the binary string [s] such that [of_string s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1:byte Bytes} *)

val of_bytes: ?ignore:char list -> bytes -> t
(** [of_bytes s] is the hexadecimal representation of the binary
    string [s]. If [ignore] is set, skip the characters in the list
    when converting. Eg [of_bytes ~ignore:[' '] "a f"]. The default
    value of [ignore] is [[]]). *)

val to_bytes: t -> bytes option
(** [to_bytes t] is the binary string [s] such that [of_bytes s] is
    [t].

    Returns [None] if [t] contains a character that is not in the range ['0'..'9'],
    ['a'..'f'], or ['A'..'F']. *)

(** {1 Debugging} *)

val hexdump_s: ?print_row_numbers:bool -> ?print_chars:bool -> t -> string
(** Same as [hexdump] except returns a string. *)

(** {1 Pretty printing} *)

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
(** [pp fmt t] will output a human-readable hex representation of [t]
    to the formatter [fmt]. *)

val show : t -> string
(** [show t] will return a human-readable hex representation of [t] as
    a string. *)
