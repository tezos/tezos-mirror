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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

type jsonm_lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe ]

val small_string_seq_of_jsonm_lexeme_seq :
  newline:bool -> jsonm_lexeme Seq.t -> string Seq.t

val string_seq_of_jsonm_lexeme_seq :
  newline:bool -> chunk_size_hint:int -> jsonm_lexeme Seq.t -> string Seq.t

(** [blit_instructions_seq_of_jsonm_lexeme_seq ~newline ~buffer json]
    is a sequence of [(buff, offset, length)] such that the concatenation of the
    sub-strings thus designated represents the json value in text form.

    The intended use is to blit each of the substring onto whatever output the
    consumer decides. In most cases, the Sequence's [buff] is physically equal
    to [buffer]. This is not always true and one cannot rely on that fact. E.g.,
    when the json includes a long string literal, the function might instruct
    the consumer to blit from that literal directly.

    This function performs few allocations, especially of fresh strings.

    Note that once the next element of the sequence is forced, the blit
    instructions become invalid: the content of [buff] may have been rewritten
    by the side effect of forcing the next element.

    @raise Invalid_argument if [Bytes.length buffer] is less than 32. *)
val blit_instructions_seq_of_jsonm_lexeme_seq :
  newline:bool ->
  buffer:bytes ->
  jsonm_lexeme Seq.t ->
  (Bytes.t * int * int) Seq.t
