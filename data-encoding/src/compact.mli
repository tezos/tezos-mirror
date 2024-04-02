(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type 'a t

val make : ?tag_size:[`Uint0 | `Uint8 | `Uint16] -> 'a t -> 'a Encoding.t

val tag_bit_count : 'a t -> int

type void

val void : void t

val refute : void -> 'a

val unit : unit t

val null : unit t

val bool : bool t

val payload : 'a Encoding.t -> 'a t

val option : 'a t -> 'a option t

val conv : ?json:'a Encoding.t -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

val tup1 : 'a t -> 'a t

val tup2 : 'a t -> 'b t -> ('a * 'b) t

val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val tup5 :
  'f1 t -> 'f2 t -> 'f3 t -> 'f4 t -> 'f5 t -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5) t

val tup6 :
  'f1 t ->
  'f2 t ->
  'f3 t ->
  'f4 t ->
  'f5 t ->
  'f6 t ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) t

val tup7 :
  'f1 t ->
  'f2 t ->
  'f3 t ->
  'f4 t ->
  'f5 t ->
  'f6 t ->
  'f7 t ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) t

val tup8 :
  'f1 t ->
  'f2 t ->
  'f3 t ->
  'f4 t ->
  'f5 t ->
  'f6 t ->
  'f7 t ->
  'f8 t ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) t

val tup9 :
  'f1 t ->
  'f2 t ->
  'f3 t ->
  'f4 t ->
  'f5 t ->
  'f6 t ->
  'f7 t ->
  'f8 t ->
  'f9 t ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) t

val tup10 :
  'f1 t ->
  'f2 t ->
  'f3 t ->
  'f4 t ->
  'f5 t ->
  'f6 t ->
  'f7 t ->
  'f8 t ->
  'f9 t ->
  'f10 t ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) t

type 'a field

val req : string -> 'a t -> 'a field

val opt : string -> 'a t -> 'a option field

val obj1 : 'a field -> 'a t

val obj2 : 'a field -> 'b field -> ('a * 'b) t

val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t

val obj4 : 'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

val obj5 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) t

val obj6 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) t

val obj7 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) t

val obj8 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) t

val obj9 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  'f9 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) t

val obj10 :
  'f1 field ->
  'f2 field ->
  'f3 field ->
  'f4 field ->
  'f5 field ->
  'f6 field ->
  'f7 field ->
  'f8 field ->
  'f9 field ->
  'f10 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) t

val int32 : int32 t

val int64 : int64 t

val list : bits:int -> 'a Encoding.t -> 'a list t

type 'a case

val case :
  title:string ->
  ?description:string ->
  'b t ->
  ('a -> 'b option) ->
  ('b -> 'a) ->
  'a case

val void_case : title:string -> 'a case

val union : ?union_tag_bits:int -> ?cases_tag_bits:int -> 'a case list -> 'a t

val or_int32 :
  int32_title:string ->
  alt_title:string ->
  ?alt_description:string ->
  'a Encoding.t ->
  (int32, 'a) Either.t t

val splitted : json:'a Encoding.t -> compact:'a t -> 'a t

module Custom : sig
  type tag = int

  val join_tags : (tag * int) list -> tag

  module type S = sig
    type input

    type layout

    val layouts : layout list

    val tag_len : int

    val tag : layout -> tag

    val title : layout -> string option

    val partial_encoding : layout -> input Encoding.t

    val classify : input -> layout

    val json_encoding : input Encoding.t
  end

  val make : (module S with type input = 'a) -> 'a t
end
