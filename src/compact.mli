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

(** This is for internal use in [Data_encoding] only. *)

type 'a t

val make : ?tag_size:[`Uint8 | `Uint16] -> 'a t -> 'a Encoding.t

type void

val void : void t

val refute : void -> 'a

val empty : unit t

val bool : bool t

val payload : 'a Encoding.t -> 'a t

val option : 'a t -> 'a option t

val conv : ?json:'a Encoding.t -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

val tup1 : 'a t -> 'a t

val tup2 : 'a t -> 'b t -> ('a * 'b) t

val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

type 'a field

val req : string -> 'a t -> 'a field

val opt : string -> 'a t -> 'a option field

val obj1 : 'a field -> 'a t

val obj2 : 'a field -> 'b field -> ('a * 'b) t

val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t

val obj4 : 'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

val int32 : int32 t

val int64 : int64 t

val list : bits:int -> 'a Encoding.t -> 'a list t

type 'a case

val case : string -> ('a -> 'b option) -> ('b -> 'a) -> 'b t -> 'a case

val union : ?union_tag_bits:int -> ?cases_tag_bits:int -> 'a case list -> 'a t

val or_int32 :
  int32_kind:string ->
  alt_kind:string ->
  'a Encoding.t ->
  (int32, 'a) Either.t t

module Custom : sig
  type tag = int32

  val join_tags : (tag * int) list -> tag

  module type S = sig
    type input

    type layout

    val layouts : layout list

    val tag_len : int

    val tag : layout -> tag

    val partial_encoding : layout -> input Encoding.t

    val classify : input -> layout

    val json_encoding : input Encoding.t
  end

  val make : (module S with type input = 'a) -> 'a t
end
