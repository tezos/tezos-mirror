(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Michelson types, hash-consed. *)

(** Base types *)
module Base : sig
  type comparable_tag = Comparable | Maybe_not_comparable

  type t = t_node Hashcons.hash_consed

  and t_node = private
    | Unit_t
    | Var_t of int
    | Int_t
    | Nat_t
    | Bool_t
    | String_t
    | Bytes_t
    | Key_hash_t
    | Timestamp_t
    | Mutez_t
    | Key_t
    | Option_t of t
    | Pair_t of t * t
    | Or_t of t * t
    | List_t of t
    | Set_t of t
    | Map_t of t * t
    | Lambda_t of t * t

  val pp : Format.formatter -> t -> unit

  val vars : t -> int list
end

(** Stack types *)
module Stack : sig
  type t = t_node Hashcons.hash_consed

  and t_node = private Empty_t | Stack_var_t of int | Item_t of Base.t * t

  val pp : Format.formatter -> t -> unit

  val vars : t -> int option
end

(** Smart constructors *)
val unit : Base.t

val var : int -> Base.t

val int : Base.t

val nat : Base.t

val bool : Base.t

val string : Base.t

val bytes : Base.t

val key_hash : Base.t

val timestamp : Base.t

val mutez : Base.t

val key : Base.t

val option : Base.t -> Base.t

val pair : Base.t -> Base.t -> Base.t

val or_ : Base.t -> Base.t -> Base.t

val list : Base.t -> Base.t

val set : Base.t -> Base.t

val map : Base.t -> Base.t -> Base.t

val lambda : Base.t -> Base.t -> Base.t

val empty : Stack.t

val stack_var : int -> Stack.t

val item : Base.t -> Stack.t -> Stack.t
