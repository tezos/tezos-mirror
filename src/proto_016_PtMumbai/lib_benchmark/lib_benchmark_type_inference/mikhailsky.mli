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

open Protocol

(** Mikhailsky: Michelson in Micheline form, with typed holes and annotations.
    Mikhailsky terms are hash-consed. *)

(**
   Michelson code is a hard to type-check and generate incrementally due to
   the presence of ambiguous constructs, such as literals
   like [{ 1 ; 2 ; 3 }]. Is it a list of ints? of nats? of tez? Or a set?

   Thus, we will work with Mikhailsky, a better behaved version of Michelson
   allowing local reconstruction of types.

   Differences wrt Michelson:

   1. non string/byte literals are explicitly annotated with their head type constructor.
   Here is an int i: Prim (_, D_int, [Int i], _)
   Here is an nat n: Prim (_, D_nat, [Int i], _)
   Here is an list of something: Prim (_, D_list, michelson_list, _)
   Here is a set: Prim (_, D_set, michelson_set, _)
   Here is a map: Prim (_, D_map, michelson_map, _)
   etc.
   Projecting back from this language to Michelson is trivial.

   2. Instructions `LEFT/RIGHT` do not need to carry the type of the other
      component of the disjunction. These has to be filled in back when
      generating Michelson from Mikhailsky.

   4. The same holds for the input/output type of a lambda as specified in the
      `LAMBDA` instruction.

   3. Some instructions are annotated with the type on which they operate.
      Eg if Prim (_, I_ADD, [], []) is the (ad-hoc polymorphic) addition in Michelson,
      we will have the following variants in Mikhailsky:
      - Prim (_, I_ADD, [ Prim (_, T_mutez, [], []),
                          Prim (_, T_mutez, [], []) ], []) for mutez addition
      - Prim (_, I_ADD, [ Prim (_, T_int, [], []),
                          Prim (_, T_nat, [], []) ], []) for int+nat addition
      etc.
*)

(** The signature of Mikhailsky terms. *)
module Mikhailsky_signature :
  Algebraic_signature.S with type t = Mikhailsky_prim.prim

(** Elements of type [Path.t] allow to index subterms of Mikhailsky terms. *)
module Path : Path.S

(** The following types correspond to those provided when instantiating the
    functor [Micheline_with_hash_consing.Make] on [Mikhailsky_signature]. *)
type label = Micheline_with_hash_consing.hcons_info

type head = Mikhailsky_signature.t

type node = (label, head) Micheline.node

exception Term_contains_holes

exception Ill_formed_mikhailsky

(** [parse_ty] returns a type from a Mikhailsky term. *)
val parse_ty :
  allow_big_map:bool ->
  allow_operation:bool ->
  allow_contract:bool ->
  node ->
  Type.Base.t

(** [map_var f x] maps the function f on all variables contained
 in the type [x]. *)
val map_var : (int -> node) -> Type.Base.t -> node

(** [unparse_ty] returns a Mikhailsky term representing a type. *)
val unparse_ty_exn : Type.Base.t -> node

val unparse_ty : Type.Base.t -> node option

(** Extracts a Michelson term from a Mikhailsky one. Raises
    [Term_contains_holes] if it cannot be done. *)
val to_michelson : node -> Script_repr.expr

(** Pretty printer. *)
val pp : Format.formatter -> node -> unit

val to_string : node -> string

(** Returns the number of nodes of a Mikhailsky term. *)
val size : node -> int

(** Micheline generic constructors *)
val prim : Mikhailsky_prim.prim -> node list -> string list -> node

val seq : node list -> node

val string : string -> node

val bytes : Bytes.t -> node

(** Mikhailsky smart constructors*)

(** Holes *)
val instr_hole : node

val data_hole : node

(** Types *)
val unit_ty : node

val int_ty : node

val nat_ty : node

val bool_ty : node

val string_ty : node

val bytes_ty : node

val key_hash_ty : node

val option_ty : node -> node

val list_ty : node -> node

(** Project unique tag out of Mikhailsky node *)
val tag : node -> int

(** Project hash out of Mikhailsky node *)
val hash : node -> int

(** Instructions *)
module Instructions : sig
  (** Arithmetic. Binary operations take the input types as extra arguments. *)
  val add : node -> node -> node

  val sub : node -> node -> node

  val mul : node -> node -> node

  val ediv : node -> node -> node

  val abs : node

  val gt : node

  (** Stack *)
  val push : node -> node -> node

  val dip : node -> node

  val dup : node

  val drop : node

  val dropn : int -> node

  val swap : node

  (** Crypto *)
  val blake2b : node

  val sha256 : node

  val sha512 : node

  val hash_key : node

  (** Control *)
  val if_ : node -> node -> node

  val if_left : node -> node -> node

  val if_none : node -> node -> node

  val loop : node -> node

  val loop_left : node -> node

  (** Pairs *)
  val car : node

  val cdr : node

  val pair : node

  (** Unions *)
  val left : node

  val right : node

  (** Booleans *)
  val and_ : node

  (** Compare *)
  val compare : node

  (** Set/Map *)
  val empty_set : node

  val update_set : node

  val size_set : node

  val iter_set : node list -> node

  val mem_set : node

  val empty_map : node

  val update_map : node

  val size_map : node

  val iter_map : node list -> node

  val map_map : node list -> node

  val get_map : node

  val mem_map : node

  (** Lists *)
  val nil : node

  val cons : node

  val size_list : node

  val iter_list : node list -> node

  val map_list : node list -> node

  (** Strings/bytes *)
  val concat : node

  val size_string : node

  val size_bytes : node

  (** Lambdas *)
  val lambda : node list -> node

  val exec : node

  val apply : node

  (** pack/unpack *)

  val pack : node

  val unpack : node

  (** Hole *)
  val hole : node
end

(** data *)
module Data : sig
  val unit : node

  val false_ : node

  val true_ : node

  val none : node

  val some : node -> node

  val pair : node -> node -> node

  val left : node -> node

  val right : node -> node

  val list : node list -> node

  val set : node list -> node

  val map_elt : node -> node -> node

  val map : node list -> node

  val timestamp : Script_timestamp.t -> node

  val mutez : Alpha_context.Tez.t -> node

  val key_hash : Environment.Signature.Public_key_hash.t -> node

  val key : Environment.Signature.Public_key.t -> node

  val integer : int -> node

  val natural : int -> node

  val big_integer : Z.t -> node

  val big_natural : Z.t -> node

  val string : string -> node

  val bytes : Bytes.t -> node

  val lambda : node list -> node

  val hole : node
end
