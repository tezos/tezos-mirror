(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Alpha_context
open Script_int

(* Preliminary definitions. *)

type var_annot = Var_annot of string

type type_annot = Type_annot of string

type field_annot = Field_annot of string

type never = |

type address = Contract.t * string

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) union = L of 'a | R of 'b

type operation = packed_internal_operation * Lazy_storage.diffs option

type 'a ticket = {ticketer : address; contents : 'a; amount : n num}

type end_of_stack = unit * unit

type _ comparable_ty =
  | Unit_key : type_annot option -> unit comparable_ty
  | Never_key : type_annot option -> never comparable_ty
  | Int_key : type_annot option -> z num comparable_ty
  | Nat_key : type_annot option -> n num comparable_ty
  | Signature_key : type_annot option -> signature comparable_ty
  | String_key : type_annot option -> string comparable_ty
  | Bytes_key : type_annot option -> Bytes.t comparable_ty
  | Mutez_key : type_annot option -> Tez.t comparable_ty
  | Bool_key : type_annot option -> bool comparable_ty
  | Key_hash_key : type_annot option -> public_key_hash comparable_ty
  | Key_key : type_annot option -> public_key comparable_ty
  | Timestamp_key : type_annot option -> Script_timestamp.t comparable_ty
  | Chain_id_key : type_annot option -> Chain_id.t comparable_ty
  | Address_key : type_annot option -> address comparable_ty
  | Pair_key :
      ('a comparable_ty * field_annot option)
      * ('b comparable_ty * field_annot option)
      * type_annot option
      -> ('a, 'b) pair comparable_ty
  | Union_key :
      ('a comparable_ty * field_annot option)
      * ('b comparable_ty * field_annot option)
      * type_annot option
      -> ('a, 'b) union comparable_ty
  | Option_key :
      'v comparable_ty * type_annot option
      -> 'v option comparable_ty

module type Boxed_set = sig
  type elt

  val elt_ty : elt comparable_ty

  module OPS : S.SET with type elt = elt

  val boxed : OPS.t

  val size : int
end

type 'elt set = (module Boxed_set with type elt = 'elt)

module type Boxed_map = sig
  type key

  type value

  val key_ty : key comparable_ty

  module OPS : S.MAP with type key = key

  val boxed : value OPS.t * int
end

type ('key, 'value) map =
  (module Boxed_map with type key = 'key and type value = 'value)

module Big_map_overlay = Map.Make (struct
  type t = Script_expr_hash.t

  let compare = Script_expr_hash.compare
end)

type ('key, 'value) big_map_overlay = {
  map : ('key * 'value option) Big_map_overlay.t;
  size : int;
}

and 'elt boxed_list = {elements : 'elt list; length : int}

and ('arg, 'storage) script = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage : 'storage;
  storage_type : 'storage ty;
  root_name : field_annot option;
}

(* ---- Instructions --------------------------------------------------------*)

(*

   The instructions of Michelson are represented in the following
   Generalized Algebraic Datatypes.

   There are three important aspects in that type declaration.

   First, we follow a tagless approach for values: they are directly
   represented as OCaml values. This reduces the computational cost of
   interpretation because there is no need to check the shape of a
   value before applying an operation to it. To achieve that, the GADT
   encodes the typing rules of the Michelson programming
   language. This static information is sufficient for the typechecker
   to justify the absence of runtime checks.  As a bonus, it also
   ensures that well-typed Michelson programs cannot go wrong: if the
   interpreter typechecks then we have the static guarantee that no
   stack underflow or type error can occur at runtime.

   Second, we maintain the invariant that the stack type always has a
   distinguished topmost element. This invariant is important to
   implement the stack as an accumulator followed by a linked list of
   cells, a so-called A-Stack. This representation is considered in
   the literature[1] as an efficient representation of the stack for a
   stack-based abstract machine, mainly because this opens the
   opportunity for the accumulator to be stored in a hardware
   register. In the GADT, this invariant is encoded by representing
   the stack type using two parameters instead of one: the first one
   is the type of the accumulator while the second is the type of the
   rest of the stack.

   Third, in this representation, each instruction embeds its
   potential successor instructions in the control flow. This design
   choice permits an efficient implementation of the continuation
   stack in the interpreter. Assigning a precise type to this kind of
   instruction which is a cell in a linked list of instructions is
   similar to the typing of delimited continuations: we need to give a
   type to the stack ['before] the execution of the instruction, a
   type to the stack ['after] the execution of the instruction and
   before the execution of the next, and a type for the [`result]ing
   stack type after the execution of the whole chain of instructions.

   Combining these three aspects, the type [kinstr] needs four
   parameters:

   ('before_top, 'before, 'result_top, 'result) kinstr

   Notice that we could have chosen to only give two parameters to
   [kinstr] by manually enforcing each argument to be a pair but this
   is error-prone: with four parameters, this constraint is enforced
   by the arity of the type constructor itself.

   Hence, an instruction which has a successor instruction enjoys a
   type of the form:

   ... * ('after_top, 'after, 'result_top, 'result) kinstr * ... ->
   ('before_top, 'before, 'result_top, 'result) kinstr

   where ['before_top] and ['before] are the types of the stack top
   and rest before the instruction chain, ['after_top] and ['after]
   are the types of the stack top and rest after the instruction
   chain, and ['result_top] and ['result] are the types of the stack
   top and rest after the instruction chain. The [IHalt] instruction
   ends a sequence of instructions and has no successor, as shown by
   its type:

   IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr

   Each instruction is decorated by some metadata (typically to hold
   locations). The type for these metadata is [kinfo].

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...  to assign
   types to stack cell contents while we use 's, 't, 'u, 'v, ... to
   assign types to stacks.

   The types for the final result and stack rest of a whole sequence
   of instructions are written 'r and 'f (standing for "result" and
   "final stack rest", respectively).

   Instructions for internal execution steps
   =========================================

   Some instructions encoded in the following type are not present in the
   source language. They only appear during evaluation to account for
   intermediate execution steps. Indeed, since the interpreter follows
   a small-step style, it is sometimes necessary to decompose a
   source-level instruction (e.g. List_map) into several instructions
   with smaller steps. This technique seems required to get an
   efficient tail-recursive interpreter.

   References
   ==========

   [1]: http://www.complang.tuwien.ac.at/projects/interpreters.html

   References
   ==========
   [1]: http://www.complang.tuwien.ac.at/projects/interpreters.html

*)
and ('before_top, 'before, 'result_top, 'result) kinstr =
  (*
     Stack
     -----
  *)
  | IDrop :
      ('a, 'b * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDup :
      ('a, 's) kinfo * ('a, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISwap :
      ('a, 'b * 's) kinfo * ('b, 'a * 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IConst :
      ('a, 's) kinfo * 'ty * ('ty, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  (*
     Pairs
     -----
  *)
  | ICons_pair :
      ('a, 'b * 's) kinfo * ('a * 'b, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | ICar :
      ('a * 'b, 's) kinfo * ('a, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | ICdr :
      ('a * 'b, 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  | IUnpair :
      ('a * 'b, 's) kinfo * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr
  (*
     Options
     -------
   *)
  | ICons_some :
      ('v, 's) kinfo * ('v option, 's, 'r, 'f) kinstr
      -> ('v, 's, 'r, 'f) kinstr
  | ICons_none :
      ('a, 's) kinfo * 'b ty * ('b option, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IIf_none :
      ('a option, 'b * 's) kinfo
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      * ('b, 's, 'r, 'f) kinstr
      * ('a, 'b * 's, 'r, 'f) kinstr
      -> ('a option, 'b * 's, 'r, 'f) kinstr
  (*
     Unions
     ------
   *)
  | ICons_left :
      ('a, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ICons_right :
      ('b, 's) kinfo * (('a, 'b) union, 's, 'r, 'f) kinstr
      -> ('b, 's, 'r, 'f) kinstr
  | IIf_left :
      (('a, 'b) union, 's) kinfo
      (* See remark in IIf_none. *)
      * ('a, 's, 'r, 'f) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  (*
     Lists
     -----
  *)
  | ICons_list :
      ('a, 'a boxed_list * 's) kinfo * ('a boxed_list, 's, 'r, 'f) kinstr
      -> ('a, 'a boxed_list * 's, 'r, 'f) kinstr
  | INil :
      ('a, 's) kinfo * ('b boxed_list, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IIf_cons :
      ('a boxed_list, 'b * 's) kinfo
      (* See remark in IIf_none. *)
      * ('a, 'a boxed_list * ('b * 's), 'r, 'f) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | IList_map :
      ('a boxed_list, 'c * 's) kinfo
      * ('a, 'c * 's, 'b, 'c * 's) kinstr
      * ('b boxed_list, 'c * 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'c * 's, 'r, 'f) kinstr
  | IList_iter :
      ('a boxed_list, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 'b * 's, 'r, 'f) kinstr
  | IList_size :
      ('a boxed_list, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a boxed_list, 's, 'r, 'f) kinstr
  (*
    Sets
    ----
  *)
  | IEmpty_set :
      ('a, 's) kinfo * 'b comparable_ty * ('b set, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISet_iter :
      ('a set, 'b * 's) kinfo
      * ('a, 'b * 's, 'b, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> ('a set, 'b * 's, 'r, 'f) kinstr
  | ISet_mem :
      ('a, 'a set * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, 'a set * 's, 'r, 'f) kinstr
  | ISet_update :
      ('a, bool * ('a set * 's)) kinfo * ('a set, 's, 'r, 'f) kinstr
      -> ('a, bool * ('a set * 's), 'r, 'f) kinstr
  | ISet_size :
      ('a set, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> ('a set, 's, 'r, 'f) kinstr
  (*
     Maps
     ----
   *)
  | IEmpty_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IMap_map :
      (('a, 'b) map, 'd * 's) kinfo
      * ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * (('a, 'c) map, 'd * 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'd * 's, 'r, 'f) kinstr
  | IMap_iter :
      (('a, 'b) map, 'c * 's) kinfo
      * ('a * 'b, 'c * 's, 'c, 's) kinstr
      * ('c, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 'c * 's, 'r, 'f) kinstr
  | IMap_mem :
      ('a, ('a, 'b) map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | IMap_get :
      ('a, ('a, 'b) map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) map * 's, 'r, 'f) kinstr
  | IMap_update :
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * (('a, 'b) map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
  | IMap_get_and_update :
      ('a, 'v option * (('a, 'v) map * 'rest)) kinfo
      * ('v option, ('a, 'v) map * 'rest, 'r, 'f) kinstr
      -> ('a, 'v option * (('a, 'v) map * 'rest), 'r, 'f) kinstr
  | IMap_size :
      (('a, 'b) map, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (('a, 'b) map, 's, 'r, 'f) kinstr
  (*
     Big maps
     --------
  *)
  | IEmpty_big_map :
      ('a, 's) kinfo
      * 'b comparable_ty
      * 'c ty
      * (('b, 'c) big_map, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IBig_map_mem :
      ('a, ('a, 'b) big_map * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | IBig_map_get :
      ('a, ('a, 'b) big_map * 's) kinfo * ('b option, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) big_map * 's, 'r, 'f) kinstr
  | IBig_map_update :
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * (('a, 'b) big_map, 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  | IBig_map_get_and_update :
      ('a, 'v option * (('a, 'v) big_map * 'rest)) kinfo
      * ('v option, ('a, 'v) big_map * 'rest, 'r, 'f) kinstr
      -> ('a, 'v option * (('a, 'v) big_map * 'rest), 'r, 'f) kinstr
  (*
     Strings
     -------
  *)
  | IConcat_string :
      (string boxed_list, 's) kinfo * (string, 's, 'r, 'f) kinstr
      -> (string boxed_list, 's, 'r, 'f) kinstr
  | IConcat_string_pair :
      (string, string * 's) kinfo * (string, 's, 'r, 'f) kinstr
      -> (string, string * 's, 'r, 'f) kinstr
  | ISlice_string :
      (n num, n num * (string * 's)) kinfo * (string option, 's, 'r, 'f) kinstr
      -> (n num, n num * (string * 's), 'r, 'f) kinstr
  | IString_size :
      (string, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (string, 's, 'r, 'f) kinstr
  (*
     Bytes
     -----
  *)
  | IConcat_bytes :
      (bytes boxed_list, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes boxed_list, 's, 'r, 'f) kinstr
  | IConcat_bytes_pair :
      (bytes, bytes * 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, bytes * 's, 'r, 'f) kinstr
  | ISlice_bytes :
      (n num, n num * (bytes * 's)) kinfo * (bytes option, 's, 'r, 'f) kinstr
      -> (n num, n num * (bytes * 's), 'r, 'f) kinstr
  | IBytes_size :
      (bytes, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  (*
     Timestamps
     ----------
   *)
  | IAdd_seconds_to_timestamp :
      (z num, Script_timestamp.t * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (z num, Script_timestamp.t * 's, 'r, 'f) kinstr
  | IAdd_timestamp_to_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | ISub_timestamp_seconds :
      (Script_timestamp.t, z num * 's) kinfo
      * (Script_timestamp.t, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, z num * 's, 'r, 'f) kinstr
  | IDiff_timestamps :
      (Script_timestamp.t, Script_timestamp.t * 's) kinfo
      * (z num, 's, 'r, 'f) kinstr
      -> (Script_timestamp.t, Script_timestamp.t * 's, 'r, 'f) kinstr
  (*
     Tez
     ---
    *)
  | IAdd_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | ISub_tez :
      (Tez.t, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  | IMul_teznat :
      (Tez.t, n num * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | IMul_nattez :
      (n num, Tez.t * 's) kinfo * (Tez.t, 's, 'r, 'f) kinstr
      -> (n num, Tez.t * 's, 'r, 'f) kinstr
  | IEdiv_teznat :
      (Tez.t, n num * 's) kinfo
      * ((Tez.t, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, n num * 's, 'r, 'f) kinstr
  | IEdiv_tez :
      (Tez.t, Tez.t * 's) kinfo
      * ((n num, Tez.t) pair option, 's, 'r, 'f) kinstr
      -> (Tez.t, Tez.t * 's, 'r, 'f) kinstr
  (*
     Booleans
     --------
   *)
  | IOr :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | IAnd :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | IXor :
      (bool, bool * 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, bool * 's, 'r, 'f) kinstr
  | INot :
      (bool, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (bool, 's, 'r, 'f) kinstr
  (*
     Integers
     --------
  *)
  | IIs_nat :
      (z num, 's) kinfo * (n num option, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | INeg_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | INeg_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IAbs_int :
      (z num, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IInt_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | IAdd_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | IAdd_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | IAdd_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | IAdd_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | ISub_int :
      ('a num, 'b num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> ('a num, 'b num * 's, 'r, 'f) kinstr
  | IMul_intint :
      (z num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | IMul_intnat :
      (z num, n num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | IMul_natint :
      (n num, z num * 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | IMul_natnat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IEdiv_intint :
      (z num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, z num * 's, 'r, 'f) kinstr
  | IEdiv_intnat :
      (z num, n num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | IEdiv_natint :
      (n num, z num * 's) kinfo
      * ((z num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, z num * 's, 'r, 'f) kinstr
  | IEdiv_natnat :
      (n num, n num * 's) kinfo
      * ((n num, n num) pair option, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | ILsl_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | ILsr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IOr_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IAnd_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | IAnd_int_nat :
      (z num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (z num, n num * 's, 'r, 'f) kinstr
  | IXor_nat :
      (n num, n num * 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (n num, n num * 's, 'r, 'f) kinstr
  | INot_nat :
      (n num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (n num, 's, 'r, 'f) kinstr
  | INot_int :
      (z num, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  (*
     Control
     -------
  *)
  | IIf :
      (bool, 'a * 's) kinfo
      (* See remark in IIf_none. *)
      * ('a, 's, 'r, 'f) kinstr
      * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | ILoop :
      (bool, 'a * 's) kinfo
      * ('a, 's, bool, 'a * 's) kinstr
      * ('a, 's, 'r, 'f) kinstr
      -> (bool, 'a * 's, 'r, 'f) kinstr
  | ILoop_left :
      (('a, 'b) union, 's) kinfo
      * ('a, 's, ('a, 'b) union, 's) kinstr
      * ('b, 's, 'r, 'f) kinstr
      -> (('a, 'b) union, 's, 'r, 'f) kinstr
  | IDip :
      ('a, 'b * 's) kinfo
      * ('c, 't) kinfo
      * ('b, 's, 'c, 't) kinstr
      * ('a, 'c * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IExec :
      ('a, ('a, 'b) lambda * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f) kinstr
  | IApply :
      ('a, ('a * 't, 'b) lambda * 's) kinfo
      * 'a ty
      * (('t, 'b) lambda, 's, 'r, 'f) kinstr
      -> ('a, ('a * 't, 'b) lambda * 's, 'r, 'f) kinstr
  | ILambda :
      ('a, 's) kinfo
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IFailwith :
      ('a, 's) kinfo * Script.location * 'a ty * ('b, 't, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | INop : ('a, 's) kinfo * ('a, 's, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr
  (*
     Comparison
     ----------
  *)
  | ICompare :
      ('a, 'a * 's) kinfo * 'a comparable_ty * (z num, 's, 'r, 'f) kinstr
      -> ('a, 'a * 's, 'r, 'f) kinstr
  (*
     Comparators
     -----------
  *)
  | IEq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | INeq :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | ILt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IGt :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | ILe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  | IGe :
      (z num, 's) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (z num, 's, 'r, 'f) kinstr
  (*
     Protocol
     --------
  *)
  | IAddress :
      ('a typed_contract, 's) kinfo * (address, 's, 'r, 'f) kinstr
      -> ('a typed_contract, 's, 'r, 'f) kinstr
  | IContract :
      (address, 's) kinfo
      * 'a ty
      * string
      * ('a typed_contract option, 's, 'r, 'f) kinstr
      -> (address, 's, 'r, 'f) kinstr
  | ITransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | IImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ICreate_contract :
      (public_key_hash option, Tez.t * ('a * 's)) kinfo
      * 'a ty
      * 'b ty
      * ('b * 'a, operation boxed_list * 'a) lambda
      * field_annot option
      * (operation, address * 's, 'r, 'f) kinstr
      -> (public_key_hash option, Tez.t * ('a * 's), 'r, 'f) kinstr
  | ISet_delegate :
      (public_key_hash option, 's) kinfo * (operation, 's, 'r, 'f) kinstr
      -> (public_key_hash option, 's, 'r, 'f) kinstr
  | INow :
      ('a, 's) kinfo * (Script_timestamp.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IBalance :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ILevel :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ICheck_signature :
      (public_key, signature * (bytes * 's)) kinfo * (bool, 's, 'r, 'f) kinstr
      -> (public_key, signature * (bytes * 's), 'r, 'f) kinstr
  | IHash_key :
      (public_key, 's) kinfo * (public_key_hash, 's, 'r, 'f) kinstr
      -> (public_key, 's, 'r, 'f) kinstr
  | IPack :
      ('a, 's) kinfo * 'a ty * (bytes, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IUnpack :
      (bytes, 's) kinfo * 'a ty * ('a option, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | IBlake2b :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha256 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha512 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISource :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISender :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISelf :
      ('a, 's) kinfo
      * 'b ty
      * string
      * ('b typed_contract, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISelf_address :
      ('a, 's) kinfo * (address, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IAmount :
      ('a, 's) kinfo * (Tez.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ISapling_empty_state :
      ('a, 's) kinfo
      * Sapling.Memo_size.t
      * (Sapling.state, 'a * 's, 'b, 'f) kinstr
      -> ('a, 's, 'b, 'f) kinstr
  | ISapling_verify_update :
      (Sapling.transaction, Sapling.state * 's) kinfo
      * ((z num, Sapling.state) pair option, 's, 'r, 'f) kinstr
      -> (Sapling.transaction, Sapling.state * 's, 'r, 'f) kinstr
  | IDig :
      ('a, 's) kinfo
      * int
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('b, 'd * 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDug :
      ('a, 'b * 's) kinfo
      * int
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('d, 'u, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDipn :
      ('a, 's) kinfo
      * int
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDropn :
      ('a, 's) kinfo
      * int
      * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IChainId :
      ('a, 's) kinfo * (Chain_id.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | INever :
      (never, 's) kinfo * ('b, 'u, 'r, 'f) kinstr
      -> (never, 's, 'r, 'f) kinstr
  | IVoting_power :
      (public_key_hash, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ITotal_voting_power :
      ('a, 's) kinfo * (n num, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IKeccak :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | ISha3 :
      (bytes, 's) kinfo * (bytes, 's, 'r, 'f) kinstr
      -> (bytes, 's, 'r, 'f) kinstr
  | IAdd_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.G1.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.G1.t * 's, 'r, 'f) kinstr
  | IAdd_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.G2.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.G2.t * 's, 'r, 'f) kinstr
  | IAdd_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_g1 :
      (Bls12_381.G1.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_g2 :
      (Bls12_381.G2.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_fr :
      (Bls12_381.Fr.t, Bls12_381.Fr.t * 's) kinfo
      * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | IMul_bls12_381_z_fr :
      (Bls12_381.Fr.t, 'a num * 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 'a num * 's, 'r, 'f) kinstr
  | IMul_bls12_381_fr_z :
      ('a num, Bls12_381.Fr.t * 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> ('a num, Bls12_381.Fr.t * 's, 'r, 'f) kinstr
  | IInt_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (z num, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_g1 :
      (Bls12_381.G1.t, 's) kinfo * (Bls12_381.G1.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G1.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_g2 :
      (Bls12_381.G2.t, 's) kinfo * (Bls12_381.G2.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.G2.t, 's, 'r, 'f) kinstr
  | INeg_bls12_381_fr :
      (Bls12_381.Fr.t, 's) kinfo * (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
      -> (Bls12_381.Fr.t, 's, 'r, 'f) kinstr
  | IPairing_check_bls12_381 :
      ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's) kinfo
      * (bool, 's, 'r, 'f) kinstr
      -> ((Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list, 's, 'r, 'f) kinstr
  | IComb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) comb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IUncomb :
      ('a, 's) kinfo
      * int
      * ('a * 's, 'b * 'u) uncomb_gadt_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IComb_get :
      ('t, 's) kinfo
      * int
      * ('t, 'v) comb_get_gadt_witness
      * ('v, 's, 'r, 'f) kinstr
      -> ('t, 's, 'r, 'f) kinstr
  | IComb_set :
      ('a, 'b * 's) kinfo
      * int
      * ('a, 'b, 'c) comb_set_gadt_witness
      * ('c, 's, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDup_n :
      ('a, 's) kinfo
      * int
      * ('a * 's, 't) dup_n_gadt_witness
      * ('t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | ITicket :
      ('a, n num * 's) kinfo * ('a ticket, 's, 'r, 'f) kinstr
      -> ('a, n num * 's, 'r, 'f) kinstr
  | IRead_ticket :
      ('a ticket, 's) kinfo
      * (address * ('a * n num), 'a ticket * 's, 'r, 'f) kinstr
      -> ('a ticket, 's, 'r, 'f) kinstr
  | ISplit_ticket :
      ('a ticket, (n num * n num) * 's) kinfo
      * (('a ticket * 'a ticket) option, 's, 'r, 'f) kinstr
      -> ('a ticket, (n num * n num) * 's, 'r, 'f) kinstr
  | IJoin_tickets :
      ('a ticket * 'a ticket, 's) kinfo
      * 'a comparable_ty
      * ('a ticket option, 's, 'r, 'f) kinstr
      -> ('a ticket * 'a ticket, 's, 'r, 'f) kinstr
  | IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr

and ('arg, 'ret) lambda =
  | Lam :
      ('arg, end_of_stack, 'ret, end_of_stack) kdescr * Script.node
      -> ('arg, 'ret) lambda

and 'arg typed_contract = 'arg ty * address

(* ---- Auxiliary types -----------------------------------------------------*)
and 'ty ty =
  | Unit_t : type_annot option -> unit ty
  | Int_t : type_annot option -> z num ty
  | Nat_t : type_annot option -> n num ty
  | Signature_t : type_annot option -> signature ty
  | String_t : type_annot option -> string ty
  | Bytes_t : type_annot option -> bytes ty
  | Mutez_t : type_annot option -> Tez.t ty
  | Key_hash_t : type_annot option -> public_key_hash ty
  | Key_t : type_annot option -> public_key ty
  | Timestamp_t : type_annot option -> Script_timestamp.t ty
  | Address_t : type_annot option -> address ty
  | Bool_t : type_annot option -> bool ty
  | Pair_t :
      ('a ty * field_annot option * var_annot option)
      * ('b ty * field_annot option * var_annot option)
      * type_annot option
      -> ('a, 'b) pair ty
  | Union_t :
      ('a ty * field_annot option)
      * ('b ty * field_annot option)
      * type_annot option
      -> ('a, 'b) union ty
  | Lambda_t : 'arg ty * 'ret ty * type_annot option -> ('arg, 'ret) lambda ty
  | Option_t : 'v ty * type_annot option -> 'v option ty
  | List_t : 'v ty * type_annot option -> 'v boxed_list ty
  | Set_t : 'v comparable_ty * type_annot option -> 'v set ty
  | Map_t : 'k comparable_ty * 'v ty * type_annot option -> ('k, 'v) map ty
  | Big_map_t :
      'k comparable_ty * 'v ty * type_annot option
      -> ('k, 'v) big_map ty
  | Contract_t : 'arg ty * type_annot option -> 'arg typed_contract ty
  | Sapling_transaction_t :
      Sapling.Memo_size.t * type_annot option
      -> Sapling.transaction ty
  | Sapling_state_t :
      Sapling.Memo_size.t * type_annot option
      -> Sapling.state ty
  | Operation_t : type_annot option -> operation ty
  | Chain_id_t : type_annot option -> Chain_id.t ty
  | Never_t : type_annot option -> never ty
  | Bls12_381_g1_t : type_annot option -> Bls12_381.G1.t ty
  | Bls12_381_g2_t : type_annot option -> Bls12_381.G2.t ty
  | Bls12_381_fr_t : type_annot option -> Bls12_381.Fr.t ty
  | Ticket_t : 'a comparable_ty * type_annot option -> 'a ticket ty

and ('top_ty, 'resty) stack_ty =
  | Item_t :
      'ty ty * ('ty2, 'rest) stack_ty * var_annot option
      -> ('ty, 'ty2 * 'rest) stack_ty
  | Bot_t : (unit, unit) stack_ty

and ('key, 'value) big_map = {
  id : Big_map.Id.t option;
  diff : ('key, 'value) big_map_overlay;
  key_type : 'key comparable_ty;
  value_type : 'value ty;
}

and ('a, 's, 'r, 'f) kdescr = {
  kloc : Script.location;
  kbef : ('a, 's) stack_ty;
  kaft : ('r, 'f) stack_ty;
  kinstr : ('a, 's, 'r, 'f) kinstr;
}

and ('a, 's) kinfo = {iloc : Script.location; kstack_ty : ('a, 's) stack_ty}

and (_, _, _, _, _, _, _, _) stack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'u) kinfo
      * ('c, 'v, 'd, 'w, 'x, 's, 'y, 'u) stack_prefix_preservation_witness
      -> ( 'c,
           'v,
           'd,
           'w,
           'a,
           'x * 's,
           'a,
           'y * 'u )
         stack_prefix_preservation_witness
  | KRest : ('a, 's, 'b, 'u, 'a, 's, 'b, 'u) stack_prefix_preservation_witness

and ('before, 'after) comb_gadt_witness =
  | Comb_one : ('a * ('x * 'before), 'a * ('x * 'before)) comb_gadt_witness
  | Comb_succ :
      ('before, 'b * 'after) comb_gadt_witness
      -> ('a * 'before, ('a * 'b) * 'after) comb_gadt_witness

and ('before, 'after) uncomb_gadt_witness =
  | Uncomb_one : ('rest, 'rest) uncomb_gadt_witness
  | Uncomb_succ :
      ('b * 'before, 'after) uncomb_gadt_witness
      -> (('a * 'b) * 'before, 'a * 'after) uncomb_gadt_witness

and ('before, 'after) comb_get_gadt_witness =
  | Comb_get_zero : ('b, 'b) comb_get_gadt_witness
  | Comb_get_one : ('a * 'b, 'a) comb_get_gadt_witness
  | Comb_get_plus_two :
      ('before, 'after) comb_get_gadt_witness
      -> ('a * 'before, 'after) comb_get_gadt_witness

and ('value, 'before, 'after) comb_set_gadt_witness =
  | Comb_set_zero : ('value, _, 'value) comb_set_gadt_witness
  | Comb_set_one : ('value, 'hd * 'tl, 'value * 'tl) comb_set_gadt_witness
  | Comb_set_plus_two :
      ('value, 'before, 'after) comb_set_gadt_witness
      -> ('value, 'a * 'before, 'a * 'after) comb_set_gadt_witness

(*

   [dup_n_gadt_witness ('s, 't)] ensures that there exists at least
   [n] elements in ['s] and that the [n]-th element of ['s] is of type
   ['t]. Here [n] follows Peano's encoding (0 and successor).
   Besides, [0] corresponds to the topmost element of ['s].

   This relational predicate is defined by induction on [n].

*)
and (_, _) dup_n_gadt_witness =
  | Dup_n_zero : ('a * 'rest, 'a) dup_n_gadt_witness
  | Dup_n_succ :
      ('stack, 'b) dup_n_gadt_witness
      -> ('a * 'stack, 'b) dup_n_gadt_witness

let kinfo_of_kinstr : type a s b f. (a, s, b, f) kinstr -> (a, s) kinfo =
 fun i ->
  match i with
  | IDrop (kinfo, _) ->
      kinfo
  | IDup (kinfo, _) ->
      kinfo
  | ISwap (kinfo, _) ->
      kinfo
  | IConst (kinfo, _, _) ->
      kinfo
  | ICons_pair (kinfo, _) ->
      kinfo
  | ICar (kinfo, _) ->
      kinfo
  | ICdr (kinfo, _) ->
      kinfo
  | IUnpair (kinfo, _) ->
      kinfo
  | ICons_some (kinfo, _) ->
      kinfo
  | ICons_none (kinfo, _, _) ->
      kinfo
  | IIf_none (kinfo, _, _) ->
      kinfo
  | ICons_left (kinfo, _) ->
      kinfo
  | ICons_right (kinfo, _) ->
      kinfo
  | IIf_left (kinfo, _, _) ->
      kinfo
  | ICons_list (kinfo, _) ->
      kinfo
  | INil (kinfo, _) ->
      kinfo
  | IIf_cons (kinfo, _, _) ->
      kinfo
  | IList_map (kinfo, _, _) ->
      kinfo
  | IList_iter (kinfo, _, _) ->
      kinfo
  | IList_size (kinfo, _) ->
      kinfo
  | IEmpty_set (kinfo, _, _) ->
      kinfo
  | ISet_iter (kinfo, _, _) ->
      kinfo
  | ISet_mem (kinfo, _) ->
      kinfo
  | ISet_update (kinfo, _) ->
      kinfo
  | ISet_size (kinfo, _) ->
      kinfo
  | IEmpty_map (kinfo, _, _, _) ->
      kinfo
  | IMap_map (kinfo, _, _) ->
      kinfo
  | IMap_iter (kinfo, _, _) ->
      kinfo
  | IMap_mem (kinfo, _) ->
      kinfo
  | IMap_get (kinfo, _) ->
      kinfo
  | IMap_update (kinfo, _) ->
      kinfo
  | IMap_get_and_update (kinfo, _) ->
      kinfo
  | IMap_size (kinfo, _) ->
      kinfo
  | IEmpty_big_map (kinfo, _, _, _) ->
      kinfo
  | IBig_map_mem (kinfo, _) ->
      kinfo
  | IBig_map_get (kinfo, _) ->
      kinfo
  | IBig_map_update (kinfo, _) ->
      kinfo
  | IBig_map_get_and_update (kinfo, _) ->
      kinfo
  | IConcat_string (kinfo, _) ->
      kinfo
  | IConcat_string_pair (kinfo, _) ->
      kinfo
  | ISlice_string (kinfo, _) ->
      kinfo
  | IString_size (kinfo, _) ->
      kinfo
  | IConcat_bytes (kinfo, _) ->
      kinfo
  | IConcat_bytes_pair (kinfo, _) ->
      kinfo
  | ISlice_bytes (kinfo, _) ->
      kinfo
  | IBytes_size (kinfo, _) ->
      kinfo
  | IAdd_seconds_to_timestamp (kinfo, _) ->
      kinfo
  | IAdd_timestamp_to_seconds (kinfo, _) ->
      kinfo
  | ISub_timestamp_seconds (kinfo, _) ->
      kinfo
  | IDiff_timestamps (kinfo, _) ->
      kinfo
  | IAdd_tez (kinfo, _) ->
      kinfo
  | ISub_tez (kinfo, _) ->
      kinfo
  | IMul_teznat (kinfo, _) ->
      kinfo
  | IMul_nattez (kinfo, _) ->
      kinfo
  | IEdiv_teznat (kinfo, _) ->
      kinfo
  | IEdiv_tez (kinfo, _) ->
      kinfo
  | IOr (kinfo, _) ->
      kinfo
  | IAnd (kinfo, _) ->
      kinfo
  | IXor (kinfo, _) ->
      kinfo
  | INot (kinfo, _) ->
      kinfo
  | IIs_nat (kinfo, _) ->
      kinfo
  | INeg_nat (kinfo, _) ->
      kinfo
  | INeg_int (kinfo, _) ->
      kinfo
  | IAbs_int (kinfo, _) ->
      kinfo
  | IInt_nat (kinfo, _) ->
      kinfo
  | IAdd_intint (kinfo, _) ->
      kinfo
  | IAdd_intnat (kinfo, _) ->
      kinfo
  | IAdd_natint (kinfo, _) ->
      kinfo
  | IAdd_natnat (kinfo, _) ->
      kinfo
  | ISub_int (kinfo, _) ->
      kinfo
  | IMul_intint (kinfo, _) ->
      kinfo
  | IMul_intnat (kinfo, _) ->
      kinfo
  | IMul_natint (kinfo, _) ->
      kinfo
  | IMul_natnat (kinfo, _) ->
      kinfo
  | IEdiv_intint (kinfo, _) ->
      kinfo
  | IEdiv_intnat (kinfo, _) ->
      kinfo
  | IEdiv_natint (kinfo, _) ->
      kinfo
  | IEdiv_natnat (kinfo, _) ->
      kinfo
  | ILsl_nat (kinfo, _) ->
      kinfo
  | ILsr_nat (kinfo, _) ->
      kinfo
  | IOr_nat (kinfo, _) ->
      kinfo
  | IAnd_nat (kinfo, _) ->
      kinfo
  | IAnd_int_nat (kinfo, _) ->
      kinfo
  | IXor_nat (kinfo, _) ->
      kinfo
  | INot_nat (kinfo, _) ->
      kinfo
  | INot_int (kinfo, _) ->
      kinfo
  | IIf (kinfo, _, _) ->
      kinfo
  | ILoop (kinfo, _, _) ->
      kinfo
  | ILoop_left (kinfo, _, _) ->
      kinfo
  | IDip (kinfo, _, _, _) ->
      kinfo
  | IExec (kinfo, _) ->
      kinfo
  | IApply (kinfo, _, _) ->
      kinfo
  | ILambda (kinfo, _, _) ->
      kinfo
  | IFailwith (kinfo, _, _, _) ->
      kinfo
  | INop (kinfo, _) ->
      kinfo
  | ICompare (kinfo, _, _) ->
      kinfo
  | IEq (kinfo, _) ->
      kinfo
  | INeq (kinfo, _) ->
      kinfo
  | ILt (kinfo, _) ->
      kinfo
  | IGt (kinfo, _) ->
      kinfo
  | ILe (kinfo, _) ->
      kinfo
  | IGe (kinfo, _) ->
      kinfo
  | IAddress (kinfo, _) ->
      kinfo
  | IContract (kinfo, _, _, _) ->
      kinfo
  | ITransfer_tokens (kinfo, _) ->
      kinfo
  | IImplicit_account (kinfo, _) ->
      kinfo
  | ICreate_contract (kinfo, _, _, _, _, _) ->
      kinfo
  | ISet_delegate (kinfo, _) ->
      kinfo
  | INow (kinfo, _) ->
      kinfo
  | IBalance (kinfo, _) ->
      kinfo
  | ILevel (kinfo, _) ->
      kinfo
  | ICheck_signature (kinfo, _) ->
      kinfo
  | IHash_key (kinfo, _) ->
      kinfo
  | IPack (kinfo, _, _) ->
      kinfo
  | IUnpack (kinfo, _, _) ->
      kinfo
  | IBlake2b (kinfo, _) ->
      kinfo
  | ISha256 (kinfo, _) ->
      kinfo
  | ISha512 (kinfo, _) ->
      kinfo
  | ISource (kinfo, _) ->
      kinfo
  | ISender (kinfo, _) ->
      kinfo
  | ISelf (kinfo, _, _, _) ->
      kinfo
  | ISelf_address (kinfo, _) ->
      kinfo
  | IAmount (kinfo, _) ->
      kinfo
  | ISapling_empty_state (kinfo, _, _) ->
      kinfo
  | ISapling_verify_update (kinfo, _) ->
      kinfo
  | IDig (kinfo, _, _, _) ->
      kinfo
  | IDug (kinfo, _, _, _) ->
      kinfo
  | IDipn (kinfo, _, _, _, _) ->
      kinfo
  | IDropn (kinfo, _, _, _) ->
      kinfo
  | IChainId (kinfo, _) ->
      kinfo
  | INever (kinfo, _) ->
      kinfo
  | IVoting_power (kinfo, _) ->
      kinfo
  | ITotal_voting_power (kinfo, _) ->
      kinfo
  | IKeccak (kinfo, _) ->
      kinfo
  | ISha3 (kinfo, _) ->
      kinfo
  | IAdd_bls12_381_g1 (kinfo, _) ->
      kinfo
  | IAdd_bls12_381_g2 (kinfo, _) ->
      kinfo
  | IAdd_bls12_381_fr (kinfo, _) ->
      kinfo
  | IMul_bls12_381_g1 (kinfo, _) ->
      kinfo
  | IMul_bls12_381_g2 (kinfo, _) ->
      kinfo
  | IMul_bls12_381_fr (kinfo, _) ->
      kinfo
  | IMul_bls12_381_z_fr (kinfo, _) ->
      kinfo
  | IMul_bls12_381_fr_z (kinfo, _) ->
      kinfo
  | IInt_bls12_381_fr (kinfo, _) ->
      kinfo
  | INeg_bls12_381_g1 (kinfo, _) ->
      kinfo
  | INeg_bls12_381_g2 (kinfo, _) ->
      kinfo
  | INeg_bls12_381_fr (kinfo, _) ->
      kinfo
  | IPairing_check_bls12_381 (kinfo, _) ->
      kinfo
  | IComb (kinfo, _, _, _) ->
      kinfo
  | IUncomb (kinfo, _, _, _) ->
      kinfo
  | IComb_get (kinfo, _, _, _) ->
      kinfo
  | IComb_set (kinfo, _, _, _) ->
      kinfo
  | IDup_n (kinfo, _, _, _) ->
      kinfo
  | ITicket (kinfo, _) ->
      kinfo
  | IRead_ticket (kinfo, _) ->
      kinfo
  | ISplit_ticket (kinfo, _) ->
      kinfo
  | IJoin_tickets (kinfo, _, _) ->
      kinfo
  | IHalt kinfo ->
      kinfo

let rec ty_of_comparable_ty : type a. a comparable_ty -> a ty =
 fun s ->
  match s with
  | Unit_key _ ->
      Unit_t None
  | Never_key _ ->
      Never_t None
  | Int_key _ ->
      Int_t None
  | Nat_key _ ->
      Nat_t None
  | Signature_key _ ->
      Signature_t None
  | String_key _ ->
      String_t None
  | Bytes_key _ ->
      Bytes_t None
  | Mutez_key _ ->
      Mutez_t None
  | Bool_key _ ->
      Bool_t None
  | Key_hash_key _ ->
      Key_hash_t None
  | Key_key _ ->
      Key_t None
  | Timestamp_key _ ->
      Timestamp_t None
  | Chain_id_key _ ->
      Chain_id_t None
  | Address_key _ ->
      Address_t None
  | Pair_key ((a, _), (b, _), _) ->
      Pair_t
        ( (ty_of_comparable_ty a, None, None),
          (ty_of_comparable_ty b, None, None),
          None )
  | Union_key ((a, _), (b, _), _) ->
      Union_t
        ((ty_of_comparable_ty a, None), (ty_of_comparable_ty b, None), None)
  | Option_key (t, _) ->
      Option_t (ty_of_comparable_ty t, None)
