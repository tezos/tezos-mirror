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

type empty_cell = EmptyCell

type end_of_stack = empty_cell * empty_cell

type _ comparable_ty =
  | Unit_key : type_annot option -> unit comparable_ty
  | Never_key : type_annot option -> never comparable_ty
  | Int_key : type_annot option -> z num comparable_ty
  | Nat_key : type_annot option -> n num comparable_ty
  | Signature_key : type_annot option -> signature comparable_ty
  | String_key : type_annot option -> Script_string.t comparable_ty
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
  | Option_key : 'v comparable_ty * type_annot option -> 'v option comparable_ty

module type Boxed_set = sig
  type elt

  val elt_ty : elt comparable_ty

  module OPS : Set.S with type elt = elt

  val boxed : OPS.t

  val size : int
end

type 'elt set = (module Boxed_set with type elt = 'elt)

module type Boxed_map = sig
  type key

  type value

  val key_ty : key comparable_ty

  module OPS : Map.S with type key = key

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

type 'elt boxed_list = {elements : 'elt list; length : int}

module SMap = Map.Make (Script_string)

type view = {
  input_ty : Script.node;
  output_ty : Script.node;
  view_code : Script.node;
}

type ('arg, 'storage) script = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage : 'storage;
  storage_type : 'storage ty;
  views : view SMap.t;
  root_name : field_annot option;
  base_size : int;
      (* This is an over-approximation of the value size in memory, in
         bytes, of the contract permanent part, that is its source
         code. *)
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
   locations). The type for these metadata is [kinfo]: such a value is
   only used for logging and error reporting and has no impact on the
   operational semantics.

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
  | IIf_none : {
      kinfo : ('a option, 'b * 's) kinfo;
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
      branch_if_none : ('b, 's, 'r, 'f) kinstr;
      branch_if_some : ('a, 'b * 's, 'r, 'f) kinstr;
    }
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
  | IIf_left : {
      kinfo : (('a, 'b) union, 's) kinfo;
      (* See remark in IIf_none. *)
      branch_if_left : ('a, 's, 'r, 'f) kinstr;
      branch_if_right : ('b, 's, 'r, 'f) kinstr;
    }
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
  | IIf_cons : {
      kinfo : ('a boxed_list, 'b * 's) kinfo;
      (* See remark in IIf_none. *)
      branch_if_cons : ('a, 'a boxed_list * ('b * 's), 'r, 'f) kinstr;
      branch_if_nil : ('b, 's, 'r, 'f) kinstr;
    }
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
      ('a, 'b option * (('a, 'b) map * 's)) kinfo
      * ('b option, ('a, 'b) map * 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) map * 's), 'r, 'f) kinstr
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
      ('a, 'b option * (('a, 'b) big_map * 's)) kinfo
      * ('b option, ('a, 'b) big_map * 's, 'r, 'f) kinstr
      -> ('a, 'b option * (('a, 'b) big_map * 's), 'r, 'f) kinstr
  (*
     Strings
     -------
  *)
  | IConcat_string :
      (Script_string.t boxed_list, 's) kinfo
      * (Script_string.t, 's, 'r, 'f) kinstr
      -> (Script_string.t boxed_list, 's, 'r, 'f) kinstr
  | IConcat_string_pair :
      (Script_string.t, Script_string.t * 's) kinfo
      * (Script_string.t, 's, 'r, 'f) kinstr
      -> (Script_string.t, Script_string.t * 's, 'r, 'f) kinstr
  | ISlice_string :
      (n num, n num * (Script_string.t * 's)) kinfo
      * (Script_string.t option, 's, 'r, 'f) kinstr
      -> (n num, n num * (Script_string.t * 's), 'r, 'f) kinstr
  | IString_size :
      (Script_string.t, 's) kinfo * (n num, 's, 'r, 'f) kinstr
      -> (Script_string.t, 's, 'r, 'f) kinstr
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
  | IIf : {
      kinfo : (bool, 'a * 's) kinfo;
      (* See remark in IIf_none. *)
      branch_if_true : ('a, 's, 'r, 'f) kinstr;
      branch_if_false : ('a, 's, 'r, 'f) kinstr;
    }
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
      * ('b, 's, 'c, 't) kinstr
      * ('a, 'c * 't, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IExec :
      ('a, ('a, 'b) lambda * 's) kinfo * ('b, 's, 'r, 'f) kinstr
      -> ('a, ('a, 'b) lambda * 's, 'r, 'f) kinstr
  | IApply :
      ('a, ('a * 'b, 'c) lambda * 's) kinfo
      * 'a ty
      * (('b, 'c) lambda, 's, 'r, 'f) kinstr
      -> ('a, ('a * 'b, 'c) lambda * 's, 'r, 'f) kinstr
  | ILambda :
      ('a, 's) kinfo
      * ('b, 'c) lambda
      * (('b, 'c) lambda, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IFailwith :
      ('a, 's) kinfo * Script.location * 'a ty * ('b, 't, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
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
  | IView :
      ('a, address * 's) kinfo
      * ('a, 'b) view_signature
      * ('b option, 's, 'r, 'f) kinstr
      -> ('a, address * 's, 'r, 'f) kinstr
  | ITransfer_tokens :
      ('a, Tez.t * ('a typed_contract * 's)) kinfo
      * (operation, 's, 'r, 'f) kinstr
      -> ('a, Tez.t * ('a typed_contract * 's), 'r, 'f) kinstr
  | IImplicit_account :
      (public_key_hash, 's) kinfo * (unit typed_contract, 's, 'r, 'f) kinstr
      -> (public_key_hash, 's, 'r, 'f) kinstr
  | ICreate_contract : {
      kinfo : (public_key_hash option, Tez.t * ('a * 's)) kinfo;
      storage_type : 'a ty;
      arg_type : 'b ty;
      lambda : ('b * 'a, operation boxed_list * 'a) lambda;
      root_name : field_annot option;
      k : (operation, address * 's, 'r, 'f) kinstr;
    }
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
      (*
         There is a prefix of length [n] common to the input stack
         of type ['a * 's] and an intermediary stack of type ['d * 'u].
      *)
      * int
        (*
         Under this common prefix, the input stack has type ['b * 'c * 't] and
         the intermediary stack type ['c * 't] because we removed the ['b] from
         the input stack. This value of type ['b] is pushed on top of the
         stack passed to the continuation.
      *)
      * ('b, 'c * 't, 'c, 't, 'a, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('b, 'd * 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDug :
      ('a, 'b * 's) kinfo
      (*
         The input stack has type ['a * 'b * 's].

         There is a prefix of length [n] common to its substack
         of type ['b * 's] and the output stack of type ['d * 'u].
      *)
      * int
        (*
         Under this common prefix, the first stack has type ['c * 't]
         and the second has type ['a * 'c * 't] because we have pushed
         the topmost element of this input stack under the common prefix.
      *)
      * ('c, 't, 'a, 'c * 't, 'b, 's, 'd, 'u) stack_prefix_preservation_witness
      * ('d, 'u, 'r, 'f) kinstr
      -> ('a, 'b * 's, 'r, 'f) kinstr
  | IDipn :
      ('a, 's) kinfo
      (*
         The body of Dipn is applied under a prefix of size [n]...
      *)
      * int
        (*
         ... the relation between the types of the input and output stacks
         is characterized by the following witness.
         (See forthcoming comments about [stack_prefix_preservation_witness].)
      *)
      * ('c, 't, 'd, 'v, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDropn :
      ('a, 's) kinfo
      (*
         The input stack enjoys a prefix of length [n]...
      *)
      * int
        (*
         ... and the following value witnesses that under this prefix
         the stack has type ['b * 'u].
      *)
      * ('b, 'u, 'b, 'u, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      (*
         This stack is passed to the continuation since we drop the
         entire prefix.
      *)
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IChainId :
      ('a, 's) kinfo * (Chain_id.t, 'a * 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | INever : (never, 's) kinfo -> (never, 's, 'r, 'f) kinstr
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
  | IOpen_chest :
      (Timelock.chest_key, Timelock.chest * (n num * 's)) kinfo
      * ((bytes, bool) union, 's, 'r, 'f) kinstr
      -> (Timelock.chest_key, Timelock.chest * (n num * 's), 'r, 'f) kinstr
  (*

     Internal control instructions
     =============================

     The following instructions are not available in the source language.
     They are used by the internals of the interpreter.
  *)
  | IHalt : ('a, 's) kinfo -> ('a, 's, 'a, 's) kinstr
  | ILog :
      ('a, 's) kinfo * logging_event * logger * ('a, 's, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr

and logging_event =
  | LogEntry : logging_event
  | LogExit : ('b, 'u) kinfo -> logging_event

and ('arg, 'ret) lambda =
  | Lam :
      ('arg, end_of_stack, 'ret, end_of_stack) kdescr * Script.node
      -> ('arg, 'ret) lambda
[@@coq_force_gadt]

and 'arg typed_contract = 'arg ty * address

(*

  Control stack
  =============

  The control stack is a list of [kinstr].

  Since [kinstr] denotes a list  of instructions, the control stack
  can be seen as a list of instruction sequences, each representing a
  form of delimited continuation (i.e. a control stack fragment). The
  [continuation] GADT ensures that the input and output stack types of the
  continuations are consistent.

  Loops have a special treatment because their control stack is reused
  as is for the next iteration. This avoids the reallocation of a
  control stack cell at each iteration.

  To implement [step] as a tail-recursive function, we implement
  higher-order iterators (i.e. MAPs and ITERs) using internal instructions
. Roughly speaking, these instructions help in decomposing the execution
  of [I f c] (where [I] is an higher-order iterator over a container [c])
  into three phases: to start the iteration, to execute [f] if there are
  elements to be processed in [c], and to loop.

  Dip also has a dedicated constructor in the control stack.  This
  allows the stack prefix to be restored after the execution of the
  [Dip]'s body.

  Following the same style as in [kinstr], [continuation] has four
  arguments, two for each stack types. More precisely, with

            [('bef_top, 'bef, 'aft_top, 'aft) continuation]

  we encode the fact that the stack before executing the continuation
  has type [('bef_top * 'bef)] and that the stack after this execution
  has type [('aft_top * 'aft)].

*)
and (_, _, _, _) continuation =
  (* This continuation returns immediately. *)
  | KNil : ('r, 'f, 'r, 'f) continuation
  (* This continuation starts with the next instruction to execute. *)
  | KCons :
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation represents a call frame: it stores the caller's
     stack of type ['s] and the continuation which expects the callee's
     result on top of the stack. *)
  | KReturn :
      's * ('a, 's, 'r, 'f) continuation
      -> ('a, end_of_stack, 'r, 'f) continuation
  (* This continuation comes right after a [Dip i] to restore the topmost
     element ['b] of the stack after having executed [i] in the substack
     of type ['a * 's]. *)
  | KUndip :
      'b * ('b, 'a * 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a loop with
     a Boolean condition. *)
  | KLoop_in :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) continuation
      -> (bool, 'a * 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a loop with
     a condition encoded by a sum type. *)
  | KLoop_in_left :
      ('a, 's, ('a, 'b) union, 's) kinstr * ('b, 's, 'r, 'f) continuation
      -> (('a, 'b) union, 's, 'r, 'f) continuation
  (* This continuation is executed at each iteration of a traversal.
     (Used in List, Map and Set.) *)
  | KIter :
      ('a, 'b * 's, 'b, 's) kinstr * 'a list * ('b, 's, 'r, 'f) continuation
      -> ('b, 's, 'r, 'f) continuation
  (* This continuation represents each step of a List.map. *)
  | KList_enter_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('c, 's, 'r, 'f) continuation
  (* This continuation represents what is done after each step of a List.map. *)
  | KList_exit_body :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('b, 'c * 's, 'r, 'f) continuation
  (* This continuation represents each step of a Map.map. *)
  | KMap_enter_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('d, 's, 'r, 'f) continuation
  (* This continuation represents what is done after each step of a Map.map. *)
  | KMap_exit_body :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) map
      * 'a
      * (('a, 'c) map, 'd * 's, 'r, 'f) continuation
      -> ('c, 'd * 's, 'r, 'f) continuation
  (* This continuation instruments the execution with a [logger]. *)
  | KLog :
      ('a, 's, 'r, 'f) continuation * logger
      -> ('a, 's, 'r, 'f) continuation

(*

    Execution instrumentation
    =========================

   One can observe the context and the stack at some specific points
   of an execution step. This feature is implemented by calling back
   some [logging_function]s defined in a record of type [logger]
   passed as argument to the step function.

   A [logger] is typically embedded in an [KLog] continuation by the
   client to trigger an evaluation instrumented with some logging. The
   logger is then automatically propagated to the logging instruction
   [ILog] as well as to any instructions that need to generate a
   backtrace when it fails (e.g., [IFailwith], [IMul_teznat], ...).

*)
and ('a, 's, 'b, 'f, 'c, 'u) logging_function =
  ('a, 's, 'b, 'f) kinstr ->
  context ->
  Script.location ->
  ('c, 'u) stack_ty ->
  'c * 'u ->
  unit

and execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

and logger = {
  log_interp : 'a 's 'b 'f 'c 'u. ('a, 's, 'b, 'f, 'c, 'u) logging_function;
      (** [log_interp] is called at each call of the internal function
          [interp]. [interp] is called when starting the interpretation of
          a script and subsequently at each [Exec] instruction. *)
  log_entry : 'a 's 'b 'f. ('a, 's, 'b, 'f, 'a, 's) logging_function;
      (** [log_entry] is called {i before} executing each instruction but
          {i after} gas for this instruction has been successfully
          consumed. *)
  log_control : 'a 's 'b 'f. ('a, 's, 'b, 'f) continuation -> unit;
      (** [log_control] is called {i before} the interpretation of the
          current continuation. *)
  log_exit : 'a 's 'b 'f 'c 'u. ('a, 's, 'b, 'f, 'c, 'u) logging_function;
      (** [log_exit] is called {i after} executing each instruction. *)
  get_log : unit -> execution_trace option tzresult Lwt.t;
      (** [get_log] allows to obtain an execution trace, if any was
          produced. *)
}

(* ---- Auxiliary types -----------------------------------------------------*)
and 'ty ty =
  | Unit_t : type_annot option -> unit ty
  | Int_t : type_annot option -> z num ty
  | Nat_t : type_annot option -> n num ty
  | Signature_t : type_annot option -> signature ty
  | String_t : type_annot option -> Script_string.t ty
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
  | Chest_key_t : type_annot option -> Timelock.chest_key ty
  | Chest_t : type_annot option -> Timelock.chest ty

and ('top_ty, 'resty) stack_ty =
  | Item_t :
      'ty ty * ('ty2, 'rest) stack_ty * var_annot option
      -> ('ty, 'ty2 * 'rest) stack_ty
  | Bot_t : (empty_cell, empty_cell) stack_ty

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

(*

   Several instructions work under an arbitrary deep stack prefix
   (e.g, IDipn, IDropn, etc). To convince the typechecker that
   these instructions are well-typed, we must provide a witness
   to statically characterize the relationship between the input
   and the output stacks. The inhabitants of the following GADT
   act as such witnesses.

   More precisely, a value [w] of type

   [(c, t, d, v, a, s, b, u) stack_prefix_preservation_witness]

   proves that there is a common prefix between an input stack
   of type [a * s] and an output stack of type [b * u]. This prefix
   is as deep as the number of [KPrefix] application in [w]. When
   used with an operation parameterized by a natural number [n]
   characterizing the depth at which the operation must be applied,
   [w] is the Peano encoding of [n].

   When this prefix is removed from the two stacks, the input stack
   has type [c * t] while the output stack has type [d * v].

*)
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
[@@coq_force_gadt]

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

and ('a, 'b) view_signature =
  | View_signature of {
      name : Script_string.t;
      input_ty : 'a ty;
      output_ty : 'b ty;
    }

let kinfo_of_kinstr : type a s b f. (a, s, b, f) kinstr -> (a, s) kinfo =
 fun i ->
  match i with
  | IDrop (kinfo, _) -> kinfo
  | IDup (kinfo, _) -> kinfo
  | ISwap (kinfo, _) -> kinfo
  | IConst (kinfo, _, _) -> kinfo
  | ICons_pair (kinfo, _) -> kinfo
  | ICar (kinfo, _) -> kinfo
  | ICdr (kinfo, _) -> kinfo
  | IUnpair (kinfo, _) -> kinfo
  | ICons_some (kinfo, _) -> kinfo
  | ICons_none (kinfo, _, _) -> kinfo
  | IIf_none {kinfo; _} -> kinfo
  | ICons_left (kinfo, _) -> kinfo
  | ICons_right (kinfo, _) -> kinfo
  | IIf_left {kinfo; _} -> kinfo
  | ICons_list (kinfo, _) -> kinfo
  | INil (kinfo, _) -> kinfo
  | IIf_cons {kinfo; _} -> kinfo
  | IList_map (kinfo, _, _) -> kinfo
  | IList_iter (kinfo, _, _) -> kinfo
  | IList_size (kinfo, _) -> kinfo
  | IEmpty_set (kinfo, _, _) -> kinfo
  | ISet_iter (kinfo, _, _) -> kinfo
  | ISet_mem (kinfo, _) -> kinfo
  | ISet_update (kinfo, _) -> kinfo
  | ISet_size (kinfo, _) -> kinfo
  | IEmpty_map (kinfo, _, _, _) -> kinfo
  | IMap_map (kinfo, _, _) -> kinfo
  | IMap_iter (kinfo, _, _) -> kinfo
  | IMap_mem (kinfo, _) -> kinfo
  | IMap_get (kinfo, _) -> kinfo
  | IMap_update (kinfo, _) -> kinfo
  | IMap_get_and_update (kinfo, _) -> kinfo
  | IMap_size (kinfo, _) -> kinfo
  | IEmpty_big_map (kinfo, _, _, _) -> kinfo
  | IBig_map_mem (kinfo, _) -> kinfo
  | IBig_map_get (kinfo, _) -> kinfo
  | IBig_map_update (kinfo, _) -> kinfo
  | IBig_map_get_and_update (kinfo, _) -> kinfo
  | IConcat_string (kinfo, _) -> kinfo
  | IConcat_string_pair (kinfo, _) -> kinfo
  | ISlice_string (kinfo, _) -> kinfo
  | IString_size (kinfo, _) -> kinfo
  | IConcat_bytes (kinfo, _) -> kinfo
  | IConcat_bytes_pair (kinfo, _) -> kinfo
  | ISlice_bytes (kinfo, _) -> kinfo
  | IBytes_size (kinfo, _) -> kinfo
  | IAdd_seconds_to_timestamp (kinfo, _) -> kinfo
  | IAdd_timestamp_to_seconds (kinfo, _) -> kinfo
  | ISub_timestamp_seconds (kinfo, _) -> kinfo
  | IDiff_timestamps (kinfo, _) -> kinfo
  | IAdd_tez (kinfo, _) -> kinfo
  | ISub_tez (kinfo, _) -> kinfo
  | IMul_teznat (kinfo, _) -> kinfo
  | IMul_nattez (kinfo, _) -> kinfo
  | IEdiv_teznat (kinfo, _) -> kinfo
  | IEdiv_tez (kinfo, _) -> kinfo
  | IOr (kinfo, _) -> kinfo
  | IAnd (kinfo, _) -> kinfo
  | IXor (kinfo, _) -> kinfo
  | INot (kinfo, _) -> kinfo
  | IIs_nat (kinfo, _) -> kinfo
  | INeg_nat (kinfo, _) -> kinfo
  | INeg_int (kinfo, _) -> kinfo
  | IAbs_int (kinfo, _) -> kinfo
  | IInt_nat (kinfo, _) -> kinfo
  | IAdd_intint (kinfo, _) -> kinfo
  | IAdd_intnat (kinfo, _) -> kinfo
  | IAdd_natint (kinfo, _) -> kinfo
  | IAdd_natnat (kinfo, _) -> kinfo
  | ISub_int (kinfo, _) -> kinfo
  | IMul_intint (kinfo, _) -> kinfo
  | IMul_intnat (kinfo, _) -> kinfo
  | IMul_natint (kinfo, _) -> kinfo
  | IMul_natnat (kinfo, _) -> kinfo
  | IEdiv_intint (kinfo, _) -> kinfo
  | IEdiv_intnat (kinfo, _) -> kinfo
  | IEdiv_natint (kinfo, _) -> kinfo
  | IEdiv_natnat (kinfo, _) -> kinfo
  | ILsl_nat (kinfo, _) -> kinfo
  | ILsr_nat (kinfo, _) -> kinfo
  | IOr_nat (kinfo, _) -> kinfo
  | IAnd_nat (kinfo, _) -> kinfo
  | IAnd_int_nat (kinfo, _) -> kinfo
  | IXor_nat (kinfo, _) -> kinfo
  | INot_nat (kinfo, _) -> kinfo
  | INot_int (kinfo, _) -> kinfo
  | IIf {kinfo; _} -> kinfo
  | ILoop (kinfo, _, _) -> kinfo
  | ILoop_left (kinfo, _, _) -> kinfo
  | IDip (kinfo, _, _) -> kinfo
  | IExec (kinfo, _) -> kinfo
  | IApply (kinfo, _, _) -> kinfo
  | ILambda (kinfo, _, _) -> kinfo
  | IFailwith (kinfo, _, _, _) -> kinfo
  | ICompare (kinfo, _, _) -> kinfo
  | IEq (kinfo, _) -> kinfo
  | INeq (kinfo, _) -> kinfo
  | ILt (kinfo, _) -> kinfo
  | IGt (kinfo, _) -> kinfo
  | ILe (kinfo, _) -> kinfo
  | IGe (kinfo, _) -> kinfo
  | IAddress (kinfo, _) -> kinfo
  | IContract (kinfo, _, _, _) -> kinfo
  | ITransfer_tokens (kinfo, _) -> kinfo
  | IView (kinfo, _, _) -> kinfo
  | IImplicit_account (kinfo, _) -> kinfo
  | ICreate_contract {kinfo; _} -> kinfo
  | ISet_delegate (kinfo, _) -> kinfo
  | INow (kinfo, _) -> kinfo
  | IBalance (kinfo, _) -> kinfo
  | ILevel (kinfo, _) -> kinfo
  | ICheck_signature (kinfo, _) -> kinfo
  | IHash_key (kinfo, _) -> kinfo
  | IPack (kinfo, _, _) -> kinfo
  | IUnpack (kinfo, _, _) -> kinfo
  | IBlake2b (kinfo, _) -> kinfo
  | ISha256 (kinfo, _) -> kinfo
  | ISha512 (kinfo, _) -> kinfo
  | ISource (kinfo, _) -> kinfo
  | ISender (kinfo, _) -> kinfo
  | ISelf (kinfo, _, _, _) -> kinfo
  | ISelf_address (kinfo, _) -> kinfo
  | IAmount (kinfo, _) -> kinfo
  | ISapling_empty_state (kinfo, _, _) -> kinfo
  | ISapling_verify_update (kinfo, _) -> kinfo
  | IDig (kinfo, _, _, _) -> kinfo
  | IDug (kinfo, _, _, _) -> kinfo
  | IDipn (kinfo, _, _, _, _) -> kinfo
  | IDropn (kinfo, _, _, _) -> kinfo
  | IChainId (kinfo, _) -> kinfo
  | INever kinfo -> kinfo
  | IVoting_power (kinfo, _) -> kinfo
  | ITotal_voting_power (kinfo, _) -> kinfo
  | IKeccak (kinfo, _) -> kinfo
  | ISha3 (kinfo, _) -> kinfo
  | IAdd_bls12_381_g1 (kinfo, _) -> kinfo
  | IAdd_bls12_381_g2 (kinfo, _) -> kinfo
  | IAdd_bls12_381_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_g1 (kinfo, _) -> kinfo
  | IMul_bls12_381_g2 (kinfo, _) -> kinfo
  | IMul_bls12_381_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_z_fr (kinfo, _) -> kinfo
  | IMul_bls12_381_fr_z (kinfo, _) -> kinfo
  | IInt_bls12_381_fr (kinfo, _) -> kinfo
  | INeg_bls12_381_g1 (kinfo, _) -> kinfo
  | INeg_bls12_381_g2 (kinfo, _) -> kinfo
  | INeg_bls12_381_fr (kinfo, _) -> kinfo
  | IPairing_check_bls12_381 (kinfo, _) -> kinfo
  | IComb (kinfo, _, _, _) -> kinfo
  | IUncomb (kinfo, _, _, _) -> kinfo
  | IComb_get (kinfo, _, _, _) -> kinfo
  | IComb_set (kinfo, _, _, _) -> kinfo
  | IDup_n (kinfo, _, _, _) -> kinfo
  | ITicket (kinfo, _) -> kinfo
  | IRead_ticket (kinfo, _) -> kinfo
  | ISplit_ticket (kinfo, _) -> kinfo
  | IJoin_tickets (kinfo, _, _) -> kinfo
  | IHalt kinfo -> kinfo
  | ILog (kinfo, _, _, _) -> kinfo
  | IOpen_chest (kinfo, _) -> kinfo

type kinstr_rewritek = {
  apply : 'b 'u 'r 'f. ('b, 'u, 'r, 'f) kinstr -> ('b, 'u, 'r, 'f) kinstr;
}

let kinstr_rewritek :
    type a s r f. (a, s, r, f) kinstr -> kinstr_rewritek -> (a, s, r, f) kinstr
    =
 fun i f ->
  match i with
  | IDrop (kinfo, k) -> IDrop (kinfo, f.apply k)
  | IDup (kinfo, k) -> IDup (kinfo, f.apply k)
  | ISwap (kinfo, k) -> ISwap (kinfo, f.apply k)
  | IConst (kinfo, x, k) -> IConst (kinfo, x, f.apply k)
  | ICons_pair (kinfo, k) -> ICons_pair (kinfo, f.apply k)
  | ICar (kinfo, k) -> ICar (kinfo, f.apply k)
  | ICdr (kinfo, k) -> ICdr (kinfo, f.apply k)
  | IUnpair (kinfo, k) -> IUnpair (kinfo, f.apply k)
  | ICons_some (kinfo, k) -> ICons_some (kinfo, f.apply k)
  | ICons_none (kinfo, ty, k) -> ICons_none (kinfo, ty, f.apply k)
  | IIf_none {kinfo; branch_if_none; branch_if_some} ->
      let branch_if_none = f.apply branch_if_none
      and branch_if_some = f.apply branch_if_some in
      IIf_none {kinfo; branch_if_none; branch_if_some}
  | ICons_left (kinfo, k) -> ICons_left (kinfo, f.apply k)
  | ICons_right (kinfo, k) -> ICons_right (kinfo, f.apply k)
  | IIf_left {kinfo; branch_if_left; branch_if_right} ->
      let branch_if_left = f.apply branch_if_left
      and branch_if_right = f.apply branch_if_right in
      IIf_left {kinfo; branch_if_left; branch_if_right}
  | ICons_list (kinfo, k) -> ICons_list (kinfo, f.apply k)
  | INil (kinfo, k) -> INil (kinfo, f.apply k)
  | IIf_cons {kinfo; branch_if_cons; branch_if_nil} ->
      let branch_if_nil = f.apply branch_if_nil
      and branch_if_cons = f.apply branch_if_cons in
      IIf_cons {kinfo; branch_if_cons; branch_if_nil}
  | IList_map (kinfo, body, k) -> IList_map (kinfo, f.apply body, f.apply k)
  | IList_iter (kinfo, body, k) -> IList_iter (kinfo, f.apply body, f.apply k)
  | IList_size (kinfo, k) -> IList_size (kinfo, f.apply k)
  | IEmpty_set (kinfo, ty, k) -> IEmpty_set (kinfo, ty, f.apply k)
  | ISet_iter (kinfo, body, k) -> ISet_iter (kinfo, f.apply body, f.apply k)
  | ISet_mem (kinfo, k) -> ISet_mem (kinfo, f.apply k)
  | ISet_update (kinfo, k) -> ISet_update (kinfo, f.apply k)
  | ISet_size (kinfo, k) -> ISet_size (kinfo, f.apply k)
  | IEmpty_map (kinfo, cty, ty, k) -> IEmpty_map (kinfo, cty, ty, f.apply k)
  | IMap_map (kinfo, body, k) -> IMap_map (kinfo, f.apply body, f.apply k)
  | IMap_iter (kinfo, body, k) -> IMap_iter (kinfo, f.apply body, f.apply k)
  | IMap_mem (kinfo, k) -> IMap_mem (kinfo, f.apply k)
  | IMap_get (kinfo, k) -> IMap_get (kinfo, f.apply k)
  | IMap_update (kinfo, k) -> IMap_update (kinfo, f.apply k)
  | IMap_get_and_update (kinfo, k) -> IMap_get_and_update (kinfo, f.apply k)
  | IMap_size (kinfo, k) -> IMap_size (kinfo, f.apply k)
  | IEmpty_big_map (kinfo, cty, ty, k) ->
      IEmpty_big_map (kinfo, cty, ty, f.apply k)
  | IBig_map_mem (kinfo, k) -> IBig_map_mem (kinfo, f.apply k)
  | IBig_map_get (kinfo, k) -> IBig_map_get (kinfo, f.apply k)
  | IBig_map_update (kinfo, k) -> IBig_map_update (kinfo, f.apply k)
  | IBig_map_get_and_update (kinfo, k) ->
      IBig_map_get_and_update (kinfo, f.apply k)
  | IConcat_string (kinfo, k) -> IConcat_string (kinfo, f.apply k)
  | IConcat_string_pair (kinfo, k) -> IConcat_string_pair (kinfo, f.apply k)
  | ISlice_string (kinfo, k) -> ISlice_string (kinfo, f.apply k)
  | IString_size (kinfo, k) -> IString_size (kinfo, f.apply k)
  | IConcat_bytes (kinfo, k) -> IConcat_bytes (kinfo, f.apply k)
  | IConcat_bytes_pair (kinfo, k) -> IConcat_bytes_pair (kinfo, f.apply k)
  | ISlice_bytes (kinfo, k) -> ISlice_bytes (kinfo, f.apply k)
  | IBytes_size (kinfo, k) -> IBytes_size (kinfo, f.apply k)
  | IAdd_seconds_to_timestamp (kinfo, k) ->
      IAdd_seconds_to_timestamp (kinfo, f.apply k)
  | IAdd_timestamp_to_seconds (kinfo, k) ->
      IAdd_timestamp_to_seconds (kinfo, f.apply k)
  | ISub_timestamp_seconds (kinfo, k) ->
      ISub_timestamp_seconds (kinfo, f.apply k)
  | IDiff_timestamps (kinfo, k) -> IDiff_timestamps (kinfo, f.apply k)
  | IAdd_tez (kinfo, k) -> IAdd_tez (kinfo, f.apply k)
  | ISub_tez (kinfo, k) -> ISub_tez (kinfo, f.apply k)
  | IMul_teznat (kinfo, k) -> IMul_teznat (kinfo, f.apply k)
  | IMul_nattez (kinfo, k) -> IMul_nattez (kinfo, f.apply k)
  | IEdiv_teznat (kinfo, k) -> IEdiv_teznat (kinfo, f.apply k)
  | IEdiv_tez (kinfo, k) -> IEdiv_tez (kinfo, f.apply k)
  | IOr (kinfo, k) -> IOr (kinfo, f.apply k)
  | IAnd (kinfo, k) -> IAnd (kinfo, f.apply k)
  | IXor (kinfo, k) -> IXor (kinfo, f.apply k)
  | INot (kinfo, k) -> INot (kinfo, f.apply k)
  | IIs_nat (kinfo, k) -> IIs_nat (kinfo, f.apply k)
  | INeg_nat (kinfo, k) -> INeg_nat (kinfo, f.apply k)
  | INeg_int (kinfo, k) -> INeg_int (kinfo, f.apply k)
  | IAbs_int (kinfo, k) -> IAbs_int (kinfo, f.apply k)
  | IInt_nat (kinfo, k) -> IInt_nat (kinfo, f.apply k)
  | IAdd_intint (kinfo, k) -> IAdd_intint (kinfo, f.apply k)
  | IAdd_intnat (kinfo, k) -> IAdd_intnat (kinfo, f.apply k)
  | IAdd_natint (kinfo, k) -> IAdd_natint (kinfo, f.apply k)
  | IAdd_natnat (kinfo, k) -> IAdd_natnat (kinfo, f.apply k)
  | ISub_int (kinfo, k) -> ISub_int (kinfo, f.apply k)
  | IMul_intint (kinfo, k) -> IMul_intint (kinfo, f.apply k)
  | IMul_intnat (kinfo, k) -> IMul_intnat (kinfo, f.apply k)
  | IMul_natint (kinfo, k) -> IMul_natint (kinfo, f.apply k)
  | IMul_natnat (kinfo, k) -> IMul_natnat (kinfo, f.apply k)
  | IEdiv_intint (kinfo, k) -> IEdiv_intint (kinfo, f.apply k)
  | IEdiv_intnat (kinfo, k) -> IEdiv_intnat (kinfo, f.apply k)
  | IEdiv_natint (kinfo, k) -> IEdiv_natint (kinfo, f.apply k)
  | IEdiv_natnat (kinfo, k) -> IEdiv_natnat (kinfo, f.apply k)
  | ILsl_nat (kinfo, k) -> ILsl_nat (kinfo, f.apply k)
  | ILsr_nat (kinfo, k) -> ILsr_nat (kinfo, f.apply k)
  | IOr_nat (kinfo, k) -> IOr_nat (kinfo, f.apply k)
  | IAnd_nat (kinfo, k) -> IAnd_nat (kinfo, f.apply k)
  | IAnd_int_nat (kinfo, k) -> IAnd_int_nat (kinfo, f.apply k)
  | IXor_nat (kinfo, k) -> IXor_nat (kinfo, f.apply k)
  | INot_nat (kinfo, k) -> INot_nat (kinfo, f.apply k)
  | INot_int (kinfo, k) -> INot_int (kinfo, f.apply k)
  | IIf {kinfo; branch_if_true; branch_if_false} ->
      let branch_if_true = f.apply branch_if_true
      and branch_if_false = f.apply branch_if_false in
      IIf {kinfo; branch_if_true; branch_if_false}
  | ILoop (kinfo, kbody, k) -> ILoop (kinfo, f.apply kbody, f.apply k)
  | ILoop_left (kinfo, kl, kr) -> ILoop_left (kinfo, f.apply kl, f.apply kr)
  | IDip (kinfo, body, k) -> IDip (kinfo, f.apply body, f.apply k)
  | IExec (kinfo, k) -> IExec (kinfo, f.apply k)
  | IApply (kinfo, ty, k) -> IApply (kinfo, ty, f.apply k)
  | ILambda (kinfo, l, k) -> ILambda (kinfo, l, f.apply k)
  | IFailwith (kinfo, i, ty, k) -> IFailwith (kinfo, i, ty, f.apply k)
  | ICompare (kinfo, ty, k) -> ICompare (kinfo, ty, f.apply k)
  | IEq (kinfo, k) -> IEq (kinfo, f.apply k)
  | INeq (kinfo, k) -> INeq (kinfo, f.apply k)
  | ILt (kinfo, k) -> ILt (kinfo, f.apply k)
  | IGt (kinfo, k) -> IGt (kinfo, f.apply k)
  | ILe (kinfo, k) -> ILe (kinfo, f.apply k)
  | IGe (kinfo, k) -> IGe (kinfo, f.apply k)
  | IAddress (kinfo, k) -> IAddress (kinfo, f.apply k)
  | IContract (kinfo, ty, code, k) -> IContract (kinfo, ty, code, f.apply k)
  | ITransfer_tokens (kinfo, k) -> ITransfer_tokens (kinfo, f.apply k)
  | IView (kinfo, view_signature, k) -> IView (kinfo, view_signature, f.apply k)
  | IImplicit_account (kinfo, k) -> IImplicit_account (kinfo, f.apply k)
  | ICreate_contract {kinfo; storage_type; arg_type; lambda; root_name; k} ->
      let k = f.apply k in
      ICreate_contract {kinfo; storage_type; arg_type; lambda; root_name; k}
  | ISet_delegate (kinfo, k) -> ISet_delegate (kinfo, f.apply k)
  | INow (kinfo, k) -> INow (kinfo, f.apply k)
  | IBalance (kinfo, k) -> IBalance (kinfo, f.apply k)
  | ILevel (kinfo, k) -> ILevel (kinfo, f.apply k)
  | ICheck_signature (kinfo, k) -> ICheck_signature (kinfo, f.apply k)
  | IHash_key (kinfo, k) -> IHash_key (kinfo, f.apply k)
  | IPack (kinfo, ty, k) -> IPack (kinfo, ty, f.apply k)
  | IUnpack (kinfo, ty, k) -> IUnpack (kinfo, ty, f.apply k)
  | IBlake2b (kinfo, k) -> IBlake2b (kinfo, f.apply k)
  | ISha256 (kinfo, k) -> ISha256 (kinfo, f.apply k)
  | ISha512 (kinfo, k) -> ISha512 (kinfo, f.apply k)
  | ISource (kinfo, k) -> ISource (kinfo, f.apply k)
  | ISender (kinfo, k) -> ISender (kinfo, f.apply k)
  | ISelf (kinfo, ty, s, k) -> ISelf (kinfo, ty, s, f.apply k)
  | ISelf_address (kinfo, k) -> ISelf_address (kinfo, f.apply k)
  | IAmount (kinfo, k) -> IAmount (kinfo, f.apply k)
  | ISapling_empty_state (kinfo, s, k) ->
      ISapling_empty_state (kinfo, s, f.apply k)
  | ISapling_verify_update (kinfo, k) ->
      ISapling_verify_update (kinfo, f.apply k)
  | IDig (kinfo, n, p, k) -> IDig (kinfo, n, p, f.apply k)
  | IDug (kinfo, n, p, k) -> IDug (kinfo, n, p, f.apply k)
  | IDipn (kinfo, n, p, k1, k2) -> IDipn (kinfo, n, p, f.apply k1, f.apply k2)
  | IDropn (kinfo, n, p, k) -> IDropn (kinfo, n, p, f.apply k)
  | IChainId (kinfo, k) -> IChainId (kinfo, f.apply k)
  | INever kinfo -> INever kinfo
  | IVoting_power (kinfo, k) -> IVoting_power (kinfo, f.apply k)
  | ITotal_voting_power (kinfo, k) -> ITotal_voting_power (kinfo, f.apply k)
  | IKeccak (kinfo, k) -> IKeccak (kinfo, f.apply k)
  | ISha3 (kinfo, k) -> ISha3 (kinfo, f.apply k)
  | IAdd_bls12_381_g1 (kinfo, k) -> IAdd_bls12_381_g1 (kinfo, f.apply k)
  | IAdd_bls12_381_g2 (kinfo, k) -> IAdd_bls12_381_g2 (kinfo, f.apply k)
  | IAdd_bls12_381_fr (kinfo, k) -> IAdd_bls12_381_fr (kinfo, f.apply k)
  | IMul_bls12_381_g1 (kinfo, k) -> IMul_bls12_381_g1 (kinfo, f.apply k)
  | IMul_bls12_381_g2 (kinfo, k) -> IMul_bls12_381_g2 (kinfo, f.apply k)
  | IMul_bls12_381_fr (kinfo, k) -> IMul_bls12_381_fr (kinfo, f.apply k)
  | IMul_bls12_381_z_fr (kinfo, k) -> IMul_bls12_381_z_fr (kinfo, f.apply k)
  | IMul_bls12_381_fr_z (kinfo, k) -> IMul_bls12_381_fr_z (kinfo, f.apply k)
  | IInt_bls12_381_fr (kinfo, k) -> IInt_bls12_381_fr (kinfo, f.apply k)
  | INeg_bls12_381_g1 (kinfo, k) -> INeg_bls12_381_g1 (kinfo, f.apply k)
  | INeg_bls12_381_g2 (kinfo, k) -> INeg_bls12_381_g2 (kinfo, f.apply k)
  | INeg_bls12_381_fr (kinfo, k) -> INeg_bls12_381_fr (kinfo, f.apply k)
  | IPairing_check_bls12_381 (kinfo, k) ->
      IPairing_check_bls12_381 (kinfo, f.apply k)
  | IComb (kinfo, n, p, k) -> IComb (kinfo, n, p, f.apply k)
  | IUncomb (kinfo, n, p, k) -> IUncomb (kinfo, n, p, f.apply k)
  | IComb_get (kinfo, n, p, k) -> IComb_get (kinfo, n, p, f.apply k)
  | IComb_set (kinfo, n, p, k) -> IComb_set (kinfo, n, p, f.apply k)
  | IDup_n (kinfo, n, p, k) -> IDup_n (kinfo, n, p, f.apply k)
  | ITicket (kinfo, k) -> ITicket (kinfo, f.apply k)
  | IRead_ticket (kinfo, k) -> IRead_ticket (kinfo, f.apply k)
  | ISplit_ticket (kinfo, k) -> ISplit_ticket (kinfo, f.apply k)
  | IJoin_tickets (kinfo, ty, k) -> IJoin_tickets (kinfo, ty, f.apply k)
  | IHalt kinfo -> IHalt kinfo
  | ILog (kinfo, event, logger, k) -> ILog (kinfo, event, logger, k)
  | IOpen_chest (kinfo, k) -> IOpen_chest (kinfo, f.apply k)
