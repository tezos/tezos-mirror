(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Script_typed_ir

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
   cells. This representation is considered in the literature as an
   efficient representation of the stack for a stack-based abstract
   machine, mainly because this opens the opportunity for the
   accumulator to be stored in a hardware register. In the GADT, this
   invariant is encoded by representing the stack type using two
   parameters instead of one: the first one is the type of the
   accumulator while the second is the type of the rest of the stack.

   Third, in this representation, each instruction embeds its
   potential successor instructions in the control flow. This design
   choice permits an efficient implementation of the continuation
   stack in the interpreter. Assigning a precise type to this kind of
   instruction which is a cell in a linked list of instructions is
   similar to the typing of delimited continuations: we need to give a
   type [`bef] to the stack before the execution of the instruction, a
   type [`aft] to the stack after the execution of the instruction and
   before the execution of the next, and a type [`res] for the resulting
   stack type after the execution of the whole chain of instructions.

   Combining these three aspects, the type [kinstr] needs four parameters:

   ('bef_top, 'bef, 'res_top, `res) kinstr

   Notice that we could have chosen to only give two parameters to [kinstr]
   by manually enforcing each argument to be a pair but this is
   error-prone: with four parameters, this constraint is enforced by the arity of
   the type constructor itself.

   Hence, an instruction which has a successor instruction enjoys a
   type of the form:

   ('aft_top, 'aft, 'res_top, 'res) kinstr ->
   ('bef_top, 'bef, 'res_top, 'res) kinstr

   where [bef_top] and [bef] are the types of the stack top and rest
   before the instruction chain, and [res_top] and [res] are the types
   of the stack top and rest after the instruction chain

   Notations:
   ----------

   In the following declaration, we use 'a, 'b, 'c, 'd, ...
   to assign types to stack cell contents while we use 's, 't,
   'u, 'v, ... to assign types to stacks.

   The types for the final result and stack rest of a whole
   sequence of instructions are written 'r and 'f
   (standing for "result" and "final stack rest", respectively).

   Instructions for internal execution steps
   =========================================

   Some instructions of the following list are not present in the
   source language. They only appear during evaluation to account
   for intermediate execution steps. Indeed, since the interpreter
   follows a small-step style, it is sometimes necessary to decompose
   a source-level instruction (e.g. List_map) into several instructions
   with smaller steps. This technique seems required to get an efficient
   tail-recursive interpreter.

*)

type ('bef_top, 'bef, 'res_top, 'res) kinstr =
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
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
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
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
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
      (* Notice that the continuations of the following two
         instructions should have a shared suffix to avoid code
         duplication. *)
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
      * ('b * 't, 't, 'a * 's, 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDug :
      ('a, 's) kinfo
      * int
      * ('t, 'a * 't, 's, 'b * 'u) stack_prefix_preservation_witness
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDipn :
      ('a, 's) kinfo
      * int
      * ('c * 't, 'd * 'v, 'a * 's, 'b * 'u) kstack_prefix_preservation_witness
      * ('c, 't, 'd, 'v) kinstr
      * ('b, 'u, 'r, 'f) kinstr
      -> ('a, 's, 'r, 'f) kinstr
  | IDropn :
      ('a, 's) kinfo
      * int
      * ('b * 'u, 'b * 'u, 'a * 's, 'a * 's) stack_prefix_preservation_witness
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

and ('bef, 'aft) kdescr =
  | KDescr : {
      kloc : Script.location;
      kbef : 'bef stack_ty;
      kaft : 'aft stack_ty;
      kli : ('bef, 'a * 's) lift;
      klo : ('aft, 'r * 'f) lift;
      kinstr : ('a, 's, 'r, 'f) kinstr;
    }
      -> ('bef, 'aft) kdescr

and ('a, 's) kinfo = {kloc : Script.location; kstack_ty : ('a * 's) stack_ty}

and (_, _) lift =
  | BaseLift : (unit, unit * unit) lift
  | IndLift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift

and _ is_lifted = IsLifted : ('a, 'b) lift -> 'b is_lifted

and ('bef, 'aft, 'bef_suffix, 'aft_suffix) kstack_prefix_preservation_witness =
  | KPrefix :
      ('y, 'aft) kinfo
      * 'bef is_lifted
      * ('y * 'aft) is_lifted
      * ('fbef, 'faft, 'bef, 'y * 'aft) kstack_prefix_preservation_witness
      -> ( 'fbef,
           'faft,
           'x * 'bef,
           'x * ('y * 'aft) )
         kstack_prefix_preservation_witness
  | KRest :
      'bef is_lifted * 'aft is_lifted
      -> ('bef, 'aft, 'bef, 'aft) kstack_prefix_preservation_witness

(*

    We sometimes need to hide the exact shape of the input stack behind
    an existential quantification.

      [('t, 'b, 'u) exkinstr = exists 'x 'z, ('x, 'z, 'b, 'u) kinstr]

 *)

type (_, _, _) exkinstr =
  | ExKInstr : ('x, 'z, 'b, 'u) kinstr -> ('x * 'z, 'b, 'u) exkinstr
[@@unboxed]

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

(*
   FIXME: After this point, the code is TEMPORARY and will be significantly simplified
   FIXME: when the new elaboration is in place.
*)

let rec lift : type s t. (s, t) lift -> s -> t =
 fun l s ->
  match l with BaseLift -> ((), ()) | IndLift l -> (fst s, lift l (snd s))

let rec unlift : type s t. (s, t) lift -> t -> s =
 fun l t ->
  match l with BaseLift -> () | IndLift l -> (fst t, unlift l (snd t))

let succ_lift : ('a, 'y * 'b) lift -> ('x * 'a, 'x * ('y * 'b)) lift =
 fun l -> IndLift l

let coerce_lift : type x y a b. (x * a, x * b) lift -> (y * a, y * b) lift =
  function
  | IndLift l ->
      IndLift l

let succ_is_lifted : type x a. a is_lifted -> (x * a) is_lifted = function
  | IsLifted BaseLift ->
      IsLifted (IndLift BaseLift)
  | IsLifted (IndLift l) ->
      IsLifted (IndLift (IndLift l))

type (_, _) eq = Refl : ('a, 'a) eq

type (_, _) exlift_inverse =
  | ExLiftInverse : ('a * 'w, 'v) eq -> ('u, 'v) exlift_inverse

let inverse_lift : type u v. (u, v) lift -> (u, v) exlift_inverse = function
  | BaseLift ->
      ExLiftInverse Refl
  | IndLift _ ->
      ExLiftInverse Refl

type _ exlift = ExLift : ('v, 'a * 'w) lift -> 'v exlift

let rec lift_type :
    type v. v stack_ty -> (* ∃ a w. (v, a * w) lift *) v exlift = function
  | Empty_t ->
      ExLift BaseLift
  | Item_t (_, s, _) -> (
    match lift_type s with ExLift l -> ExLift (IndLift l) )

let rec fun_lift : type a b c. (a, b) lift -> (a, c) lift -> (b, c) eq =
 fun l1 l2 ->
  match (l1, l2) with
  | (BaseLift, BaseLift) ->
      Refl
  | (IndLift l1, IndLift l2) -> (
    match fun_lift l1 l2 with Refl -> Refl )

let rec kstack_prefix_preservation_witness :
    type s u s' u' ds du ds' du'.
    Script.location ->
    u' stack_ty ->
    (ds, du, s, u) stack_prefix_preservation_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (ds, ds') lift ->
    (du, du') lift ->
    (ds', du', s', u') kstack_prefix_preservation_witness =
 fun kloc kstack_ty w ls lu lds ldu ->
  match w with
  | Rest -> (
    match fun_lift ls lds with
    | Refl -> (
      match fun_lift lu ldu with
      | Refl ->
          (* ds  = s, du = u *)
          KRest (IsLifted lds, IsLifted ldu) ) )
  | Prefix w -> (
    (*
       s = x * s0
       u = x * u0
    *)
    match (ls, lu) with
    | (IndLift ls, IndLift lu) -> (
      (*
         s' = x * s'0
         u' = x * u'0
         ls : (s0, s'0)
         lu : (u0, u'0)
         *)
      match kstack_ty with
      | Item_t (_, kstack_ty, _) ->
          let kw =
            kstack_prefix_preservation_witness kloc kstack_ty w ls lu lds ldu
          in
          let kinfo = {kloc; kstack_ty} in
          KPrefix (kinfo, IsLifted ls, IsLifted lu, kw) ) )

type (_, _, _, _) exlift_stack_prefix_preservation_witness =
  | ExLiftStackPrefixPreservationWitness :
      ('ds, 'lds) lift
      * ('du, 'ldu) lift
      * ('lds, 'ldu, 's, 'u) stack_prefix_preservation_witness
      -> ('ds, 'du, 's, 'u) exlift_stack_prefix_preservation_witness

let rec lift_stack_prefix_preservation_witness :
    type s u s' u' ds du.
    (ds, du, s, u) stack_prefix_preservation_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (ds, du, s', u') exlift_stack_prefix_preservation_witness =
 fun w ls lu ->
  match w with
  | Rest ->
      (* ds = s, du = u *)
      ExLiftStackPrefixPreservationWitness (ls, lu, Rest)
  | Prefix w -> (
    (*
       s = x * s0
       u = x * u0
    *)
    match (ls, lu) with
    | (IndLift ls, IndLift lu) -> (
      (*
         s' = x * s'0
         u' = x * u'0
         ls : (s0, s'0)
         lu : (u0, u'0)
       *)
      match lift_stack_prefix_preservation_witness w ls lu with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w) ->
          (*
              lds : lift (ds, 'lds)
              ldu : lift (du, 'ldu)
              w   : ('lds, 'ldu, s'0, u'0)
          *)
          ExLiftStackPrefixPreservationWitness (lds, ldu, Prefix w) ) )

let rec lift_dup_n_gadt_witness :
    type s s' a.
    (s, a) dup_n_gadt_witness -> (s, s') lift -> (s', a) dup_n_gadt_witness =
 fun w l ->
  match w with
  | Dup_n_zero -> (
    (* s = a * s0 *)
    match l with IndLift _ -> Dup_n_zero )
  | Dup_n_succ w -> (
    (* s = a * s0 *)
    match l with
    | IndLift l ->
        let w = lift_dup_n_gadt_witness w l in
        Dup_n_succ w )

let rec lift_comb_gadt_witness :
    type s s' u u'.
    (s, u) comb_gadt_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (s', u') comb_gadt_witness =
 fun w li lo ->
  match w with
  | Comb_one -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> Comb_one ) ) )
  | Comb_succ w -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo ->
          let w = lift_comb_gadt_witness w li (IndLift lo) in
          Comb_succ w ) )

let rec lift_stack_ty :
    type t a s. (t, a * s) lift -> t stack_ty -> (a * s) stack_ty =
 fun li stack ->
  match li with
  | BaseLift ->
      Item_t (Unit_t None, stack, None)
  | IndLift li -> (
    match stack with
    | Item_t (ty, stack, a) ->
        let kstack = lift_stack_ty li stack in
        Item_t (ty, kstack, a) )

let rec lift_uncomb_gadt_witness :
    type s s' u u'.
    (s, u) uncomb_gadt_witness ->
    (s, s') lift ->
    (u, u') lift ->
    (s', u') uncomb_gadt_witness =
 fun w li lo ->
  match w with
  | Uncomb_one -> (
    match fun_lift li lo with Refl -> Uncomb_one )
  | Uncomb_succ w -> (
    match li with
    | IndLift li -> (
      match lo with
      | IndLift lo ->
          let w = lift_uncomb_gadt_witness w (IndLift li) lo in
          Uncomb_succ w ) )

let rec translate_instr :
    type a b s t v u r f.
    (t, v) descr ->
    (t, a * s) lift ->
    (v, b * u) lift ->
    (b, u, r, f) kinstr ->
    (a, s, r, f) kinstr =
  let return k = k in
  fun i li lo k ->
    let kstack_ty = lift_stack_ty li i.bef in
    let kinfo = {kloc = i.loc; kstack_ty} in
    match i.instr with
    | Seq (i1, i2) -> (
      match lift_type i1.aft with
      | ExLift lii ->
          let ki2 = translate_instr i2 lii lo k in
          translate_instr i1 li lii ki2 )
    | Drop -> (
      match li with
      | IndLift l -> (
        match fun_lift l lo with Refl -> return (IDrop (kinfo, k)) ) )
    | Dup -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ IDup (kinfo, k) ) ) ) )
    | Swap -> (
      match lo with
      | IndLift lo -> (
        match lo with
        | IndLift lo -> (
          match li with
          | IndLift li -> (
            match li with
            | IndLift li -> (
              match fun_lift lo li with Refl -> return @@ ISwap (kinfo, k) ) )
          ) ) )
    | Const ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ IConst (kinfo, ty, k) ) )
    | Cons_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ ICons_pair (kinfo, k) )
          ) ) )
    | Car -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ ICar (kinfo, k) ) ) )
    | Cdr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ ICdr (kinfo, k) ) ) )
    | Unpair -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ IUnpair (kinfo, k) ) )
        ) )
    | Cons_some -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ ICons_some (kinfo, k) ) )
      )
    | Cons_none ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ ICons_none (kinfo, ty, k) )
      )
    | If_none (i1, i2) -> (
      match li with
      | IndLift li' ->
          let ki1 = translate_instr i1 li' lo k in
          let ki2 = translate_instr i2 (coerce_lift li) lo k in
          return @@ IIf_none (kinfo, ki1, ki2) )
    | Cons_left -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ ICons_left (kinfo, k) ) )
      )
    | Cons_right -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift lo li with Refl -> return @@ ICons_right (kinfo, k) )
        ) )
    | If_left (i1, i2) -> (
      match li with
      | IndLift _ ->
          let ki1 = translate_instr i1 (coerce_lift li) lo k in
          let ki2 = translate_instr i2 (coerce_lift li) lo k in
          return @@ IIf_left (kinfo, ki1, ki2) )
    | Cons_list -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift lo li with Refl -> return @@ ICons_list (kinfo, k) )
          ) ) )
    | Nil -> (
      match lo with
      | IndLift lo -> (
        match fun_lift lo li with Refl -> return @@ INil (kinfo, k) ) )
    | If_cons (i1, i2) -> (
      match li with
      | IndLift li' ->
          let ki1 = translate_instr i1 (succ_lift li) lo k in
          let ki2 = translate_instr i2 li' lo k in
          return @@ IIf_cons (kinfo, ki1, ki2) )
    | List_map i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                IHalt
                  {
                    kloc = i.loc;
                    kstack_ty = lift_stack_ty (coerce_lift lo) i.aft;
                  }
              in
              let ki =
                translate_instr i (coerce_lift li) (coerce_lift lo) khalt
              in
              return @@ IList_map (kinfo, ki, k) ) ) )
    | List_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let kinfo' = {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft} in
            let ki = translate_instr i (coerce_lift li) lo (IHalt kinfo') in
            return @@ IList_iter (kinfo, ki, k) ) )
    | List_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IList_size (kinfo, k) ) )
      )
    | Empty_set ty -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ IEmpty_set (kinfo, ty, k) )
      )
    | Set_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let kinfo' = {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft} in
            let ki = translate_instr i (coerce_lift li) lo (IHalt kinfo') in
            return @@ ISet_iter (kinfo, ki, k) ) )
    | Set_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ISet_mem (kinfo, k) ) )
        ) )
    | Set_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ ISet_update (kinfo, k) ) ) ) ) )
    | Set_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ISet_size (kinfo, k) ) )
      )
    | Empty_map (cty, ty) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ IEmpty_map (kinfo, cty, ty, k) ) )
    | Map_map i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                IHalt
                  {
                    kloc = i.loc;
                    kstack_ty = lift_stack_ty (coerce_lift lo) i.aft;
                  }
              in
              let ki =
                translate_instr i (coerce_lift li) (coerce_lift lo) khalt
              in
              return @@ IMap_map (kinfo, ki, k) ) ) )
    | Map_iter i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let khalt =
              IHalt {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft}
            in
            let ki = translate_instr i (coerce_lift li) lo khalt in
            return @@ IMap_iter (kinfo, ki, k) ) )
    | Map_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMap_mem (kinfo, k) ) )
        ) )
    | Map_get -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMap_get (kinfo, k) ) )
        ) )
    | Map_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ IMap_update (kinfo, k) ) ) ) ) )
    | Map_get_and_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match lo with
              | IndLift lo -> (
                match fun_lift li lo with
                | Refl ->
                    return @@ IMap_get_and_update (kinfo, k) ) ) ) ) ) )
    | Map_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IMap_size (kinfo, k) ) )
      )
    | Empty_big_map (cty, ty) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ IEmpty_big_map (kinfo, cty, ty, k) ) )
    | Big_map_mem -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IBig_map_mem (kinfo, k) ) ) ) )
    | Big_map_get -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IBig_map_get (kinfo, k) ) ) ) )
    | Big_map_get_and_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match lo with
              | IndLift lo -> (
                match fun_lift li lo with
                | Refl ->
                    return @@ IBig_map_get_and_update (kinfo, k) ) ) ) ) ) )
    | Big_map_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ IBig_map_update (kinfo, k) ) ) ) ) )
    | Concat_string -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IConcat_string (kinfo, k) ) ) )
    | Concat_string_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IConcat_string_pair (kinfo, k) ) ) ) )
    | Slice_string -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ ISlice_string (kinfo, k) ) ) ) ) )
    | String_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IString_size (kinfo, k) )
        ) )
    | Concat_bytes -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IConcat_bytes (kinfo, k)
          ) ) )
    | Concat_bytes_pair -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IConcat_bytes_pair (kinfo, k) ) ) ) )
    | Slice_bytes -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ ISlice_bytes (kinfo, k) ) ) ) ) )
    | Bytes_size -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IBytes_size (kinfo, k) )
        ) )
    | Add_seconds_to_timestamp -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAdd_seconds_to_timestamp (kinfo, k) ) ) ) )
    | Add_timestamp_to_seconds -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAdd_timestamp_to_seconds (kinfo, k) ) ) ) )
    | Sub_timestamp_seconds -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ ISub_timestamp_seconds (kinfo, k) ) ) ) )
    | Diff_timestamps -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IDiff_timestamps (kinfo, k) ) ) ) )
    | Add_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAdd_tez (kinfo, k) ) )
        ) )
    | Sub_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ISub_tez (kinfo, k) ) )
        ) )
    | Mul_teznat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_teznat (kinfo, k)
            ) ) ) )
    | Mul_nattez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_nattez (kinfo, k)
            ) ) ) )
    | Ediv_teznat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IEdiv_teznat (kinfo, k) ) ) ) )
    | Ediv_tez -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IEdiv_tez (kinfo, k) )
          ) ) )
    | Or -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IOr (kinfo, k) ) ) ) )
    | And -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAnd (kinfo, k) ) ) ) )
    | Xor -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IXor (kinfo, k) ) ) ) )
    | Not -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INot (kinfo, k) ) ) )
    | Is_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IIs_nat (kinfo, k) ) ) )
    | Neg_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INeg_nat (kinfo, k) ) ) )
    | Neg_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INeg_int (kinfo, k) ) ) )
    | Abs_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IAbs_int (kinfo, k) ) ) )
    | Int_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IInt_bls12_381_fr (kinfo, k) ) ) )
    | Int_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IInt_nat (kinfo, k) ) ) )
    | Add_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAdd_intint (kinfo, k)
            ) ) ) )
    | Add_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAdd_intnat (kinfo, k)
            ) ) ) )
    | Add_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAdd_natint (kinfo, k)
            ) ) ) )
    | Add_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAdd_natnat (kinfo, k)
            ) ) ) )
    | Sub_int -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ISub_int (kinfo, k) ) )
        ) )
    | Mul_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_intint (kinfo, k)
            ) ) ) )
    | Mul_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_intnat (kinfo, k)
            ) ) ) )
    | Mul_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_natint (kinfo, k)
            ) ) ) )
    | Mul_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IMul_natnat (kinfo, k)
            ) ) ) )
    | Ediv_intint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IEdiv_intint (kinfo, k) ) ) ) )
    | Ediv_intnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IEdiv_intnat (kinfo, k) ) ) ) )
    | Ediv_natint -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IEdiv_natint (kinfo, k) ) ) ) )
    | Ediv_natnat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IEdiv_natnat (kinfo, k) ) ) ) )
    | Lsl_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ILsl_nat (kinfo, k) ) )
        ) )
    | Lsr_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ILsr_nat (kinfo, k) ) )
        ) )
    | Or_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IOr_nat (kinfo, k) ) )
        ) )
    | And_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IAnd_nat (kinfo, k) ) )
        ) )
    | And_int_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAnd_int_nat (kinfo, k) ) ) ) )
    | Xor_nat -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IXor_nat (kinfo, k) ) )
        ) )
    | Not_nat -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INot_nat (kinfo, k) ) ) )
    | Not_int -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INot_int (kinfo, k) ) ) )
    | If (i1, i2) -> (
      match li with
      | IndLift li ->
          let ki1 = translate_instr i1 li lo k in
          let ki2 = translate_instr i2 li lo k in
          return @@ IIf (kinfo, ki1, ki2) )
    | Loop i -> (
      match li with
      | IndLift li' -> (
        match fun_lift li' lo with
        | Refl ->
            let khalt =
              IHalt {kloc = i.loc; kstack_ty = lift_stack_ty li i.aft}
            in
            let ki = translate_instr i li' li khalt in
            return @@ ILoop (kinfo, ki, k) ) )
    | Loop_left i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              let khalt =
                IHalt {kloc = i.loc; kstack_ty = lift_stack_ty li i.aft}
              in
              let ki = translate_instr i (coerce_lift li) li khalt in
              return @@ ILoop_left (kinfo, ki, k) ) ) )
    | Dip i -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo ->
            let kinfo_const =
              {kloc = i.loc; kstack_ty = lift_stack_ty lo i.aft}
            in
            let ki = translate_instr i li' lo (IHalt kinfo_const) in
            return @@ IDip (kinfo, kinfo_const, ki, k) ) )
    | Exec -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IExec (kinfo, k) ) ) )
      )
    | Apply f -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ IApply (kinfo, f, k) )
          ) ) )
    | Lambda b -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ILambda (kinfo, b, k) ) )
    | Failwith e -> (
      match li with IndLift _ -> return @@ IFailwith (kinfo, i.loc, e, k) )
    | Nop -> (
      match fun_lift li lo with Refl -> return @@ INop (kinfo, k) )
    | Compare c -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ICompare (kinfo, c, k)
            ) ) ) )
    | Eq -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IEq (kinfo, k) ) ) )
    | Neq -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ INeq (kinfo, k) ) ) )
    | Lt -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ILt (kinfo, k) ) ) )
    | Gt -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IGt (kinfo, k) ) ) )
    | Le -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ILe (kinfo, k) ) ) )
    | Ge -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IGe (kinfo, k) ) ) )
    | Address -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IAddress (kinfo, k) ) ) )
    | Contract (a, b) -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IContract (kinfo, a, b, k) ) ) )
    | Transfer_tokens -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ ITransfer_tokens (kinfo, k) ) ) ) ) )
    | Implicit_account -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IImplicit_account (kinfo, k) ) ) )
    | Create_contract (a, b, c, d) -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match lo with
              | IndLift lo -> (
                match fun_lift li lo with
                | Refl ->
                    return @@ ICreate_contract (kinfo, a, b, c, d, k) ) ) ) ) )
      )
    | Set_delegate -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ISet_delegate (kinfo, k)
          ) ) )
    | Now -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ INow (kinfo, k) ) )
    | Balance -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ IBalance (kinfo, k) ) )
    | Level -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ILevel (kinfo, k) ) )
    | Check_signature -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match li with
          | IndLift li -> (
            match lo with
            | IndLift lo -> (
              match fun_lift li lo with
              | Refl ->
                  return @@ ICheck_signature (kinfo, k) ) ) ) ) )
    | Hash_key -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IHash_key (kinfo, k) ) )
      )
    | Pack ty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IPack (kinfo, ty, k) ) )
      )
    | Unpack ty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IUnpack (kinfo, ty, k) )
        ) )
    | Blake2b -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IBlake2b (kinfo, k) ) ) )
    | Sha256 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ISha256 (kinfo, k) ) ) )
    | Sha512 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ISha512 (kinfo, k) ) ) )
    | Source -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ISource (kinfo, k) ) )
    | Sender -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ISender (kinfo, k) ) )
    | Self (a, b) -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ISelf (kinfo, a, b, k) ) )
    | Self_address -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ ISelf_address (kinfo, k) )
      )
    | Amount -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ IAmount (kinfo, k) ) )
    | Sapling_empty_state m -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ ISapling_empty_state (kinfo, m.memo_size, k) ) )
    | Sapling_verify_update -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ ISapling_verify_update (kinfo, k) ) ) ) )
    | Dig (n, w) -> (
      match lo with
      | IndLift lo' -> (
        match lift_stack_prefix_preservation_witness w li lo' with
        | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
          match lds with
          | IndLift lds -> (
            match fun_lift lds ldu with
            | Refl ->
                return @@ IDig (kinfo, n, w', k) ) ) ) )
    | Dug (n, w) -> (
      match li with
      | IndLift li' -> (
        match lift_stack_prefix_preservation_witness w li' lo with
        | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
          match ldu with
          | IndLift ldu -> (
            match fun_lift lds ldu with
            | Refl ->
                return @@ IDug (kinfo, n, w', k) ) ) ) )
    | Dipn (n, w, i') -> (
      match lift_stack_prefix_preservation_witness w li lo with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, _) -> (
        match inverse_lift lds with
        | ExLiftInverse Refl -> (
          match inverse_lift ldu with
          | ExLiftInverse Refl ->
              let hinfo =
                {kloc = i.loc; kstack_ty = lift_stack_ty ldu i'.aft}
              in
              let ki' = translate_instr i' lds ldu (IHalt hinfo) in
              let sty = lift_stack_ty lo i.aft in
              let l = i.loc in
              let w =
                kstack_prefix_preservation_witness l sty w li lo lds ldu
              in
              return @@ IDipn (kinfo, n, w, ki', k) ) ) )
    | Dropn (n, w) -> (
      match lift_stack_prefix_preservation_witness w li li with
      | ExLiftStackPrefixPreservationWitness (lds, ldu, w') -> (
        match inverse_lift lds with
        | ExLiftInverse Refl -> (
          match inverse_lift ldu with
          | ExLiftInverse Refl -> (
            match fun_lift lds ldu with
            | Refl -> (
              match fun_lift lo lds with
              | Refl ->
                  return @@ IDropn (kinfo, n, w', k) ) ) ) ) )
    | ChainId -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with Refl -> return @@ IChainId (kinfo, k) ) )
    | Never -> (
      match li with IndLift _ -> return @@ INever (kinfo, k) )
    | Voting_power -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IVoting_power (kinfo, k)
          ) ) )
    | Total_voting_power -> (
      match lo with
      | IndLift lo -> (
        match fun_lift li lo with
        | Refl ->
            return @@ ITotal_voting_power (kinfo, k) ) )
    | Keccak -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IKeccak (kinfo, k) ) ) )
    | Sha3 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ ISha3 (kinfo, k) ) ) )
    | Add_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAdd_bls12_381_g1 (kinfo, k) ) ) ) )
    | Add_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAdd_bls12_381_g2 (kinfo, k) ) ) ) )
    | Add_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IAdd_bls12_381_fr (kinfo, k) ) ) ) )
    | Mul_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IMul_bls12_381_g1 (kinfo, k) ) ) ) )
    | Mul_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IMul_bls12_381_g2 (kinfo, k) ) ) ) )
    | Mul_bls12_381_z_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IMul_bls12_381_z_fr (kinfo, k) ) ) ) )
    | Mul_bls12_381_fr_z -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IMul_bls12_381_fr_z (kinfo, k) ) ) ) )
    | Mul_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IMul_bls12_381_fr (kinfo, k) ) ) ) )
    | Neg_bls12_381_g1 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ INeg_bls12_381_g1 (kinfo, k) ) ) )
    | Neg_bls12_381_g2 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ INeg_bls12_381_g2 (kinfo, k) ) ) )
    | Neg_bls12_381_fr -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ INeg_bls12_381_fr (kinfo, k) ) ) )
    | Pairing_check_bls12_381 -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IPairing_check_bls12_381 (kinfo, k) ) ) )
    | Dup_n (n, i) -> (
        let i = lift_dup_n_gadt_witness i li in
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with Refl -> return @@ IDup_n (kinfo, n, i, k) )
        )
    | Comb (n, w) ->
        let w = lift_comb_gadt_witness w li lo in
        return @@ IComb (kinfo, n, w, k)
    | Uncomb (n, w) ->
        let w = lift_uncomb_gadt_witness w li lo in
        return @@ IUncomb (kinfo, n, w, k)
    | Comb_get (n, w) -> (
      match li with
      | IndLift li' -> (
        match lo with
        | IndLift lo' -> (
          match fun_lift li' lo' with
          | Refl ->
              return @@ IComb_get (kinfo, n, w, k) ) ) )
    | Comb_set (n, w) -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IComb_set (kinfo, n, w, k) ) ) ) )
    | Ticket -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with Refl -> return @@ ITicket (kinfo, k) ) )
        ) )
    | Read_ticket -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ IRead_ticket (kinfo, k) ) ) ) )
    | Split_ticket -> (
      match li with
      | IndLift li -> (
        match li with
        | IndLift li -> (
          match lo with
          | IndLift lo -> (
            match fun_lift li lo with
            | Refl ->
                return @@ ISplit_ticket (kinfo, k) ) ) ) )
    | Join_tickets cty -> (
      match li with
      | IndLift li -> (
        match lo with
        | IndLift lo -> (
          match fun_lift li lo with
          | Refl ->
              return @@ IJoin_tickets (kinfo, cty, k) ) ) )

let translate : type bef aft. (bef, aft) descr -> (bef, aft) kdescr =
 fun d ->
  match (lift_type d.bef, lift_type d.aft) with
  | (ExLift kli, ExLift klo) ->
      let khalt = IHalt {kloc = d.loc; kstack_ty = lift_stack_ty klo d.aft} in
      let kinstr = translate_instr d kli klo khalt in
      KDescr {kloc = d.loc; kbef = d.bef; kaft = d.aft; kli; klo; kinstr}
