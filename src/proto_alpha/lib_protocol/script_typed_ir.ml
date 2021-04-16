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

(* ---- Auxiliary types -----------------------------------------------------*)

type var_annot = Var_annot of string

type type_annot = Type_annot of string

type field_annot = Field_annot of string

type address = Contract.t * string

type ('a, 'b) pair = 'a * 'b

type ('a, 'b) union = L of 'a | R of 'b

type never = |

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

type operation = packed_internal_operation * Lazy_storage.diffs option

type 'a ticket = {ticketer : address; contents : 'a; amount : n num}

type ('arg, 'storage) script = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage : 'storage;
  storage_type : 'storage ty;
  root_name : field_annot option;
}

and end_of_stack = unit

and ('arg, 'ret) lambda =
  | Lam :
      ('arg * end_of_stack, 'ret * end_of_stack) descr * Script.node
      -> ('arg, 'ret) lambda
[@@coq_force_gadt]

and 'arg typed_contract = 'arg ty * address

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

and 'ty stack_ty =
  | Item_t :
      'ty ty * 'rest stack_ty * var_annot option
      -> ('ty * 'rest) stack_ty
  | Empty_t : end_of_stack stack_ty

and ('key, 'value) big_map = {
  id : Big_map.Id.t option;
  diff : ('key, 'value) big_map_overlay;
  key_type : 'key comparable_ty;
  value_type : 'value ty;
}

and 'elt boxed_list = {elements : 'elt list; length : int}

(* ---- Instructions --------------------------------------------------------*)

(* The low-level, typed instructions, as a GADT whose parameters
   encode the typing rules.

   The left parameter is the typed shape of the stack before the
   instruction, the right one the shape after. Any program whose
   construction is accepted by OCaml's type-checker is guaranteed to
   be type-safe. Overloadings of the concrete syntax are already
   resolved in this representation, either by using different
   constructors or type witness parameters.

   When adding a new instruction, please check whether it is duplicating a data
   (rule of thumb: the type variable appears twice in the after stack, beware
   it might be hidden in a witness).
   If it is, please protect it with [check_dupable_ty].
*)
and ('bef, 'aft) instr =
  (* stack ops *)
  | Drop : (_ * 'rest, 'rest) instr
  | Dup : ('top * 'rest, 'top * ('top * 'rest)) instr
  | Swap : ('tip * ('top * 'rest), 'top * ('tip * 'rest)) instr
  | Const : 'ty -> ('rest, 'ty * 'rest) instr
  (* pairs *)
  | Cons_pair : ('car * ('cdr * 'rest), ('car, 'cdr) pair * 'rest) instr
  | Car : (('car, _) pair * 'rest, 'car * 'rest) instr
  | Cdr : ((_, 'cdr) pair * 'rest, 'cdr * 'rest) instr
  | Unpair : (('car, 'cdr) pair * 'rest, 'car * ('cdr * 'rest)) instr
  (* options *)
  | Cons_some : ('v * 'rest, 'v option * 'rest) instr
  | Cons_none : 'a ty -> ('rest, 'a option * 'rest) instr
  | If_none :
      ('bef, 'aft) descr * ('a * 'bef, 'aft) descr
      -> ('a option * 'bef, 'aft) instr
  (* unions *)
  | Cons_left : ('l * 'rest, ('l, 'r) union * 'rest) instr
  | Cons_right : ('r * 'rest, ('l, 'r) union * 'rest) instr
  | If_left :
      ('l * 'bef, 'aft) descr * ('r * 'bef, 'aft) descr
      -> (('l, 'r) union * 'bef, 'aft) instr
  (* lists *)
  | Cons_list : ('a * ('a boxed_list * 'rest), 'a boxed_list * 'rest) instr
  | Nil : ('rest, 'a boxed_list * 'rest) instr
  | If_cons :
      ('a * ('a boxed_list * 'bef), 'aft) descr * ('bef, 'aft) descr
      -> ('a boxed_list * 'bef, 'aft) instr
  | List_map :
      ('a * 'rest, 'b * 'rest) descr
      -> ('a boxed_list * 'rest, 'b boxed_list * 'rest) instr
  | List_iter :
      ('a * 'rest, 'rest) descr
      -> ('a boxed_list * 'rest, 'rest) instr
  | List_size : ('a boxed_list * 'rest, n num * 'rest) instr
  (* sets *)
  | Empty_set : 'a comparable_ty -> ('rest, 'a set * 'rest) instr
  | Set_iter : ('a * 'rest, 'rest) descr -> ('a set * 'rest, 'rest) instr
  | Set_mem : ('elt * ('elt set * 'rest), bool * 'rest) instr
  | Set_update : ('elt * (bool * ('elt set * 'rest)), 'elt set * 'rest) instr
  | Set_size : ('a set * 'rest, n num * 'rest) instr
  (* maps *)
  | Empty_map : 'a comparable_ty * 'v ty -> ('rest, ('a, 'v) map * 'rest) instr
  | Map_map :
      (('a * 'v) * 'rest, 'r * 'rest) descr
      -> (('a, 'v) map * 'rest, ('a, 'r) map * 'rest) instr
  | Map_iter :
      (('a * 'v) * 'rest, 'rest) descr
      -> (('a, 'v) map * 'rest, 'rest) instr
  | Map_mem : ('a * (('a, 'v) map * 'rest), bool * 'rest) instr
  | Map_get : ('a * (('a, 'v) map * 'rest), 'v option * 'rest) instr
  | Map_update
      : ('a * ('v option * (('a, 'v) map * 'rest)), ('a, 'v) map * 'rest) instr
  | Map_get_and_update
      : ( 'a * ('v option * (('a, 'v) map * 'rest)),
          'v option * (('a, 'v) map * 'rest) )
        instr
  | Map_size : (('a, 'b) map * 'rest, n num * 'rest) instr
  (* big maps *)
  | Empty_big_map :
      'a comparable_ty * 'v ty
      -> ('rest, ('a, 'v) big_map * 'rest) instr
  | Big_map_mem : ('a * (('a, 'v) big_map * 'rest), bool * 'rest) instr
  | Big_map_get : ('a * (('a, 'v) big_map * 'rest), 'v option * 'rest) instr
  | Big_map_update
      : ( 'key * ('value option * (('key, 'value) big_map * 'rest)),
          ('key, 'value) big_map * 'rest )
        instr
  | Big_map_get_and_update
      : ( 'a * ('v option * (('a, 'v) big_map * 'rest)),
          'v option * (('a, 'v) big_map * 'rest) )
        instr
  (* string operations *)
  | Concat_string : (string boxed_list * 'rest, string * 'rest) instr
  | Concat_string_pair : (string * (string * 'rest), string * 'rest) instr
  | Slice_string
      : (n num * (n num * (string * 'rest)), string option * 'rest) instr
  | String_size : (string * 'rest, n num * 'rest) instr
  (* bytes operations *)
  | Concat_bytes : (bytes boxed_list * 'rest, bytes * 'rest) instr
  | Concat_bytes_pair : (bytes * (bytes * 'rest), bytes * 'rest) instr
  | Slice_bytes
      : (n num * (n num * (bytes * 'rest)), bytes option * 'rest) instr
  | Bytes_size : (bytes * 'rest, n num * 'rest) instr
  (* timestamp operations *)
  | Add_seconds_to_timestamp
      : ( z num * (Script_timestamp.t * 'rest),
          Script_timestamp.t * 'rest )
        instr
  | Add_timestamp_to_seconds
      : ( Script_timestamp.t * (z num * 'rest),
          Script_timestamp.t * 'rest )
        instr
  | Sub_timestamp_seconds
      : ( Script_timestamp.t * (z num * 'rest),
          Script_timestamp.t * 'rest )
        instr
  | Diff_timestamps
      : ( Script_timestamp.t * (Script_timestamp.t * 'rest),
          z num * 'rest )
        instr
  (* tez operations *)
  | Add_tez : (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Sub_tez : (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Mul_teznat : (Tez.t * (n num * 'rest), Tez.t * 'rest) instr
  | Mul_nattez : (n num * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Ediv_teznat
      : (Tez.t * (n num * 'rest), (Tez.t, Tez.t) pair option * 'rest) instr
  | Ediv_tez
      : (Tez.t * (Tez.t * 'rest), (n num, Tez.t) pair option * 'rest) instr
  (* boolean operations *)
  | Or : (bool * (bool * 'rest), bool * 'rest) instr
  | And : (bool * (bool * 'rest), bool * 'rest) instr
  | Xor : (bool * (bool * 'rest), bool * 'rest) instr
  | Not : (bool * 'rest, bool * 'rest) instr
  (* integer operations *)
  | Is_nat : (z num * 'rest, n num option * 'rest) instr
  | Neg_nat : (n num * 'rest, z num * 'rest) instr
  | Neg_int : (z num * 'rest, z num * 'rest) instr
  | Abs_int : (z num * 'rest, n num * 'rest) instr
  | Int_nat : (n num * 'rest, z num * 'rest) instr
  | Add_intint : (z num * (z num * 'rest), z num * 'rest) instr
  | Add_intnat : (z num * (n num * 'rest), z num * 'rest) instr
  | Add_natint : (n num * (z num * 'rest), z num * 'rest) instr
  | Add_natnat : (n num * (n num * 'rest), n num * 'rest) instr
  | Sub_int : ('s num * ('t num * 'rest), z num * 'rest) instr
  | Mul_intint : (z num * (z num * 'rest), z num * 'rest) instr
  | Mul_intnat : (z num * (n num * 'rest), z num * 'rest) instr
  | Mul_natint : (n num * (z num * 'rest), z num * 'rest) instr
  | Mul_natnat : (n num * (n num * 'rest), n num * 'rest) instr
  | Ediv_intint
      : (z num * (z num * 'rest), (z num, n num) pair option * 'rest) instr
  | Ediv_intnat
      : (z num * (n num * 'rest), (z num, n num) pair option * 'rest) instr
  | Ediv_natint
      : (n num * (z num * 'rest), (z num, n num) pair option * 'rest) instr
  | Ediv_natnat
      : (n num * (n num * 'rest), (n num, n num) pair option * 'rest) instr
  | Lsl_nat : (n num * (n num * 'rest), n num * 'rest) instr
  | Lsr_nat : (n num * (n num * 'rest), n num * 'rest) instr
  | Or_nat : (n num * (n num * 'rest), n num * 'rest) instr
  | And_nat : (n num * (n num * 'rest), n num * 'rest) instr
  | And_int_nat : (z num * (n num * 'rest), n num * 'rest) instr
  | Xor_nat : (n num * (n num * 'rest), n num * 'rest) instr
  | Not_nat : (n num * 'rest, z num * 'rest) instr
  | Not_int : (z num * 'rest, z num * 'rest) instr
  (* control *)
  | Seq : ('bef, 'trans) descr * ('trans, 'aft) descr -> ('bef, 'aft) instr
  | If : ('bef, 'aft) descr * ('bef, 'aft) descr -> (bool * 'bef, 'aft) instr
  | Loop : ('rest, bool * 'rest) descr -> (bool * 'rest, 'rest) instr
  | Loop_left :
      ('a * 'rest, ('a, 'b) union * 'rest) descr
      -> (('a, 'b) union * 'rest, 'b * 'rest) instr
  | Dip : ('bef, 'aft) descr -> ('top * 'bef, 'top * 'aft) instr
  | Exec : ('arg * (('arg, 'ret) lambda * 'rest), 'ret * 'rest) instr
  | Apply :
      'arg ty
      -> ( 'arg * (('arg * 'remaining, 'ret) lambda * 'rest),
           ('remaining, 'ret) lambda * 'rest )
         instr
  | Lambda : ('arg, 'ret) lambda -> ('rest, ('arg, 'ret) lambda * 'rest) instr
  | Failwith : 'a ty -> ('a * 'rest, 'aft) instr
  | Nop : ('rest, 'rest) instr
  (* comparison *)
  | Compare : 'a comparable_ty -> ('a * ('a * 'rest), z num * 'rest) instr
  (* comparators *)
  | Eq : (z num * 'rest, bool * 'rest) instr
  | Neq : (z num * 'rest, bool * 'rest) instr
  | Lt : (z num * 'rest, bool * 'rest) instr
  | Gt : (z num * 'rest, bool * 'rest) instr
  | Le : (z num * 'rest, bool * 'rest) instr
  | Ge : (z num * 'rest, bool * 'rest) instr
  (* protocol *)
  | Address : (_ typed_contract * 'rest, address * 'rest) instr
  | Contract :
      'p ty * string
      -> (address * 'rest, 'p typed_contract option * 'rest) instr
  | Transfer_tokens
      : ( 'arg * (Tez.t * ('arg typed_contract * 'rest)),
          operation * 'rest )
        instr
  | Implicit_account
      : (public_key_hash * 'rest, unit typed_contract * 'rest) instr
  | Create_contract :
      'g ty
      * 'p ty
      * ('p * 'g, operation boxed_list * 'g) lambda
      * field_annot option
      -> ( public_key_hash option * (Tez.t * ('g * 'rest)),
           operation * (address * 'rest) )
         instr
  | Set_delegate : (public_key_hash option * 'rest, operation * 'rest) instr
  | Now : ('rest, Script_timestamp.t * 'rest) instr
  | Balance : ('rest, Tez.t * 'rest) instr
  | Level : ('rest, n num * 'rest) instr
  | Check_signature
      : (public_key * (signature * (bytes * 'rest)), bool * 'rest) instr
  | Hash_key : (public_key * 'rest, public_key_hash * 'rest) instr
  | Pack : 'a ty -> ('a * 'rest, bytes * 'rest) instr
  | Unpack : 'a ty -> (bytes * 'rest, 'a option * 'rest) instr
  | Blake2b : (bytes * 'rest, bytes * 'rest) instr
  | Sha256 : (bytes * 'rest, bytes * 'rest) instr
  | Sha512 : (bytes * 'rest, bytes * 'rest) instr
  | Source : ('rest, address * 'rest) instr
  | Sender : ('rest, address * 'rest) instr
  | Self : 'p ty * string -> ('rest, 'p typed_contract * 'rest) instr
  | Self_address : ('rest, address * 'rest) instr
  | Amount : ('rest, Tez.t * 'rest) instr
  | Sapling_empty_state : {
      memo_size : Sapling.Memo_size.t;
    }
      -> ('rest, Sapling.state * 'rest) instr
  | Sapling_verify_update
      : ( Sapling.transaction * (Sapling.state * 'rest),
          (z num, Sapling.state) pair option * 'rest )
        instr
  | Dig :
      int * ('x * 'rest, 'rest, 'bef, 'aft) stack_prefix_preservation_witness
      -> ('bef, 'x * 'aft) instr
  | Dug :
      int * ('rest, 'x * 'rest, 'bef, 'aft) stack_prefix_preservation_witness
      -> ('x * 'bef, 'aft) instr
  | Dipn :
      int
      * ('fbef, 'faft, 'bef, 'aft) stack_prefix_preservation_witness
      * ('fbef, 'faft) descr
      -> ('bef, 'aft) instr
  | Dropn :
      int * ('rest, 'rest, 'bef, 'bef) stack_prefix_preservation_witness
      -> ('bef, 'rest) instr
  | ChainId : ('rest, Chain_id.t * 'rest) instr
  | Never : (never * 'rest, 'aft) instr
  | Voting_power : (public_key_hash * 'rest, n num * 'rest) instr
  | Total_voting_power : ('rest, n num * 'rest) instr
  | Keccak : (bytes * 'rest, bytes * 'rest) instr
  | Sha3 : (bytes * 'rest, bytes * 'rest) instr
  | Add_bls12_381_g1
      : ( Bls12_381.G1.t * (Bls12_381.G1.t * 'rest),
          Bls12_381.G1.t * 'rest )
        instr
  | Add_bls12_381_g2
      : ( Bls12_381.G2.t * (Bls12_381.G2.t * 'rest),
          Bls12_381.G2.t * 'rest )
        instr
  | Add_bls12_381_fr
      : ( Bls12_381.Fr.t * (Bls12_381.Fr.t * 'rest),
          Bls12_381.Fr.t * 'rest )
        instr
  | Mul_bls12_381_g1
      : ( Bls12_381.G1.t * (Bls12_381.Fr.t * 'rest),
          Bls12_381.G1.t * 'rest )
        instr
  | Mul_bls12_381_g2
      : ( Bls12_381.G2.t * (Bls12_381.Fr.t * 'rest),
          Bls12_381.G2.t * 'rest )
        instr
  | Mul_bls12_381_fr
      : ( Bls12_381.Fr.t * (Bls12_381.Fr.t * 'rest),
          Bls12_381.Fr.t * 'rest )
        instr
  | Mul_bls12_381_z_fr
      : (Bls12_381.Fr.t * (_ num * 'rest), Bls12_381.Fr.t * 'rest) instr
  | Mul_bls12_381_fr_z
      : (_ num * (Bls12_381.Fr.t * 'rest), Bls12_381.Fr.t * 'rest) instr
  | Int_bls12_381_fr : (Bls12_381.Fr.t * 'rest, z num * 'rest) instr
  | Neg_bls12_381_g1 : (Bls12_381.G1.t * 'rest, Bls12_381.G1.t * 'rest) instr
  | Neg_bls12_381_g2 : (Bls12_381.G2.t * 'rest, Bls12_381.G2.t * 'rest) instr
  | Neg_bls12_381_fr : (Bls12_381.Fr.t * 'rest, Bls12_381.Fr.t * 'rest) instr
  | Pairing_check_bls12_381
      : ( (Bls12_381.G1.t, Bls12_381.G2.t) pair boxed_list * 'rest,
          bool * 'rest )
        instr
  | Comb : int * ('before, 'after) comb_gadt_witness -> ('before, 'after) instr
  | Uncomb :
      int * ('before, 'after) uncomb_gadt_witness
      -> ('before, 'after) instr
  | Comb_get :
      int * ('before, 'after) comb_get_gadt_witness
      -> ('before * 'rest, 'after * 'rest) instr
  | Comb_set :
      int * ('value, 'before, 'after) comb_set_gadt_witness
      -> ('value * ('before * 'rest), 'after * 'rest) instr
  | Dup_n :
      int * ('before, 'after) dup_n_gadt_witness
      -> ('before, 'after * 'before) instr
  | Ticket : ('a * (n num * 'rest), 'a ticket * 'rest) instr
  | Read_ticket
      : ( 'a ticket * 'rest,
          (address * ('a * n num)) * ('a ticket * 'rest) )
        instr
  | Split_ticket
      : ( 'a ticket * ((n num * n num) * 'rest),
          ('a ticket * 'a ticket) option * 'rest )
        instr
  | Join_tickets :
      'a comparable_ty
      -> (('a ticket * 'a ticket) * 'rest, 'a ticket option * 'rest) instr

and ('before, 'after) comb_gadt_witness =
  | Comb_one : ('a * 'before, 'a * 'before) comb_gadt_witness
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

   [dup_n_gadt_witness ('s, 't)] ensures that the n-th element of ['s]
   is of type ['t].

   This relational predicate is defined by induction on [n].

*)
and (_, _) dup_n_gadt_witness =
  | Dup_n_zero : ('a * 'rest, 'a) dup_n_gadt_witness
  | Dup_n_succ :
      ('stack, 'b) dup_n_gadt_witness
      -> ('a * 'stack, 'b) dup_n_gadt_witness

(*

   Type witness for operations that work deep in the stack ignoring
   (and preserving) a prefix.

   The two right parameters are the shape of the stack with the (same)
   prefix before and after the transformation. The two left parameters
   are the shape of the stack without the prefix before and after.

   This relational predicate is defined by induction on the common
   prefix of the two topmost stacks.

*)
and ('bef, 'aft, 'bef_suffix, 'aft_suffix) stack_prefix_preservation_witness =
  | Prefix :
      ('fbef, 'faft, 'bef, 'aft) stack_prefix_preservation_witness
      -> ('fbef, 'faft, 'x * 'bef, 'x * 'aft) stack_prefix_preservation_witness
  | Rest : ('bef, 'aft, 'bef, 'aft) stack_prefix_preservation_witness

and ('bef, 'aft) descr = {
  loc : Script.location;
  bef : 'bef stack_ty;
  aft : 'aft stack_ty;
  instr : ('bef, 'aft) instr;
}

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

let unlist_ty : type a. a boxed_list ty -> a ty = function
  | List_t (a, _) ->
      a
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false

let unset_ty : type a. a set ty -> a ty = function
  | Set_t (a, _) ->
      ty_of_comparable_ty a
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false

let unmap_ty : type k v. (k, v) map ty -> k ty * v ty = function
  | Map_t (k, v, _) ->
      (ty_of_comparable_ty k, v)
  | _ ->
      (* FIXME: This is not robust to evolutions. *)
      (* because of the concrete implementations of the type
        constructors occurring in the definition of [ty]: *)
      assert false
