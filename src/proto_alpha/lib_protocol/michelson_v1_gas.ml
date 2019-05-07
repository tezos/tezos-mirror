(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Gas

module Cost_of = struct

  let log2 =
    let rec help acc = function
      | 0 -> acc
      | n -> help (acc + 1) (n / 2)
    in help 1

  let z_bytes (z : Z.t) =
    let bits = Z.numbits z in
    (7 + bits) / 8

  let int_bytes (z : 'a Script_int.num) =
    z_bytes (Script_int.to_zint z)

  let timestamp_bytes (t : Script_timestamp.t) =
    let z = Script_timestamp.to_zint t in
    z_bytes z

  (* For now, returns size in bytes, but this could get more complicated... *)
  let rec size_of_comparable : type a b. (a, b) Script_typed_ir.comparable_struct -> a -> int =
    fun wit v ->
      match wit with
      | Int_key _ -> int_bytes v
      | Nat_key _ -> int_bytes v
      | String_key _ -> String.length v
      | Bytes_key _ -> MBytes.length v
      | Bool_key _ -> 8
      | Key_hash_key _ -> Signature.Public_key_hash.size
      | Timestamp_key _ -> timestamp_bytes v
      | Address_key _ -> Signature.Public_key_hash.size
      | Mutez_key _ -> 8
      | Pair_key ((l, _), (r, _), _) ->
          let (lval, rval) = v in
          size_of_comparable l lval + size_of_comparable r rval

  let string length =
    alloc_bytes_cost length

  let bytes length =
    alloc_mbytes_cost length

  let manager_operation = step_cost 10_000

  module Legacy = struct
    let zint z =
      alloc_bits_cost (Z.numbits z)

    let set_to_list : type item. item Script_typed_ir.set -> cost
      = fun (module Box) ->
        alloc_cost @@ Pervasives.(Box.size * 2)

    let map_to_list : type key value. (key, value) Script_typed_ir.map -> cost
      = fun (module Box) ->
        let size = snd Box.boxed in
        3 *@ alloc_cost size

    let z_to_int64 = step_cost 2 +@ alloc_cost 1

    let hash data len = 10 *@ step_cost (MBytes.length data) +@ bytes len

    let set_access : type elt. elt -> elt Script_typed_ir.set -> int
      = fun _key (module Box) ->
        log2 @@ Box.size

    let set_update key _presence set =
      set_access key set *@ alloc_cost 3
  end

  module Interpreter = struct
    let cycle = atomic_step_cost 10
    let nop = free
    let stack_op = atomic_step_cost 10
    let push = atomic_step_cost 10
    let wrap = atomic_step_cost 10
    let variant_no_data = atomic_step_cost 10
    let branch = atomic_step_cost 10
    let pair = atomic_step_cost 10
    let pair_access = atomic_step_cost 10
    let cons = atomic_step_cost 10
    let loop_size = atomic_step_cost 5
    let loop_cycle = atomic_step_cost 10
    let loop_iter = atomic_step_cost 20
    let loop_map = atomic_step_cost 30
    let empty_set = atomic_step_cost 10
    let set_to_list : type elt. elt Script_typed_ir.set -> cost =
      fun (module Box) ->
        atomic_step_cost (Box.size * 20)

    let set_mem : type elt. elt -> elt Script_typed_ir.set -> cost =
      fun elt (module Box) ->
        let elt_bytes = size_of_comparable Box.elt_ty elt in
        atomic_step_cost ((1 + (elt_bytes / 82)) * log2 Box.size)

    let set_update : type elt. elt -> bool -> elt Script_typed_ir.set -> cost =
      fun elt _ (module Box) ->
        let elt_bytes = size_of_comparable Box.elt_ty elt in
        atomic_step_cost ((1 + (elt_bytes / 82)) * log2 Box.size)

    let set_size = atomic_step_cost 10
    let empty_map = atomic_step_cost 10
    let map_to_list : type key value. (key, value) Script_typed_ir.map -> cost =
      fun (module Box) ->
        let size = snd Box.boxed in
        atomic_step_cost (size * 20)

    let map_access : type key value. key -> (key, value) Script_typed_ir.map -> cost
      = fun key (module Box) ->
        let map_card  = snd Box.boxed in
        let key_bytes = size_of_comparable Box.key_ty key in
        atomic_step_cost ((1 + (key_bytes / 70)) * log2 map_card)

    let map_mem = map_access
    let map_get = map_access

    let map_update : type key value. key -> value option -> (key, value) Script_typed_ir.map -> cost
      = fun key _value (module Box) ->
        let map_card  = snd Box.boxed in
        let key_bytes = size_of_comparable Box.key_ty key in
        atomic_step_cost ((1 + (key_bytes / 38)) * log2 map_card)

    let map_size = atomic_step_cost 10

    let add_timestamp (t1 : Script_timestamp.t) (t2 : 'a Script_int.num) =
      let bytes1 = timestamp_bytes t1 in
      let bytes2 = int_bytes t2 in
      atomic_step_cost (51 + (Compare.Int.max bytes1 bytes2 / 62))
    let sub_timestamp = add_timestamp
    let diff_timestamps (t1 : Script_timestamp.t) (t2 : Script_timestamp.t) =
      let bytes1 = timestamp_bytes t1 in
      let bytes2 = timestamp_bytes t2 in
      atomic_step_cost (51 + (Compare.Int.max bytes1 bytes2 / 62))

    let rec concat_loop l acc =
      match l with
      | [] -> 30
      | _ :: tl -> concat_loop tl (acc + 30)

    let concat_string string_list =
      atomic_step_cost (concat_loop string_list 0)

    let slice_string string_length =
      atomic_step_cost (40 + (string_length / 70))

    let concat_bytes bytes_list =
      atomic_step_cost (concat_loop bytes_list 0)

    let int64_op = atomic_step_cost 61
    let z_to_int64 = atomic_step_cost 20
    let int64_to_z = atomic_step_cost 20
    let bool_binop _ _ = atomic_step_cost 10
    let bool_unop _ = atomic_step_cost 10

    let abs int = atomic_step_cost (61 + ((int_bytes int) / 70))
    let int _int = free
    let neg = abs
    let add i1 i2 = atomic_step_cost (51 + (Compare.Int.max (int_bytes i1) (int_bytes i2) / 62))
    let sub = add

    let mul i1 i2 =
      let bytes = Compare.Int.max (int_bytes i1) (int_bytes i2) in
      atomic_step_cost (51 + (bytes / 6 * log2 bytes))

    let indic_lt x y = if Compare.Int.(x < y) then 1 else 0

    let div i1 i2 =
      let bytes1 = int_bytes i1 in
      let bytes2 = int_bytes i2 in
      let cost = indic_lt bytes2 bytes1 * (bytes1 - bytes2) * bytes2 in
      atomic_step_cost (51 + (cost / 3151))

    let shift_left _i _shift_bits = atomic_step_cost 30
    let shift_right _i _shift_bits = atomic_step_cost 30
    let logor i1 i2 =
      let bytes1 = int_bytes i1 in
      let bytes2 = int_bytes i2 in
      atomic_step_cost (51 + ((Compare.Int.max bytes1 bytes2) / 70))
    let logand i1 i2 =
      let bytes1 = int_bytes i1 in
      let bytes2 = int_bytes i2 in
      atomic_step_cost (51 + ((Compare.Int.min bytes1 bytes2) / 70))
    let logxor = logor
    let lognot i = atomic_step_cost (51 + ((int_bytes i) / 20))
    let exec = atomic_step_cost 10
    let compare_bool _ _ = atomic_step_cost 30

    let compare_string s1 s2 =
      let bytes1 = String.length s1 in
      let bytes2 = String.length s2 in
      atomic_step_cost (30 + ((Compare.Int.min bytes1 bytes2) / 123))
    let compare_bytes b1 b2 =
      let bytes1 = MBytes.length b1 in
      let bytes2 = MBytes.length b2 in
      atomic_step_cost (30 + ((Compare.Int.min bytes1 bytes2) / 123))
    let compare_tez _ _ = atomic_step_cost 30
    let compare_zint i1 i2 =
      atomic_step_cost (51 + ((Compare.Int.min (int_bytes i1) (int_bytes i2)) / 82))
    let compare_key_hash _ _ = atomic_step_cost 92

    let compare_timestamp t1 t2 =
      let bytes1 = timestamp_bytes t1 in
      let bytes2 = timestamp_bytes t2 in
      atomic_step_cost (51 + ((Compare.Int.min bytes1 bytes2) / 82))

    let compare_address _ _ = atomic_step_cost 92
    let compare_res = atomic_step_cost 30
    let unpack_failed bytes =
      (* We cannot instrument failed deserialization,
         so we take worst case fees: a set of size 1 bytes values. *)
      let len = MBytes.length bytes in
      (len *@ alloc_mbytes_cost 1) +@
      (len *@ (log2 len *@ (alloc_cost 3 +@ step_cost 1)))
    let address = atomic_step_cost 10
    let contract = step_cost 10000
    let transfer = step_cost 10
    let create_account = step_cost 10
    let create_contract = step_cost 10
    let implicit_account = step_cost 10
    let set_delegate = step_cost 10 +@ write_bytes_cost (Z.of_int 32)
    let balance = atomic_step_cost 10
    let now = atomic_step_cost 10
    let check_signature_secp256k1 bytes = atomic_step_cost (10342 + (bytes / 5))
    let check_signature_ed25519 bytes = atomic_step_cost (36864 + (bytes / 5))
    let check_signature_p256 bytes = atomic_step_cost (36864 + (bytes / 5))
    let check_signature (pkey : Signature.public_key) bytes =
      match pkey with
      | Ed25519 _ -> check_signature_ed25519 (MBytes.length bytes)
      | Secp256k1 _ -> check_signature_secp256k1 (MBytes.length bytes)
      | P256 _ ->  check_signature_p256 (MBytes.length bytes)
    let hash_key = atomic_step_cost 30
    let hash_blake2b b = atomic_step_cost (102 + ((MBytes.length b) / 5))
    let hash_sha256 b = atomic_step_cost (409 + (MBytes.length b))
    let hash_sha512 b =
      let bytes = MBytes.length b in atomic_step_cost (409 + ((bytes lsr 1) + (bytes lsr 4)))
    let steps_to_quota = atomic_step_cost 10
    let source = atomic_step_cost 10
    let self = atomic_step_cost 10
    let amount = atomic_step_cost 10
    let chain_id = step_cost 1
    let stack_n_op n = atomic_step_cost (20 + (((n lsr 1) + (n lsr 2)) + (n lsr 4)))
    let apply = alloc_cost 8 +@ step_cost 1

    let rec compare : type a s. (a, s) Script_typed_ir.comparable_struct -> a -> a -> cost = fun ty x y ->
      match ty with
      | Bool_key _ -> compare_bool x y
      | String_key _ -> compare_string x y
      | Bytes_key _ -> compare_bytes x y
      | Mutez_key _ -> compare_tez x y
      | Int_key _ -> compare_zint x y
      | Nat_key _ -> compare_zint x y
      | Key_hash_key _ -> compare_key_hash x y
      | Timestamp_key _ -> compare_timestamp x y
      | Address_key _ -> compare_address x y
      | Pair_key ((tl, _), (tr, _), _) ->
          (* Reasonable over-approximation of the cost of lexicographic comparison. *)
          let (xl, xr) = x and (yl, yr) = y in
          compare tl xl yl +@ compare tr xr yr

  end

  module Typechecking = struct
    let cycle = step_cost 1
    let bool = free
    let unit = free
    let string = string
    let bytes = bytes
    let z = Legacy.zint
    let int_of_string str =
      alloc_cost @@ (Pervasives.(/) (String.length str) 5)
    let tez = step_cost 1 +@ alloc_cost 1
    let string_timestamp = step_cost 3 +@ alloc_cost 3
    let key = step_cost 3 +@ alloc_cost 3
    let key_hash = step_cost 1 +@ alloc_cost 1
    let signature = step_cost 1 +@ alloc_cost 1
    let chain_id = step_cost 1 +@ alloc_cost 1
    let contract = step_cost 5
    let get_script = step_cost 20 +@ alloc_cost 5
    let contract_exists = step_cost 15 +@ alloc_cost 5
    let pair = alloc_cost 2
    let union = alloc_cost 1
    let lambda = alloc_cost 5 +@ step_cost 3
    let some = alloc_cost 1
    let none = alloc_cost 0
    let list_element = alloc_cost 2 +@ step_cost 1
    let set_element size = log2 size *@ (alloc_cost 3 +@ step_cost 2)
    let map_element size = log2 size *@ (alloc_cost 4 +@ step_cost 2)
    let primitive_type = alloc_cost 1
    let one_arg_type = alloc_cost 2
    let two_arg_type = alloc_cost 3
    let operation b = bytes b
    let type_ nb_args = alloc_cost (nb_args + 1)

    (* Cost of parsing instruction, is cost of allocation of
       constructor + cost of contructor parameters + cost of
       allocation on the stack type *)
    let instr
      : type b a. (b, a) Script_typed_ir.instr -> cost
      = fun i ->
        let open Script_typed_ir in
        alloc_cost 1 +@ (* cost of allocation of constructor *)
        match i with
        | Drop -> alloc_cost 0
        | Dup -> alloc_cost 1
        | Swap -> alloc_cost 0
        | Const _ -> alloc_cost 1
        | Cons_pair -> alloc_cost 2
        | Car -> alloc_cost 1
        | Cdr -> alloc_cost 1
        | Cons_some -> alloc_cost 2
        | Cons_none _ -> alloc_cost 3
        | If_none _ -> alloc_cost 2
        | Left -> alloc_cost 3
        | Right -> alloc_cost 3
        | If_left _ -> alloc_cost 2
        | Cons_list -> alloc_cost 1
        | Nil -> alloc_cost 1
        | If_cons _ -> alloc_cost 2
        | List_map _ -> alloc_cost 5
        | List_iter _ -> alloc_cost 4
        | List_size -> alloc_cost 1
        | Empty_set _ -> alloc_cost 1
        | Set_iter _ -> alloc_cost 4
        | Set_mem -> alloc_cost 1
        | Set_update -> alloc_cost 1
        | Set_size -> alloc_cost 1
        | Empty_map _ -> alloc_cost 2
        | Map_map _ -> alloc_cost 5
        | Map_iter _ -> alloc_cost 4
        | Map_mem -> alloc_cost 1
        | Map_get -> alloc_cost 1
        | Map_update -> alloc_cost 1
        | Map_size -> alloc_cost 1
        | Empty_big_map _ -> alloc_cost 2
        | Big_map_mem -> alloc_cost 1
        | Big_map_get -> alloc_cost 1
        | Big_map_update -> alloc_cost 1
        | Concat_string -> alloc_cost 1
        | Concat_string_pair -> alloc_cost 1
        | Concat_bytes -> alloc_cost 1
        | Concat_bytes_pair -> alloc_cost 1
        | Slice_string -> alloc_cost 1
        | Slice_bytes -> alloc_cost 1
        | String_size -> alloc_cost 1
        | Bytes_size -> alloc_cost 1
        | Add_seconds_to_timestamp -> alloc_cost 1
        | Add_timestamp_to_seconds -> alloc_cost 1
        | Sub_timestamp_seconds -> alloc_cost 1
        | Diff_timestamps -> alloc_cost 1
        | Add_tez -> alloc_cost 1
        | Sub_tez -> alloc_cost 1
        | Mul_teznat -> alloc_cost 1
        | Mul_nattez -> alloc_cost 1
        | Ediv_teznat -> alloc_cost 1
        | Ediv_tez -> alloc_cost 1
        | Or -> alloc_cost 1
        | And -> alloc_cost 1
        | Xor -> alloc_cost 1
        | Not -> alloc_cost 1
        | Is_nat -> alloc_cost 1
        | Neg_nat -> alloc_cost 1
        | Neg_int -> alloc_cost 1
        | Abs_int -> alloc_cost 1
        | Int_nat -> alloc_cost 1
        | Add_intint -> alloc_cost 1
        | Add_intnat -> alloc_cost 1
        | Add_natint -> alloc_cost 1
        | Add_natnat -> alloc_cost 1
        | Sub_int -> alloc_cost 1
        | Mul_intint -> alloc_cost 1
        | Mul_intnat -> alloc_cost 1
        | Mul_natint -> alloc_cost 1
        | Mul_natnat -> alloc_cost 1
        | Ediv_intint -> alloc_cost 1
        | Ediv_intnat -> alloc_cost 1
        | Ediv_natint -> alloc_cost 1
        | Ediv_natnat -> alloc_cost 1
        | Lsl_nat -> alloc_cost 1
        | Lsr_nat -> alloc_cost 1
        | Or_nat -> alloc_cost 1
        | And_nat -> alloc_cost 1
        | And_int_nat -> alloc_cost 1
        | Xor_nat -> alloc_cost 1
        | Not_nat -> alloc_cost 1
        | Not_int -> alloc_cost 1
        | Seq _ -> alloc_cost 8
        | If _ -> alloc_cost 8
        | Loop _ -> alloc_cost 4
        | Loop_left _ -> alloc_cost 5
        | Dip _ -> alloc_cost 4
        | Exec -> alloc_cost 1
        | Apply _ -> alloc_cost 1
        | Lambda _ -> alloc_cost 2
        | Failwith _ -> alloc_cost 1
        | Nop -> alloc_cost 0
        | Compare _ -> alloc_cost 1
        | Eq -> alloc_cost 1
        | Neq -> alloc_cost 1
        | Lt -> alloc_cost 1
        | Gt -> alloc_cost 1
        | Le -> alloc_cost 1
        | Ge -> alloc_cost 1
        | Address -> alloc_cost 1
        | Contract _ -> alloc_cost 2
        | Transfer_tokens -> alloc_cost 1
        | Create_account -> alloc_cost 2
        | Implicit_account -> alloc_cost 1
        | Create_contract _ -> alloc_cost 8
        (* Deducted the cost of removed arguments manager, spendable and delegatable:
           - manager: key_hash = 1
           - spendable: bool = 0
           - delegatable: bool = 0
        *)
        | Create_contract_2 _ -> alloc_cost 7
        | Set_delegate -> alloc_cost 1
        | Now -> alloc_cost 1
        | Balance -> alloc_cost 1
        | Check_signature -> alloc_cost 1
        | Hash_key -> alloc_cost 1
        | Pack _ -> alloc_cost 2
        | Unpack _ -> alloc_cost 2
        | Blake2b -> alloc_cost 1
        | Sha256 -> alloc_cost 1
        | Sha512 -> alloc_cost 1
        | Steps_to_quota -> alloc_cost 1
        | Source -> alloc_cost 1
        | Sender -> alloc_cost 1
        | Self _ -> alloc_cost 2
        | Amount -> alloc_cost 1
        | Dig (n,_) -> n *@ alloc_cost 1 (* _ is a unary development of n *)
        | Dug (n,_) -> n *@ alloc_cost 1
        | Dipn (n,_,_) -> n *@ alloc_cost 1
        | Dropn (n,_) -> n *@ alloc_cost 1
        | ChainId -> alloc_cost 1
  end

  module Unparse = struct
    let prim_cost l annot = Script.prim_node_cost_nonrec_of_length l annot
    let seq_cost = Script.seq_node_cost_nonrec_of_length
    let string_cost length = Script.string_node_cost_of_length length

    let cycle = step_cost 1
    let bool = prim_cost 0 []
    let unit = prim_cost 0 []
    (* We count the length of strings and bytes to prevent hidden
       miscalculations due to non detectable expansion of sharing. *)
    let string s = Script.string_node_cost s
    let bytes s = Script.bytes_node_cost s
    let z i = Script.int_node_cost i
    let int i = Script.int_node_cost (Script_int.to_zint i)
    let tez = Script.int_node_cost_of_numbits 60 (* int64 bound *)
    let timestamp x = Script_timestamp.to_zint x |> Script_int.of_zint |> int
    let operation bytes = Script.bytes_node_cost bytes
    let chain_id bytes = Script.bytes_node_cost bytes
    let key = string_cost 54
    let key_hash = string_cost 36
    let signature = string_cost 128
    let contract = string_cost 36
    let pair = prim_cost 2 []
    let union = prim_cost 1 []
    let some = prim_cost 1 []
    let none = prim_cost 0 []
    let list_element = alloc_cost 2
    let set_element = alloc_cost 2
    let map_element = alloc_cost 2
    let one_arg_type = prim_cost 1
    let two_arg_type = prim_cost 2

    let set_to_list = Legacy.set_to_list
    let map_to_list = Legacy.map_to_list
  end

end
