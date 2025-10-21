(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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
module S = Saturation_repr
module Size = Gas_input_size

module Cost_of = struct
  let z_bytes (z : Z.t) =
    let bits = Z.numbits z in
    (7 + bits) / 8

  let int_size_in_bytes (z : 'a Script_int.num) = z_bytes (Script_int.to_zint z)

  let manager_operation_int = 100

  let manager_operation = step_cost @@ S.safe_int manager_operation_int

  let transfer_operation_int = 2000

  let transfer_operation = step_cost @@ S.safe_int transfer_operation_int

  module Interpreter = struct
    open Michelson_v1_gas_costs

    let drop = atomic_step_cost cost_N_IDrop

    let dup = atomic_step_cost cost_N_IDup

    let swap = atomic_step_cost cost_N_ISwap

    let cons_some = atomic_step_cost cost_N_ICons_some

    let cons_none = atomic_step_cost cost_N_ICons_none

    let if_none = atomic_step_cost cost_N_IIf_none

    let opt_map = atomic_step_cost cost_N_IOpt_map

    let cons_pair = atomic_step_cost cost_N_ICons_pair

    let unpair = atomic_step_cost cost_N_IUnpair

    let car = atomic_step_cost cost_N_ICar

    let cdr = atomic_step_cost cost_N_ICdr

    let cons_left = atomic_step_cost cost_N_ILeft

    let cons_right = atomic_step_cost cost_N_IRight

    let if_left = atomic_step_cost cost_N_IIf_left

    let cons_list = atomic_step_cost cost_N_ICons_list

    let nil = atomic_step_cost cost_N_INil

    let if_cons = atomic_step_cost cost_N_IIf_cons

    let list_map : 'a Script_list.t -> Gas.cost =
     fun _ -> atomic_step_cost cost_N_IList_map

    let list_size = atomic_step_cost cost_N_IList_size

    let list_iter : 'a Script_list.t -> Gas.cost =
     fun _ -> atomic_step_cost cost_N_IList_iter

    let empty_set = atomic_step_cost cost_N_IEmpty_set

    let set_iter (type a) (set : a Script_typed_ir.set) =
      let (module Box) = Script_set.get set in
      atomic_step_cost (cost_N_ISet_iter Box.size)

    let set_size = atomic_step_cost cost_N_ISet_size

    let empty_map = atomic_step_cost cost_N_IEmpty_map

    let map_map (type k v) (map : (k, v) Script_typed_ir.map) =
      let (module Box) = Script_map.get_module map in
      atomic_step_cost (cost_N_IMap_map Box.size)

    let map_iter (type k v) (map : (k, v) Script_typed_ir.map) =
      let (module Box) = Script_map.get_module map in
      atomic_step_cost (cost_N_IMap_iter Box.size)

    let map_size = atomic_step_cost cost_N_IMap_size

    let big_map_elt_size = Script_expr_hash.size

    (* The uses of [cost_N_IMap_*] below are intentional.  They are for
       the cost of the big_map overlay. The other costs such as the storage
       access and the deserialization are separately charged in the protocol.
       We don't use [cost_N_IBig_map_*] here, since they include these partial
       carbonations. *)
    let big_map_mem ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_mem big_map_elt_size size)

    let big_map_get ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_get big_map_elt_size size)

    let big_map_update ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_update big_map_elt_size size)

    let big_map_get_and_update ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_get_and_update big_map_elt_size size)

    let add_seconds_timestamp :
        'a Script_int.num -> Script_timestamp.t -> Gas.cost =
     fun seconds timestamp ->
      let seconds_bytes = int_size_in_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost
        (cost_N_IAdd_seconds_to_timestamp seconds_bytes timestamp_bytes)

    let add_timestamp_seconds :
        Script_timestamp.t -> 'a Script_int.num -> Gas.cost =
     fun timestamp seconds ->
      let seconds_bytes = int_size_in_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost
        (cost_N_IAdd_timestamp_to_seconds timestamp_bytes seconds_bytes)

    let sub_timestamp_seconds :
        Script_timestamp.t -> 'a Script_int.num -> Gas.cost =
     fun timestamp seconds ->
      let seconds_bytes = int_size_in_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost
        (cost_N_ISub_timestamp_seconds timestamp_bytes seconds_bytes)

    let diff_timestamps t1 t2 =
      let t1_bytes = z_bytes (Script_timestamp.to_zint t1) in
      let t2_bytes = z_bytes (Script_timestamp.to_zint t2) in
      atomic_step_cost (cost_N_IDiff_timestamps t1_bytes t2_bytes)

    let concat_string_pair s1 s2 =
      atomic_step_cost
        (cost_N_IConcat_string_pair
           (Script_string.length s1)
           (Script_string.length s2))

    let slice_string s =
      atomic_step_cost (cost_N_ISlice_string (Script_string.length s))

    let string_size = atomic_step_cost cost_N_IString_size

    let concat_bytes_pair b1 b2 =
      atomic_step_cost
        (cost_N_IConcat_bytes_pair (Bytes.length b1) (Bytes.length b2))

    let slice_bytes b = atomic_step_cost (cost_N_ISlice_bytes (Bytes.length b))

    let bytes_size = atomic_step_cost cost_N_IBytes_size

    let lsl_bytes input nbits =
      match Script_int.to_int nbits with
      | None -> Saturation_repr.saturated
      | Some nbits ->
          atomic_step_cost (cost_N_ILsl_bytes (Bytes.length input) nbits)

    let lsr_bytes input nbits =
      let input_nbytes = Bytes.length input in
      let nbits =
        Option.value (Script_int.to_int nbits) ~default:(input_nbytes * 8)
      in
      atomic_step_cost (cost_N_ILsr_bytes input_nbytes nbits)

    let or_bytes b1 b2 =
      atomic_step_cost (cost_N_IOr_bytes (Bytes.length b1) (Bytes.length b2))

    let and_bytes b1 b2 =
      atomic_step_cost (cost_N_IAnd_bytes (Bytes.length b1) (Bytes.length b2))

    let xor_bytes b1 b2 =
      atomic_step_cost (cost_N_IXor_bytes (Bytes.length b1) (Bytes.length b2))

    let not_bytes b = atomic_step_cost (cost_N_INot_bytes (Bytes.length b))

    let bytes_nat n = atomic_step_cost (cost_N_IBytes_nat (int_size_in_bytes n))

    let nat_bytes b = atomic_step_cost (cost_N_INat_bytes (Bytes.length b))

    let bytes_int n = atomic_step_cost (cost_N_IBytes_int (int_size_in_bytes n))

    let int_bytes b = atomic_step_cost (cost_N_IInt_bytes (Bytes.length b))

    let add_tez = atomic_step_cost cost_N_IAdd_tez

    let sub_tez = atomic_step_cost cost_N_ISub_tez

    let sub_tez_legacy = atomic_step_cost cost_N_ISub_tez_legacy

    let mul_teznat = atomic_step_cost cost_N_IMul_teznat

    let mul_nattez = atomic_step_cost cost_N_IMul_nattez

    let bool_or = atomic_step_cost cost_N_IOr

    let bool_and = atomic_step_cost cost_N_IAnd

    let bool_xor = atomic_step_cost cost_N_IXor

    let bool_not = atomic_step_cost cost_N_INot

    let is_nat = atomic_step_cost cost_N_IIs_nat

    let abs_int i = atomic_step_cost (cost_N_IAbs_int (int_size_in_bytes i))

    let int_nat = atomic_step_cost cost_N_IInt_nat

    let neg i = atomic_step_cost (cost_N_INeg (int_size_in_bytes i))

    let add_int i1 i2 =
      atomic_step_cost
        (cost_N_IAdd_int (int_size_in_bytes i1) (int_size_in_bytes i2))

    let add_nat i1 i2 =
      atomic_step_cost
        (cost_N_IAdd_nat (int_size_in_bytes i1) (int_size_in_bytes i2))

    let sub_int i1 i2 =
      atomic_step_cost
        (cost_N_ISub_int (int_size_in_bytes i1) (int_size_in_bytes i2))

    let mul_int i1 i2 =
      atomic_step_cost
        (cost_N_IMul_int (int_size_in_bytes i1) (int_size_in_bytes i2))

    let mul_nat i1 i2 =
      atomic_step_cost
        (cost_N_IMul_nat (int_size_in_bytes i1) (int_size_in_bytes i2))

    let ediv_teznat _tez _n = atomic_step_cost cost_N_IEdiv_teznat

    let ediv_tez = atomic_step_cost cost_N_IEdiv_tez

    let ediv_int i1 i2 =
      atomic_step_cost
        (cost_N_IEdiv_int (int_size_in_bytes i1) (int_size_in_bytes i2))

    let ediv_nat i1 i2 =
      atomic_step_cost
        (cost_N_IEdiv_nat (int_size_in_bytes i1) (int_size_in_bytes i2))

    let eq = atomic_step_cost cost_N_IEq

    let lsl_nat shifted =
      atomic_step_cost (cost_N_ILsl_nat (int_size_in_bytes shifted))

    let lsr_nat shifted =
      atomic_step_cost (cost_N_ILsr_nat (int_size_in_bytes shifted))

    let or_nat n1 n2 =
      atomic_step_cost
        (cost_N_IOr_nat (int_size_in_bytes n1) (int_size_in_bytes n2))

    let and_nat n1 n2 =
      atomic_step_cost
        (cost_N_IAnd_nat (int_size_in_bytes n1) (int_size_in_bytes n2))

    let and_int_nat n1 n2 =
      atomic_step_cost
        (cost_N_IAnd_int_nat (int_size_in_bytes n1) (int_size_in_bytes n2))

    let xor_nat n1 n2 =
      atomic_step_cost
        (cost_N_IXor_nat (int_size_in_bytes n1) (int_size_in_bytes n2))

    let not_int i = atomic_step_cost (cost_N_INot_int (int_size_in_bytes i))

    let if_ = atomic_step_cost cost_N_IIf

    let loop = atomic_step_cost cost_N_ILoop

    let loop_left = atomic_step_cost cost_N_ILoop_left

    let dip = atomic_step_cost cost_N_IDip

    let view = atomic_step_cost cost_N_IView

    type algo = Ed25519 | Secp256k1 | P256 | Bls

    let algo_of_public_key (pkey : Signature.public_key) =
      match pkey with
      | Ed25519 _ -> Ed25519
      | Secp256k1 _ -> Secp256k1
      | P256 _ -> P256
      | Bls _ -> Bls

    let algo_of_public_key_hash (pkh : Signature.public_key_hash) =
      match pkh with
      | Ed25519 _ -> Ed25519
      | Secp256k1 _ -> Secp256k1
      | P256 _ -> P256
      | Bls _ -> Bls

    let check_signature_on_algo algo length =
      match algo with
      | Ed25519 -> cost_N_ICheck_signature_ed25519 length
      | Secp256k1 -> cost_N_ICheck_signature_secp256k1 length
      | P256 -> cost_N_ICheck_signature_p256 length
      | Bls -> cost_N_ICheck_signature_bls length

    let check_signature pkey b =
      check_signature_on_algo (algo_of_public_key pkey) (Bytes.length b)

    let blake2b b = atomic_step_cost (cost_N_IBlake2b (Bytes.length b))

    let sha256 b = atomic_step_cost (cost_N_ISha256 (Bytes.length b))

    let sha512 b = atomic_step_cost (cost_N_ISha512 (Bytes.length b))

    let dign n = atomic_step_cost (cost_N_IDig n)

    let dugn n = atomic_step_cost (cost_N_IDug n)

    let dipn n = atomic_step_cost (cost_N_IDipN n)

    let dropn n = atomic_step_cost (cost_N_IDropN n)

    let voting_power = atomic_step_cost cost_N_IVoting_power

    let total_voting_power = atomic_step_cost cost_N_ITotal_voting_power

    let keccak b = atomic_step_cost (cost_N_IKeccak (Bytes.length b))

    let sha3 b = atomic_step_cost (cost_N_ISha3 (Bytes.length b))

    let add_bls12_381_g1 = atomic_step_cost cost_N_IAdd_bls12_381_g1

    let add_bls12_381_g2 = atomic_step_cost cost_N_IAdd_bls12_381_g2

    let add_bls12_381_fr = atomic_step_cost cost_N_IAdd_bls12_381_fr

    let mul_bls12_381_g1 = atomic_step_cost cost_N_IMul_bls12_381_g1

    let mul_bls12_381_g2 = atomic_step_cost cost_N_IMul_bls12_381_g2

    let mul_bls12_381_fr = atomic_step_cost cost_N_IMul_bls12_381_fr

    let mul_bls12_381_fr_z z =
      atomic_step_cost (cost_N_IMul_bls12_381_fr_z (int_size_in_bytes z))

    let mul_bls12_381_z_fr z =
      atomic_step_cost (cost_N_IMul_bls12_381_z_fr (int_size_in_bytes z))

    let int_bls12_381_fr = atomic_step_cost cost_N_IInt_bls12_381_z_fr

    let neg_bls12_381_g1 = atomic_step_cost cost_N_INeg_bls12_381_g1

    let neg_bls12_381_g2 = atomic_step_cost cost_N_INeg_bls12_381_g2

    let neg_bls12_381_fr = atomic_step_cost cost_N_INeg_bls12_381_fr

    let neq = atomic_step_cost cost_N_INeq

    let pairing_check_bls12_381 (l : 'a Script_list.t) =
      atomic_step_cost (cost_N_IPairing_check_bls12_381 l.length)

    let comb n = atomic_step_cost (cost_N_IComb n)

    let uncomb n = atomic_step_cost (cost_N_IUncomb n)

    let comb_get n = atomic_step_cost (cost_N_IComb_get n)

    let comb_set n = atomic_step_cost (cost_N_IComb_set n)

    let dupn n = atomic_step_cost (cost_N_IDupN n)

    let sapling_verify_update ~inputs ~outputs ~bound_data =
      atomic_step_cost
        (cost_N_ISapling_verify_update_with_blake2b inputs outputs bound_data)

    let sapling_verify_update_deprecated ~inputs ~outputs =
      atomic_step_cost
        (cost_N_ISapling_verify_update_with_blake2b inputs outputs 0)

    let sapling_empty_state = atomic_step_cost cost_N_ISapling_empty_state

    let halt = atomic_step_cost cost_N_IHalt

    let push = atomic_step_cost cost_N_IPush

    let unit = atomic_step_cost cost_N_IUnit

    let empty_big_map = atomic_step_cost cost_N_IEmpty_big_map

    let lt = atomic_step_cost cost_N_ILt

    let le = atomic_step_cost cost_N_ILe

    let gt = atomic_step_cost cost_N_IGt

    let ge = atomic_step_cost cost_N_IGe

    let exec = atomic_step_cost cost_N_IExec

    let apply ~(rec_flag : bool) = atomic_step_cost (cost_N_IApply rec_flag)

    let lambda = atomic_step_cost cost_N_ILambda

    let address = atomic_step_cost cost_N_IAddress

    let contract = atomic_step_cost cost_N_IContract

    let transfer_tokens = atomic_step_cost cost_N_ITransfer_tokens

    let implicit_account = atomic_step_cost cost_N_IImplicit_account

    let is_implicit_account = atomic_step_cost cost_N_IIs_implicit_account

    let index_address = atomic_step_cost cost_N_IIndex_address

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/8039
       This gas cost is temporary and uses Big map's get, as their
       semantics and implementation is highly similar. *)
    let get_address_index = atomic_step_cost cost_N_IGet_address_index

    let create_contract = atomic_step_cost cost_N_ICreate_contract

    let set_delegate = atomic_step_cost cost_N_ISet_delegate

    let level = atomic_step_cost cost_N_ILevel

    let now = atomic_step_cost cost_N_INow

    let min_block_time = atomic_step_cost cost_N_IMin_block_time

    let source = atomic_step_cost cost_N_ISource

    let sender = atomic_step_cost cost_N_ISender

    let self = atomic_step_cost cost_N_ISelf

    let self_address = atomic_step_cost cost_N_ISelf_address

    let amount = atomic_step_cost cost_N_IAmount

    let balance = atomic_step_cost cost_N_IBalance

    let chain_id = atomic_step_cost cost_N_IChainId

    let ticket = atomic_step_cost cost_N_ITicket

    let read_ticket = atomic_step_cost cost_N_IRead_ticket

    let hash_key _ = atomic_step_cost cost_N_IHash_key

    let split_ticket amount_a amount_b =
      atomic_step_cost
        (cost_N_ISplit_ticket
           (int_size_in_bytes amount_a)
           (int_size_in_bytes amount_b))

    let open_chest ~chest ~time =
      let plaintext =
        Script_typed_ir.Script_timelock.get_plaintext_size chest
      in
      let log_time = Z.log2 Z.(add one time) in
      atomic_step_cost (cost_N_IOpen_chest log_time plaintext)

    (* --------------------------------------------------------------------- *)
    (* Semi-hand-crafted models *)

    let compare_unit = atomic_step_cost (S.safe_int 10)

    let compare_pair_tag = atomic_step_cost (S.safe_int 10)

    let compare_or_tag = atomic_step_cost (S.safe_int 10)

    let compare_option_tag = atomic_step_cost (S.safe_int 10)

    let compare_bool = atomic_step_cost (cost_N_ICompare 1 1)

    let compare_signature = atomic_step_cost (S.safe_int 92)

    let compare_string s1 s2 =
      atomic_step_cost
        (cost_N_ICompare (Script_string.length s1) (Script_string.length s2))

    let compare_bytes b1 b2 =
      atomic_step_cost (cost_N_ICompare (Bytes.length b1) (Bytes.length b2))

    let compare_mutez = atomic_step_cost (cost_N_ICompare 8 8)

    let compare_int i1 i2 =
      atomic_step_cost
        (cost_N_ICompare (int_size_in_bytes i1) (int_size_in_bytes i2))

    let compare_nat n1 n2 =
      atomic_step_cost
        (cost_N_ICompare (int_size_in_bytes n1) (int_size_in_bytes n2))

    let compare_key_hash =
      let sz = Signature.Public_key_hash.size in
      atomic_step_cost (cost_N_ICompare sz sz)

    let compare_key = atomic_step_cost (S.safe_int 92)

    let compare_timestamp t1 t2 =
      atomic_step_cost
        (cost_N_ICompare
           (z_bytes (Script_timestamp.to_zint t1))
           (z_bytes (Script_timestamp.to_zint t2)))

    (* Maximum size of an entrypoint in bytes *)
    let entrypoint_size = 31

    let compare_address =
      let sz = Signature.Public_key_hash.size + entrypoint_size in
      atomic_step_cost (cost_N_ICompare sz sz)

    let compare_chain_id = atomic_step_cost (S.safe_int 30)

    (* Defunctionalized CPS *)
    type cont =
      | Compare : 'a Script_typed_ir.comparable_ty * 'a * 'a * cont -> cont
      | Return : cont

    let compare : type a. a Script_typed_ir.comparable_ty -> a -> a -> cost =
     fun ty x y ->
      let rec compare : type a.
          a Script_typed_ir.comparable_ty -> a -> a -> cost -> cont -> cost =
       fun ty x y acc k ->
        match ty with
        | Unit_t -> (apply [@tailcall]) Gas.(acc +@ compare_unit) k
        | Never_t -> ( match x with _ -> .)
        | Bool_t -> (apply [@tailcall]) Gas.(acc +@ compare_bool) k
        | String_t -> (apply [@tailcall]) Gas.(acc +@ compare_string x y) k
        | Signature_t -> (apply [@tailcall]) Gas.(acc +@ compare_signature) k
        | Bytes_t -> (apply [@tailcall]) Gas.(acc +@ compare_bytes x y) k
        | Mutez_t -> (apply [@tailcall]) Gas.(acc +@ compare_mutez) k
        | Int_t -> (apply [@tailcall]) Gas.(acc +@ compare_int x y) k
        | Nat_t -> (apply [@tailcall]) Gas.(acc +@ compare_nat x y) k
        | Key_hash_t -> (apply [@tailcall]) Gas.(acc +@ compare_key_hash) k
        | Key_t -> (apply [@tailcall]) Gas.(acc +@ compare_key) k
        | Timestamp_t ->
            (apply [@tailcall]) Gas.(acc +@ compare_timestamp x y) k
        | Address_t -> (apply [@tailcall]) Gas.(acc +@ compare_address) k
        | Chain_id_t -> (apply [@tailcall]) Gas.(acc +@ compare_chain_id) k
        | Pair_t (tl, tr, _, YesYes) ->
            (* Reasonable over-approximation of the cost of lexicographic comparison. *)
            let xl, xr = x in
            let yl, yr = y in
            (compare [@tailcall])
              tl
              xl
              yl
              Gas.(acc +@ compare_pair_tag)
              (Compare (tr, xr, yr, k))
        | Or_t (tl, tr, _, YesYes) -> (
            match (x, y) with
            | L x, L y ->
                (compare [@tailcall]) tl x y Gas.(acc +@ compare_or_tag) k
            | L _, R _ -> (apply [@tailcall]) Gas.(acc +@ compare_or_tag) k
            | R _, L _ -> (apply [@tailcall]) Gas.(acc +@ compare_or_tag) k
            | R x, R y ->
                (compare [@tailcall]) tr x y Gas.(acc +@ compare_or_tag) k)
        | Option_t (t, _, Yes) -> (
            match (x, y) with
            | None, None ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | None, Some _ ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | Some _, None ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | Some x, Some y ->
                (compare [@tailcall]) t x y Gas.(acc +@ compare_option_tag) k)
      and apply cost k =
        match k with
        | Compare (ty, x, y, k) -> (compare [@tailcall]) ty x y cost k
        | Return -> cost
      in
      compare ty x y Gas.free Return

    let set_mem (type a) (elt : a) (set : a Script_typed_ir.set) =
      let (module Box) = Script_set.get set in
      let per_elt_cost = Box.OPS.elt_size elt |> Size.to_int in
      Michelson_v1_gas_costs.cost_N_ISet_mem per_elt_cost Box.size

    let set_update (type a) (elt : a) (set : a Script_typed_ir.set) =
      let (module Box) = Script_set.get set in
      let per_elt_cost = Box.OPS.elt_size elt |> Size.to_int in
      Michelson_v1_gas_costs.cost_N_ISet_update per_elt_cost Box.size

    let map_mem (type k v) (elt : k) (map : (k, v) Script_typed_ir.map) =
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt in
      Michelson_v1_gas_costs.cost_N_IMap_mem per_elt_cost Box.size

    let map_get = map_mem

    let map_update (type k v) (elt : k) (map : (k, v) Script_typed_ir.map) =
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt in
      Michelson_v1_gas_costs.cost_N_IMap_update per_elt_cost Box.size

    let map_get_and_update (type k v) (elt : k)
        (map : (k, v) Script_typed_ir.map) =
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt in
      Michelson_v1_gas_costs.cost_N_IMap_get_and_update per_elt_cost Box.size

    let view_get (elt : Script_string.t) (m : Script_typed_ir.view_map) =
      map_get elt m

    let view_update (elt : Script_string.t) (m : Script_typed_ir.view_map) =
      map_update elt m

    let join_tickets :
        'a Script_typed_ir.comparable_ty ->
        'a Script_typed_ir.ticket ->
        'a Script_typed_ir.ticket ->
        Gas.cost =
     fun ty ticket_a ticket_b ->
      let contents_comparison =
        compare ty ticket_a.contents ticket_b.contents
      in
      Gas.(
        contents_comparison +@ compare_address
        +@ add_nat
             (ticket_a.amount :> Script_int.n Script_int.num)
             (ticket_b.amount :> Script_int.n Script_int.num))

    let emit = atomic_step_cost cost_N_IEmit

    (* Continuations *)
    module Control = struct
      let nil = atomic_step_cost cost_N_KNil

      let cons = atomic_step_cost cost_N_KCons

      let return = atomic_step_cost cost_N_KReturn

      let view_exit = atomic_step_cost cost_N_KView_exit

      let map_head = atomic_step_cost cost_N_KMap_head

      let undip = atomic_step_cost cost_N_KUndip

      let loop_in = atomic_step_cost cost_N_KLoop_in

      let loop_in_left = atomic_step_cost cost_N_KLoop_in_left

      let iter = atomic_step_cost cost_N_KIter

      let list_enter_body xs ys_len =
        atomic_step_cost (cost_N_KList_enter_body xs ys_len)

      let list_exit_body = atomic_step_cost cost_N_KList_exit_body

      let map_enter_body (type k v) (map : (k, v) Script_typed_ir.map) =
        let (module Box) = Script_map.get_module map in
        atomic_step_cost (cost_N_KMap_enter_body Box.size)

      let map_exit_body (type k v) (key : k) (map : (k, v) Script_typed_ir.map)
          =
        map_update key map
    end

    let concat_string_precheck (l : 'a Script_list.t) =
      atomic_step_cost (cost_N_IConcat_string_precheck l.length)

    let concat_string total_bytes =
      atomic_step_cost (cost_N_IConcat_string total_bytes)

    let concat_bytes total_bytes =
      atomic_step_cost (cost_N_IConcat_bytes total_bytes)

    let unpack bytes =
      let blen = Bytes.length bytes in
      atomic_step_cost (cost_N_IUnpack blen)

    (* TODO benchmark *)
    (* FIXME: imported from 006, needs proper benchmarks *)
    let unpack_failed bytes =
      (* We cannot instrument failed deserialization,
         so we take worst case fees: a set of size 1 bytes values. *)
      let blen = String.length bytes in
      let len = S.safe_int blen in
      let d = Z.numbits (Z.of_int blen) in
      (len *@ alloc_mbytes_cost 1)
      +@ len
         *@ (S.safe_int d *@ (alloc_cost (S.safe_int 3) +@ step_cost S.one))
  end

  module Typechecking = struct
    open Michelson_v1_gas_costs

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_secp256k1
                (max cost_DECODING_PUBLIC_KEY_p256 cost_DECODING_PUBLIC_KEY_bls)))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1
                (max
                   cost_B58CHECK_DECODING_PUBLIC_KEY_p256
                   cost_B58CHECK_DECODING_PUBLIC_KEY_bls)))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_HASH_secp256k1
                (max
                   cost_DECODING_PUBLIC_KEY_HASH_p256
                   cost_DECODING_PUBLIC_KEY_HASH_bls)))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1
                (max
                   cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256
                   cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_bls)))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_SIGNATURE_ed25519
             (max
                cost_DECODING_SIGNATURE_secp256k1
                (max cost_DECODING_SIGNATURE_p256 cost_DECODING_SIGNATURE_bls)))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_DECODING_SIGNATURE_secp256k1
                (max
                   cost_B58CHECK_DECODING_SIGNATURE_p256
                   cost_B58CHECK_DECODING_SIGNATURE_bls)))

    let chain_id_optimized = atomic_step_cost cost_DECODING_CHAIN_ID

    let chain_id_readable = atomic_step_cost cost_B58CHECK_DECODING_CHAIN_ID

    (* Reasonable approximation *)
    let address_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_readable = key_hash_readable

    let bls12_381_g1 = atomic_step_cost cost_DECODING_BLS_G1

    let bls12_381_g2 = atomic_step_cost cost_DECODING_BLS_G2

    let bls12_381_fr = atomic_step_cost cost_DECODING_BLS_FR

    let check_printable s =
      atomic_step_cost (cost_CHECK_PRINTABLE (String.length s))

    let ty_eq ty1 ty2 =
      (* Assumes O(1) access to the size of a type *)
      let size1 = Script_typed_ir.(ty_size ty1 |> Type_size.to_int) in
      let size2 = Script_typed_ir.(ty_size ty2 |> Type_size.to_int) in
      atomic_step_cost (cost_TY_EQ (Saturation_repr.min size1 size2))

    (* The gas cost for comparing a type with a type of size 1 *)
    let ty_eq_prim = atomic_step_cost (cost_TY_EQ (Saturation_repr.safe_int 1))

    let parse_type_cycle = atomic_step_cost cost_PARSE_TYPE1

    let parse_instr_cycle = atomic_step_cost cost_TYPECHECKING_CODE

    let parse_data_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    (* Cost of a cycle of checking that a type is dupable *)
    (* TODO: bench *)
    let check_dupable_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    let find_entrypoint_cycle = atomic_step_cost cost_FIND_ENTRYPOINT

    let bool = free

    let unit = free

    let timestamp_readable s =
      atomic_step_cost (cost_TIMESTAMP_READABLE_DECODING (String.length s))

    (* Balance stored at /contracts/index/hash/balance, on 64 bits *)
    let contract_exists =
      Gas.cost_of_repr @@ Storage_costs.read_access ~path_length:4 ~read_bytes:8

    (* Constructing proof arguments consists in a decreasing loop in the result
       monad, allocating at each step. We charge a reasonable overapproximation. *)
    let proof_argument n =
      atomic_step_cost (S.mul (S.safe_int n) (S.safe_int 50))

    let chest_key = atomic_step_cost cost_DECODING_Chest_key

    let chest ~bytes = atomic_step_cost (cost_DECODING_Chest bytes)
  end

  module Unparsing = struct
    open Michelson_v1_gas_costs

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_secp256k1
                (max cost_ENCODING_PUBLIC_KEY_p256 cost_ENCODING_PUBLIC_KEY_bls)))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1
                (max
                   cost_B58CHECK_ENCODING_PUBLIC_KEY_p256
                   cost_B58CHECK_ENCODING_PUBLIC_KEY_bls)))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_HASH_secp256k1
                (max
                   cost_ENCODING_PUBLIC_KEY_HASH_p256
                   cost_ENCODING_PUBLIC_KEY_HASH_bls)))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1
                (max
                   cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256
                   cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_bls)))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_SIGNATURE_ed25519
             (max
                cost_ENCODING_SIGNATURE_secp256k1
                (max cost_ENCODING_SIGNATURE_p256 cost_ENCODING_SIGNATURE_bls)))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_ENCODING_SIGNATURE_secp256k1
                (max
                   cost_B58CHECK_ENCODING_SIGNATURE_p256
                   cost_B58CHECK_ENCODING_SIGNATURE_bls)))

    let chain_id_optimized = atomic_step_cost cost_ENCODING_CHAIN_ID

    let chain_id_readable = atomic_step_cost cost_B58CHECK_ENCODING_CHAIN_ID

    let timestamp_readable = atomic_step_cost cost_TIMESTAMP_READABLE_ENCODING

    (* Reasonable approximation *)
    let address_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_optimized = key_hash_optimized

    (* Reasonable approximation *)
    let contract_readable = key_hash_readable

    let bls12_381_g1 = atomic_step_cost cost_ENCODING_BLS_G1

    let bls12_381_g2 = atomic_step_cost cost_ENCODING_BLS_G2

    let bls12_381_fr = atomic_step_cost cost_ENCODING_BLS_FR

    let unparse_type ty =
      atomic_step_cost @@ cost_UNPARSE_TYPE
      @@ Script_typed_ir.(Type_size.to_int @@ ty_size ty)

    let unparse_instr_cycle = atomic_step_cost cost_UNPARSING_CODE

    let unparse_data_cycle = atomic_step_cost cost_UNPARSING_DATA

    let unit = Gas.free

    (* Reuse 006 costs. *)
    let operation bytes = Script.bytes_node_cost bytes

    let sapling_transaction (t : Sapling.transaction) =
      let inputs = Size.sapling_transaction_inputs t in
      let outputs = Size.sapling_transaction_outputs t in
      let bound_data = Size.sapling_transaction_bound_data t in
      atomic_step_cost
        (cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs ~bound_data)

    let sapling_transaction_deprecated (t : Sapling.Legacy.transaction) =
      let inputs = List.length t.inputs in
      let outputs = List.length t.outputs in
      atomic_step_cost
        (cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs ~bound_data:0)

    let sapling_diff (d : Sapling.diff) =
      let nfs = List.length d.nullifiers in
      let cms = List.length d.commitments_and_ciphertexts in
      atomic_step_cost (cost_SAPLING_DIFF_ENCODING ~nfs ~cms)

    let chest_key = atomic_step_cost cost_ENCODING_Chest_key

    let chest ~plaintext_size =
      atomic_step_cost (cost_ENCODING_Chest plaintext_size)
  end
end

module Internal_for_tests = struct
  let int_cost_of_manager_operation = Cost_of.manager_operation_int
end
