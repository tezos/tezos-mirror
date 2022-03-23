(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Gas
module S = Saturation_repr
module Size = Gas_input_size

module Cost_of = struct
  module S_syntax = struct
    (* This is a good enough approximation. S.numbits 0 = 0 *)
    let log2 x = S.safe_int (1 + S.numbits x)

    let ( + ) = S.add

    let ( * ) = S.mul

    let ( lsr ) = S.shift_right
  end

  let z_bytes (z : Z.t) =
    let bits = Z.numbits z in
    (7 + bits) / 8

  let int_bytes (z : 'a Script_int.num) = z_bytes (Script_int.to_zint z)

  let manager_operation = step_cost @@ S.safe_int 1_000

  module Generated_costs = struct
    (* Automatically generated costs functions. *)

    (* model N_IAbs_int *)
    (* Approximating 0.065045 x term *)
    let cost_N_IAbs_int size = S.safe_int (25 + (size lsr 4))

    (* model N_IAdd_bls12_381_fr *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IAdd_bls12_381_fr = S.safe_int 45

    (* model N_IAdd_bls12_381_g1 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IAdd_bls12_381_g1 = S.safe_int 925

    (* model N_IAdd_bls12_381_g2 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IAdd_bls12_381_g2 = S.safe_int 2_520

    let cost_linear_op_int size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int (Compare.Int.max size1 size2) in
      S.safe_int 55 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_IAdd_int *)
    (* Approximating 0.078154 x term *)
    let cost_N_IAdd_int = cost_linear_op_int

    (* model N_IAdd_nat *)
    (* Approximating 0.077807 x term *)
    let cost_N_IAdd_nat = cost_linear_op_int

    (* model N_IAdd_seconds_to_timestamp *)
    (* Approximating 0.078056 x term *)
    let cost_N_IAdd_seconds_to_timestamp = cost_linear_op_int

    (* model N_IAdd_tez *)
    let cost_N_IAdd_tez = S.safe_int 20

    (* model N_IAdd_timestamp_to_seconds *)
    (* Approximating 0.077771 x term *)
    let cost_N_IAdd_timestamp_to_seconds = cost_linear_op_int

    (* model N_IAddress *)
    let cost_N_IAddress = S.safe_int 10

    (* model N_IAmount *)
    let cost_N_IAmount = S.safe_int 15

    (* model N_IAnd *)
    let cost_N_IAnd = S.safe_int 20

    (* model N_IAnd_int_nat *)
    (* Approximating 0.076804 x 2 x term *)
    let cost_N_IAnd_int_nat size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int (Compare.Int.min size1 size2) in
      S.safe_int 50 + ((v0 lsr 3) + (v0 lsr 6))

    (* model N_IAnd_nat *)
    (* Approximating 0.076804 x term *)
    (* The difference with `cost_N_IAnd_int_nat` comes from Zarith, where the
       complexity of `Z.logand` depends on the sign of the argument. *)
    let cost_N_IAnd_nat size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int (Compare.Int.min size1 size2) in
      S.safe_int 50 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_IApply *)
    let cost_N_IApply = S.safe_int 160

    (* model N_IBlake2b *)
    (* Approximating 1.120804 x term *)
    let cost_N_IBlake2b size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 430 + v0 + (v0 lsr 3)

    (* model N_IBytes_size *)
    let cost_N_IBytes_size = S.safe_int 15

    (* model N_ICar *)
    let cost_N_ICar = S.safe_int 10

    (* model N_ICdr *)
    let cost_N_ICdr = S.safe_int 10

    (* model N_IChainId *)
    let cost_N_IChainId = S.safe_int 15

    (* model N_ICheck_signature_ed25519 *)
    (* Approximating 1.123507 x term *)
    let cost_N_ICheck_signature_ed25519 size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 65_800 + (v0 + (v0 lsr 3))

    (* model N_ICheck_signature_p256 *)
    (* Approximating 1.111539 x term *)
    let cost_N_ICheck_signature_p256 size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 990_000 + (v0 + (v0 lsr 3))

    (* model N_ICheck_signature_secp256k1 *)
    (* Approximating 1.125404 x term *)
    let cost_N_ICheck_signature_secp256k1 size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 51_600 + (v0 + (v0 lsr 3))

    (* model N_IComb *)
    (* Approximating 3.531001 x term *)
    (* Note: size >= 2, so the cost is never 0 *)
    let cost_N_IComb size =
      let open S_syntax in
      let v0 = S.safe_int size in
      (S.safe_int 3 * v0) + (v0 lsr 1) + (v0 lsr 5)

    (* model N_IComb_get *)
    (* Approximating 0.573180 x term *)
    let cost_N_IComb_get size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 30 + (v0 lsr 1) + (v0 lsr 4)

    (* model N_IComb_set *)
    (* Approximating 1.287531 x term *)
    let cost_N_IComb_set size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 40 + (v0 + (v0 lsr 2) + (v0 lsr 5))

    (* Model N_ICompare *)
    (* Approximating 0.024413 x term *)
    let cost_N_ICompare size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int (Compare.Int.min size1 size2) in
      S.safe_int 35 + ((v0 lsr 6) + (v0 lsr 7))

    (* model N_IConcat_bytes_pair *)
    (* Approximating 0.065017 x term *)
    let cost_N_IConcat_bytes_pair size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int size1 + S.safe_int size2 in
      S.safe_int 65 + (v0 lsr 4)

    (* model N_IConcat_string_pair *)
    (* Approximating 0.061402 x term *)
    let cost_N_IConcat_string_pair size1 size2 =
      let open S_syntax in
      let v0 = S.safe_int size1 + S.safe_int size2 in
      S.safe_int 65 + (v0 lsr 4)

    (* model N_ICons_list *)
    let cost_N_ICons_list = S.safe_int 15

    (* model N_ICons_none *)
    let cost_N_ICons_none = S.safe_int 15

    (* model N_ICons_pair *)
    let cost_N_ICons_pair = S.safe_int 15

    (* model N_ICons_some *)
    let cost_N_ICons_some = S.safe_int 15

    (* model N_IConst *)
    let cost_N_IConst = S.safe_int 10

    (* model N_IContract *)
    let cost_N_IContract = S.safe_int 30

    (* model N_ICreate_contract *)
    let cost_N_ICreate_contract = S.safe_int 30

    (* model N_IDiff_timestamps *)
    (* Approximating 0.077922 x term *)
    let cost_N_IDiff_timestamps = cost_linear_op_int

    (* model N_IDig *)
    (* Approximating 6.750442 x term *)
    let cost_N_IDig size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 60 + ((S.safe_int 6 * v0) + (v0 lsr 1) + (v0 lsr 2))

    (* model N_IDip *)
    let cost_N_IDip = S.safe_int 15

    (* model N_IDipN *)
    (* Approximating 1.708122 x term *)
    let cost_N_IDipN size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 45 + (v0 + (v0 lsr 1) + (v0 lsr 3))

    (* model N_IView *)
    let cost_N_IView = S.safe_int 1460

    (* model N_IDrop *)
    let cost_N_IDrop = S.safe_int 10

    (* model N_IDropN *)
    (* Approximating 2.713108 x term *)
    let cost_N_IDropN size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 60 + (S.safe_int 2 * v0) + (v0 lsr 1) + (v0 lsr 3)

    (* model N_IDug *)
    (* Approximating 6.718396 x term *)
    let cost_N_IDug size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 60 + ((S.safe_int 6 * v0) + (v0 lsr 1) + (v0 lsr 2))

    (* model N_IDup *)
    let cost_N_IDup = S.safe_int 10

    (* model N_IDupN *)
    (* Approximating 1.129785 x term *)
    let cost_N_IDupN size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 20 + v0 + (v0 lsr 3)

    let cost_div_int size1 size2 =
      let q = size1 - size2 in
      if Compare.Int.(q < 0) then S.safe_int 140
      else
        let open S_syntax in
        let v0 = S.safe_int q * S.safe_int size2 in
        S.safe_int 140 + (v0 lsr 10) + (v0 lsr 11) + (v0 lsr 13)

    (* model N_IEdiv_int *)
    (* Approximating 0.001591 x term *)
    let cost_N_IEdiv_int = cost_div_int

    (* model N_IEdiv_nat *)
    (* Approximating 0.001605 x term *)
    let cost_N_IEdiv_nat = cost_div_int

    (* model N_IEdiv_tez *)
    let cost_N_IEdiv_tez = S.safe_int 140

    (* model N_IEdiv_teznat *)
    let cost_N_IEdiv_teznat = S.safe_int 140

    (* model N_IEmpty_big_map *)
    let cost_N_IEmpty_big_map = S.safe_int 15

    (* model N_IEmpty_map *)
    let cost_N_IEmpty_map = S.safe_int 220

    (* model N_IEmpty_set *)
    let cost_N_IEmpty_set = S.safe_int 220

    (* model N_IEq *)
    let cost_N_IEq = S.safe_int 15

    (* model N_IExec *)
    let cost_N_IExec = S.safe_int 15

    (* model N_IFailwith *)
    (* let cost_N_IFailwith = S.safe_int 105 *)

    (* model N_IGe *)
    let cost_N_IGe = S.safe_int 15

    (* model N_IGt *)
    let cost_N_IGt = S.safe_int 15

    (* model N_IHalt *)
    let cost_N_IHalt = S.safe_int 15

    (* model N_IHash_key *)
    let cost_N_IHash_key = S.safe_int 655

    (* model N_IIf *)
    let cost_N_IIf = S.safe_int 10

    (* model N_IIf_cons *)
    let cost_N_IIf_cons = S.safe_int 10

    (* model N_IIf_left *)
    let cost_N_IIf_left = S.safe_int 10

    (* model N_IIf_none *)
    let cost_N_IIf_none = S.safe_int 10

    (* model N_IOpt_map *)
    let cost_opt_map = S.safe_int 15

    (* model N_IImplicit_account *)
    let cost_N_IImplicit_account = S.safe_int 10

    (* model N_IInt_bls12_381_z_fr *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IInt_bls12_381_z_fr = S.safe_int 125

    (* model N_IInt_nat *)
    let cost_N_IInt_nat = S.safe_int 15

    (* model N_IIs_nat *)
    let cost_N_IIs_nat = S.safe_int 15

    (* model N_IKeccak *)
    (* Approximating 8.276352 x term *)
    let cost_N_IKeccak size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 1350 + ((S.safe_int 8 * v0) + (v0 lsr 2))

    (* model N_ILambda *)
    let cost_N_ILambda = S.safe_int 10

    (* model N_ILe *)
    let cost_N_ILe = S.safe_int 15

    (* model N_ILeft *)
    let cost_N_ILeft = S.safe_int 15

    (* model N_ILevel *)
    let cost_N_ILevel = S.safe_int 15

    (* model N_IList_iter *)
    let cost_N_IList_iter _ = S.safe_int 25

    (* model N_IList_map *)
    let cost_N_IList_map _ = S.safe_int 25

    (* model N_IList_size *)
    let cost_N_IList_size = S.safe_int 15

    (* model N_ILoop *)
    let cost_N_ILoop = S.safe_int 10

    (* model N_ILoop_left *)
    let cost_N_ILoop_left = S.safe_int 10

    (* model N_ILsl_nat *)
    (* Approximating 0.115642 x term *)
    let cost_N_ILsl_nat size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 60 + ((v0 lsr 4) + (v0 lsr 5) + (v0 lsr 6))

    (* model N_ILsr_nat *)
    (* Approximating 0.115565 x term *)
    let cost_N_ILsr_nat size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 60 + ((v0 lsr 4) + (v0 lsr 5) + (v0 lsr 6))

    (* model N_ILt *)
    let cost_N_ILt = S.safe_int 15

    (* model N_IMap_get *)
    (* Approximating 0.048359 x term *)
    let cost_N_IMap_get size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 110 + (v0 lsr 5) + (v0 lsr 6)

    (* model N_IMap_get_and_update *)
    (* Approximating 0.145661 x term *)
    let cost_N_IMap_get_and_update size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 135 + (v0 lsr 3) + (v0 lsr 6)

    (* model N_IMap_iter *)
    (* Approximating 7.621331 x term *)
    let cost_N_IMap_iter size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 70 + (S.safe_int 7 * v0) + (v0 lsr 1) + (v0 lsr 3)

    (* model N_IMap_map *)
    (* Approximating 7.46280485884 x term *)
    let cost_N_IMap_map size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 265 + ((S.safe_int 7 * v0) + (v0 lsr 1))

    (* model N_IMap_mem *)
    (* Approximating 0.048446 x term *)
    let cost_N_IMap_mem size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 110 + (v0 lsr 5) + (v0 lsr 6)

    (* model N_IMap_size *)
    let cost_N_IMap_size = S.safe_int 15

    (* model N_IMap_update *)
    (* Approximating 0.097072 x term *)
    let cost_N_IMap_update size1 size2 =
      let open S_syntax in
      let v0 = size1 * log2 size2 in
      S.safe_int 130 + (v0 lsr 4) + (v0 lsr 5)

    (* model N_IMul_bls12_381_fr *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IMul_bls12_381_fr = S.safe_int 65

    (* model N_IMul_bls12_381_fr_z *)
    (* Approximating 1.059386 x term *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IMul_bls12_381_fr_z size1 =
      let open S_syntax in
      let v0 = S.safe_int size1 in
      S.safe_int 330 + v0 + (v0 lsr 4)

    (* model N_IMul_bls12_381_g1 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IMul_bls12_381_g1 = S.safe_int 103_000

    (* model N_IMul_bls12_381_g2 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IMul_bls12_381_g2 = S.safe_int 220_000

    (* model N_IMul_bls12_381_z_fr *)
    (* Approximating 1.068674 x term *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IMul_bls12_381_z_fr size1 =
      let open S_syntax in
      let v0 = S.safe_int size1 in
      S.safe_int 330 + v0 + (v0 lsr 4)

    let cost_mul size1 size2 =
      let open S_syntax in
      let a = S.add (S.safe_int size1) (S.safe_int size2) in
      let v0 = a * log2 a in
      S.safe_int 100 + (v0 lsr 1) + (v0 lsr 2) + (v0 lsr 4)

    (* model N_IMul_int *)
    (* Approximating 0.857931 x term *)
    let cost_N_IMul_int = cost_mul

    (* model N_IMul_nat *)
    (* Approximating 0.861823 x term *)
    let cost_N_IMul_nat = cost_mul

    (* model N_IMul_nattez *)
    let cost_N_IMul_nattez = S.safe_int 50

    (* model N_IMul_teznat *)
    let cost_N_IMul_teznat = S.safe_int 50

    (* model N_INeg_bls12_381_fr *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_INeg_bls12_381_fr = S.safe_int 45

    (* model N_INeg_bls12_381_g1 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_INeg_bls12_381_g1 = S.safe_int 60

    (* model N_INeg_bls12_381_g2 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_INeg_bls12_381_g2 = S.safe_int 85

    (* model N_INeg *)
    (* Approximating 0.066076 x term *)
    let cost_N_INeg size =
      let open S_syntax in
      S.safe_int 40 + (S.safe_int size lsr 4)

    (* model N_INeq *)
    let cost_N_INeq = S.safe_int 15

    (* model N_INil *)
    let cost_N_INil = S.safe_int 15

    (* model N_INot *)
    let cost_N_INot = S.safe_int 10

    (* model N_INot_int *)
    (* Approximating 0.075541 x term *)
    let cost_N_INot_int size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 50 + ((v0 lsr 4) + (v0 lsr 7))

    (* model N_INow *)
    let cost_N_INow = S.safe_int 15

    (* model N_IMin_block_time *)
    let cost_N_IMin_block_time =
      (* TODO: #2504
         Benchmark MIN_BLOCK_TIME instruction to get an accurate Gas cost.
      *)
      S.safe_int 30

    (* model N_IOpen_chest *)
    (* 612000 + chest * 19 + time * 19050 *)
    let cost_N_IOpen_chest ~chest ~time =
      let open S_syntax in
      let v0 = S.safe_int chest in
      let v1 = S.safe_int time in
      S.safe_int 612_000 + (S.safe_int 19 * v0) + (S.safe_int 19050 * v1)

    (* model N_IOr *)
    let cost_N_IOr = S.safe_int 15

    (* model N_IOr_nat *)
    (* Approximating 0.075758 x term *)
    let cost_N_IOr_nat = cost_linear_op_int

    (* model N_IPairing_check_bls12_381 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_N_IPairing_check_bls12_381 size =
      S.add (S.safe_int 450_000) (S.mul (S.safe_int 342_500) (S.safe_int size))

    (* model N_IRead_ticket *)
    let cost_N_IRead_ticket = S.safe_int 15

    (* model N_IRight *)
    let cost_N_IRight = S.safe_int 15

    (* model N_ISapling_empty_state *)
    let cost_N_ISapling_empty_state = S.safe_int 15

    (* model N_ISapling_verify_update *)
    (* Approximating 1.27167 x term *)
    (* Approximating 38.72115 x term *)
    let cost_N_ISapling_verify_update size1 size2 bound_data =
      let open S_syntax in
      let v1 = S.safe_int size1 in
      let v0 = S.safe_int size2 in
      let bd = S.safe_int bound_data in
      S.safe_int 84_050 + (v1 + (v1 lsr 2)) + (S.safe_int 39 * v0) + (bd lsr 2)

    (* model N_ISelf_address *)
    let cost_N_ISelf_address = S.safe_int 15

    (* model N_ISelf *)
    let cost_N_ISelf = S.safe_int 15

    (* model N_ISender *)
    let cost_N_ISender = S.safe_int 15

    (* model N_ISet_delegate *)
    let cost_N_ISet_delegate = S.safe_int 40

    (* model N_ISet_iter *)
    (* Approximating 7.633555 x term *)
    let cost_N_ISet_iter size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 70 + (S.safe_int 7 * v0) + (v0 lsr 1) + (v0 lsr 3)

    (* model N_ISet_size *)
    let cost_N_ISet_size = S.safe_int 15

    (* model N_ISha256 *)
    (* Approximating 4.763264 x term *)
    let cost_N_ISha256 size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 600 + ((S.safe_int 4 * v0) + (v0 lsr 1) + (v0 lsr 2))

    (* model N_ISha3 *)
    (* Approximating 8.362339 x term *)
    let cost_N_ISha3 = cost_N_IKeccak

    (* model N_ISha512 *)
    (* Approximating 3.074641 x term *)
    let cost_N_ISha512 size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 680 + (S.safe_int 3 * v0)

    (* model N_ISlice_bytes *)
    (* Approximating 0.065752 x term *)
    let cost_N_ISlice_bytes size =
      let open S_syntax in
      S.safe_int 40 + (S.safe_int size lsr 4)

    (* model N_ISlice_string *)
    (* Approximating 0.065688 x term *)
    let cost_N_ISlice_string size =
      let open S_syntax in
      S.safe_int 40 + (S.safe_int size lsr 4)

    (* model N_ISource *)
    let cost_N_ISource = S.safe_int 15

    (* model N_ISplit_ticket *)
    (* Approximating 0.132362 x term *)
    let cost_N_ISplit_ticket size1 size2 =
      let open S_syntax in
      let v1 = S.safe_int (Compare.Int.max size1 size2) in
      S.safe_int 55 + (v1 lsr 3)

    (* model N_IString_size *)
    let cost_N_IString_size = S.safe_int 15

    (* model N_ISub_int *)
    (* Approximating 0.077849 x term *)
    let cost_N_ISub_int = cost_linear_op_int

    (* model N_ISub_tez *)
    let cost_N_ISub_tez = S.safe_int 20

    (* model N_ISub_tez_legacy *)
    let cost_N_ISub_tez_legacy = S.safe_int 20

    (* model N_ISub_timestamp_seconds *)
    (* Approximating 0.077794 x term *)
    let cost_N_ISub_timestamp_seconds = cost_linear_op_int

    (* model N_ISwap *)
    let cost_N_ISwap = S.safe_int 10

    (* model N_ITicket *)
    let cost_N_ITicket = S.safe_int 15

    (* model N_ITotal_voting_power *)
    let cost_N_ITotal_voting_power = S.safe_int 370

    (* model N_ITransfer_tokens *)
    let cost_N_ITransfer_tokens = S.safe_int 30

    (* model N_IUncomb *)
    (* Approximating 3.944710 x term *)
    let cost_N_IUncomb size =
      let open S_syntax in
      let v0 = S.safe_int size in
      S.safe_int 25 + (S.safe_int 4 * v0)

    (* model N_IUnpair *)
    let cost_N_IUnpair = S.safe_int 10

    (* model N_IVoting_power *)
    let cost_N_IVoting_power = S.safe_int 530

    (* model N_IXor *)
    let cost_N_IXor = S.safe_int 20

    (* model N_IXor_nat *)
    (* Approximating 0.075601 x term *)
    let cost_N_IXor_nat = cost_linear_op_int

    (* model N_KCons *)
    let cost_N_KCons = S.safe_int 15

    (* model N_KIter *)
    let cost_N_KIter = S.safe_int 20

    (* model N_KList_enter_body *)
    (* Approximating 1.672196 x term *)
    let cost_N_KList_enter_body xs size_ys =
      match xs with
      | [] ->
          let open S_syntax in
          let v0 = S.safe_int size_ys in
          S.safe_int 40 + (v0 + (v0 lsr 1) + (v0 lsr 3))
      | _ :: _ -> S.safe_int 70

    (* model N_KList_exit_body *)
    let cost_N_KList_exit_body = S.safe_int 30

    (* model N_KLoop_in *)
    let cost_N_KLoop_in = S.safe_int 15

    (* model N_KLoop_in_left *)
    let cost_N_KLoop_in_left = S.safe_int 15

    (* model N_KMap_enter_body *)
    let cost_N_KMap_enter_body = S.safe_int 165

    (* model N_KNil *)
    let cost_N_KNil = S.safe_int 20

    (* model N_KReturn *)
    let cost_N_KReturn = S.safe_int 15

    (* model N_KView_exit *)
    let cost_N_KView_exit = S.safe_int 20

    (* model N_KMap_head *)
    let const_N_KMap_head = S.safe_int 20

    (* model N_KUndip *)
    let cost_N_KUndip = S.safe_int 15

    (* model DECODING_BLS_FR *)
    (* when benchmarking, compile bls12-381 without ADX, see
       https://gitlab.com/dannywillems/ocaml-bls12-381/-/blob/71d0b4d467fbfaa6452d702fcc408d7a70916a80/README.md#install
    *)
    let cost_DECODING_BLS_FR = S.safe_int 150

    (* model DECODING_BLS_G1 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_DECODING_BLS_G1 = S.safe_int 65_300

    (* model DECODING_BLS_G2 *)
    (* when benchmarking, compile bls12-381 without ADX *)
    let cost_DECODING_BLS_G2 = S.safe_int 73_300

    (* model B58CHECK_DECODING_CHAIN_ID *)
    let cost_B58CHECK_DECODING_CHAIN_ID = S.safe_int 1_600

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 3_300

    (* model B58CHECK_DECODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 4_200

    (* model B58CHECK_DECODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_p256 = S.safe_int 325_000

    (* model B58CHECK_DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 9_000

    (* model B58CHECK_DECODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_DECODING_SIGNATURE_ed25519 = S.safe_int 6_400

    (* model B58CHECK_DECODING_SIGNATURE_p256 *)
    let cost_B58CHECK_DECODING_SIGNATURE_p256 = S.safe_int 6_400

    (* model B58CHECK_DECODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_DECODING_SIGNATURE_secp256k1 = S.safe_int 6_400

    (* model ENCODING_BLS_FR *)
    let cost_ENCODING_BLS_FR = S.safe_int 80

    (* model ENCODING_BLS_G1 *)
    let cost_ENCODING_BLS_G1 = S.safe_int 3200

    (* model ENCODING_BLS_G2 *)
    let cost_ENCODING_BLS_G2 = S.safe_int 3900

    (* model B58CHECK_ENCODING_CHAIN_ID *)
    let cost_B58CHECK_ENCODING_CHAIN_ID = S.safe_int 1_800

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 3_200

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256 = S.safe_int 3_200

    (* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 3_200

    (* model B58CHECK_ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 4_500

    (* model B58CHECK_ENCODING_PUBLIC_KEY_p256 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_p256 = S.safe_int 4_550

    (* model B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 4_950

    (* model B58CHECK_ENCODING_SIGNATURE_ed25519 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_ed25519 = S.safe_int 8_300

    (* model B58CHECK_ENCODING_SIGNATURE_p256 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_p256 = S.safe_int 8_300

    (* model B58CHECK_ENCODING_SIGNATURE_secp256k1 *)
    let cost_B58CHECK_ENCODING_SIGNATURE_secp256k1 = S.safe_int 8_300

    (* model DECODING_CHAIN_ID *)
    let cost_DECODING_CHAIN_ID = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_HASH_p256 *)
    let cost_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 50

    (* model DECODING_PUBLIC_KEY_ed25519 *)
    let cost_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 60

    (* model DECODING_PUBLIC_KEY_p256 *)
    let cost_DECODING_PUBLIC_KEY_p256 = S.safe_int 320_000

    (* model DECODING_PUBLIC_KEY_secp256k1 *)
    let cost_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 4_900

    (* model DECODING_SIGNATURE_ed25519 *)
    let cost_DECODING_SIGNATURE_ed25519 = S.safe_int 35

    (* model DECODING_SIGNATURE_p256 *)
    let cost_DECODING_SIGNATURE_p256 = S.safe_int 35

    (* model DECODING_SIGNATURE_secp256k1 *)
    let cost_DECODING_SIGNATURE_secp256k1 = S.safe_int 35

    (* model DECODING_Chest_key *)
    let cost_DECODING_Chest_key = S.safe_int 5900

    (* model DECODING_Chest *)
    (* Approximating 0.039349 x term *)
    let cost_DECODING_Chest ~bytes =
      let open S_syntax in
      let v0 = S.safe_int bytes in
      S.safe_int 7400 + (v0 lsr 5) + (v0 lsr 7)

    (* model ENCODING_CHAIN_ID *)
    let cost_ENCODING_CHAIN_ID = S.safe_int 50

    (* model ENCODING_PUBLIC_KEY_HASH_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 70

    (* model ENCODING_PUBLIC_KEY_HASH_p256 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_p256 = S.safe_int 70

    (* model ENCODING_PUBLIC_KEY_HASH_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 70

    (* model ENCODING_PUBLIC_KEY_ed25519 *)
    let cost_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 80

    (* model ENCODING_PUBLIC_KEY_p256 *)
    let cost_ENCODING_PUBLIC_KEY_p256 = S.safe_int 90

    (* model ENCODING_PUBLIC_KEY_secp256k1 *)
    let cost_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 455

    (* model ENCODING_SIGNATURE_ed25519 *)
    let cost_ENCODING_SIGNATURE_ed25519 = S.safe_int 45

    (* model ENCODING_SIGNATURE_p256 *)
    let cost_ENCODING_SIGNATURE_p256 = S.safe_int 45

    (* model ENCODING_SIGNATURE_secp256k1 *)
    let cost_ENCODING_SIGNATURE_secp256k1 = S.safe_int 45

    (* model ENCODING_Chest_key *)
    let cost_ENCODING_Chest_key = S.safe_int 13500

    (* model ENCODING_Chest *)
    (* Approximating 0.120086 x term *)
    let cost_ENCODING_Chest ~plaintext_size =
      let open S_syntax in
      let v0 = S.safe_int plaintext_size in
      S.safe_int 16630 + (v0 lsr 3)

    (* model TIMESTAMP_READABLE_DECODING *)
    let cost_TIMESTAMP_READABLE_DECODING = S.safe_int 100

    (* model TIMESTAMP_READABLE_ENCODING *)
    let cost_TIMESTAMP_READABLE_ENCODING = S.safe_int 820

    (* model CHECK_PRINTABLE *)
    let cost_CHECK_PRINTABLE size =
      let open S_syntax in
      S.safe_int 14 + (S.safe_int 10 * S.safe_int size)

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2264
       Rerun benchmarks due to faster gas monad.
       With the the redesign of the gas-monad this needs to be benchmarked again.
    *)
    (* model TY_EQ
       This is the estimated cost of one iteration of ty_eq, extracted
       and copied manually from the parameter fit for the TY_EQ benchmark
       (the model is parametric on the size of the type, which we don't have
       access to in O(1)). *)
    let cost_TY_EQ = S.safe_int 220

    (* model TYPECHECKING_CODE
       This is the cost of one iteration of parse_instr, extracted by hand from the
       parameter fit for the TYPECHECKING_CODE benchmark. *)
    let cost_TYPECHECKING_CODE = S.safe_int 220

    (* model UNPARSING_CODE
       This is the cost of one iteration of unparse_instr, extracted by hand from the
       parameter fit for the UNPARSING_CODE benchmark. *)
    let cost_UNPARSING_CODE = S.safe_int 115

    (* model TYPECHECKING_DATA
       This is the cost of one iteration of parse_data, extracted by hand from the
       parameter fit for the TYPECHECKING_DATA benchmark. *)
    let cost_TYPECHECKING_DATA = S.safe_int 100

    (* model UNPARSING_DATA
       This is the cost of one iteration of unparse_data, extracted by hand from the
       parameter fit for the UNPARSING_DATA benchmark. *)
    let cost_UNPARSING_DATA = S.safe_int 45

    (* model PARSE_TYPE
       This is the cost of one iteration of parse_ty, extracted by hand from the
       parameter fit for the PARSE_TYPE benchmark. *)
    let cost_PARSE_TYPE = S.safe_int 60

    (* model UNPARSE_TYPE
       This is the cost of one iteration of unparse_ty, extracted by hand from the
       parameter fit for the UNPARSE_TYPE benchmark. *)
    let cost_UNPARSE_TYPE type_size = S.mul (S.safe_int 20) type_size

    (* TODO: Add benchmarked value from [Unparse_comparable_type_benchmark]. *)
    let cost_UNPARSE_COMPARABLE_TYPE type_size = S.mul (S.safe_int 20) type_size

    (* model the unparse_views sub function of unparse_script *)
    let cost_UNPARSING_VIEWS number_of_views =
      S.mul (S.safe_int 30) (S.safe_int number_of_views)

    (* model unparse_script *)
    let cost_UNPARSING_SCRIPT = S.safe_int 460

    (* TODO: benchmark *)
    let cost_COMPARABLE_TY_OF_TY = S.safe_int 120

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2264
       Benchmark.
       Currently approximated by 2 comparisons of the longest entrypoint. *)
    let cost_FIND_ENTRYPOINT = cost_N_ICompare 31 31

    (* model SAPLING_TRANSACTION_ENCODING *)
    let cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs ~bound_data =
      S.safe_int (1500 + (inputs * 160) + (outputs * 320) + (bound_data lsr 3))

    (* model SAPLING_DIFF_ENCODING *)
    let cost_SAPLING_DIFF_ENCODING ~nfs ~cms =
      S.safe_int ((nfs * 22) + (cms * 215))
  end

  module Interpreter = struct
    open Generated_costs

    let drop = atomic_step_cost cost_N_IDrop

    let dup = atomic_step_cost cost_N_IDup

    let swap = atomic_step_cost cost_N_ISwap

    let cons_some = atomic_step_cost cost_N_ICons_some

    let cons_none = atomic_step_cost cost_N_ICons_none

    let if_none = atomic_step_cost cost_N_IIf_none

    let opt_map = atomic_step_cost cost_opt_map

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

    let list_map : 'a Script_typed_ir.boxed_list -> Gas.cost =
     fun {length; _} -> atomic_step_cost (cost_N_IList_map length)

    let list_size = atomic_step_cost cost_N_IList_size

    let list_iter : 'a Script_typed_ir.boxed_list -> Gas.cost =
     fun {length; _} -> atomic_step_cost (cost_N_IList_iter length)

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

    let big_map_elt_size = S.safe_int Script_expr_hash.size

    let big_map_mem ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_mem big_map_elt_size (S.safe_int size))

    let big_map_get ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_get big_map_elt_size (S.safe_int size))

    let big_map_update ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost (cost_N_IMap_update big_map_elt_size (S.safe_int size))

    let big_map_get_and_update ({size; _} : _ Script_typed_ir.big_map_overlay) =
      atomic_step_cost
        (cost_N_IMap_get_and_update big_map_elt_size (S.safe_int size))

    let add_seconds_timestamp :
        'a Script_int.num -> Script_timestamp.t -> Gas.cost =
     fun seconds timestamp ->
      let seconds_bytes = int_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost
        (cost_N_IAdd_seconds_to_timestamp seconds_bytes timestamp_bytes)

    let add_timestamp_seconds :
        Script_timestamp.t -> 'a Script_int.num -> Gas.cost =
     fun timestamp seconds ->
      let seconds_bytes = int_bytes seconds in
      let timestamp_bytes = z_bytes (Script_timestamp.to_zint timestamp) in
      atomic_step_cost
        (cost_N_IAdd_timestamp_to_seconds timestamp_bytes seconds_bytes)

    let sub_timestamp_seconds :
        Script_timestamp.t -> 'a Script_int.num -> Gas.cost =
     fun timestamp seconds ->
      let seconds_bytes = int_bytes seconds in
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

    let abs_int i = atomic_step_cost (cost_N_IAbs_int (int_bytes i))

    let int_nat = atomic_step_cost cost_N_IInt_nat

    let neg i = atomic_step_cost (cost_N_INeg (int_bytes i))

    let add_int i1 i2 =
      atomic_step_cost (cost_N_IAdd_int (int_bytes i1) (int_bytes i2))

    let add_nat i1 i2 =
      atomic_step_cost (cost_N_IAdd_nat (int_bytes i1) (int_bytes i2))

    let sub_int i1 i2 =
      atomic_step_cost (cost_N_ISub_int (int_bytes i1) (int_bytes i2))

    let mul_int i1 i2 =
      atomic_step_cost (cost_N_IMul_int (int_bytes i1) (int_bytes i2))

    let mul_nat i1 i2 =
      atomic_step_cost (cost_N_IMul_nat (int_bytes i1) (int_bytes i2))

    let ediv_teznat _tez _n = atomic_step_cost cost_N_IEdiv_teznat

    let ediv_tez = atomic_step_cost cost_N_IEdiv_tez

    let ediv_int i1 i2 =
      atomic_step_cost (cost_N_IEdiv_int (int_bytes i1) (int_bytes i2))

    let ediv_nat i1 i2 =
      atomic_step_cost (cost_N_IEdiv_nat (int_bytes i1) (int_bytes i2))

    let eq = atomic_step_cost cost_N_IEq

    let lsl_nat shifted = atomic_step_cost (cost_N_ILsl_nat (int_bytes shifted))

    let lsr_nat shifted = atomic_step_cost (cost_N_ILsr_nat (int_bytes shifted))

    let or_nat n1 n2 =
      atomic_step_cost (cost_N_IOr_nat (int_bytes n1) (int_bytes n2))

    let and_nat n1 n2 =
      atomic_step_cost (cost_N_IAnd_nat (int_bytes n1) (int_bytes n2))

    let and_int_nat n1 n2 =
      atomic_step_cost (cost_N_IAnd_int_nat (int_bytes n1) (int_bytes n2))

    let xor_nat n1 n2 =
      atomic_step_cost (cost_N_IXor_nat (int_bytes n1) (int_bytes n2))

    let not_int i = atomic_step_cost (cost_N_INot_int (int_bytes i))

    let if_ = atomic_step_cost cost_N_IIf

    let loop = atomic_step_cost cost_N_ILoop

    let loop_left = atomic_step_cost cost_N_ILoop_left

    let dip = atomic_step_cost cost_N_IDip

    let view = atomic_step_cost cost_N_IView

    let check_signature (pkey : Signature.public_key) b =
      let cost =
        match pkey with
        | Ed25519 _ -> cost_N_ICheck_signature_ed25519 (Bytes.length b)
        | Secp256k1 _ -> cost_N_ICheck_signature_secp256k1 (Bytes.length b)
        | P256 _ -> cost_N_ICheck_signature_p256 (Bytes.length b)
      in
      atomic_step_cost cost

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
      atomic_step_cost (cost_N_IMul_bls12_381_fr_z (int_bytes z))

    let mul_bls12_381_z_fr z =
      atomic_step_cost (cost_N_IMul_bls12_381_z_fr (int_bytes z))

    let int_bls12_381_fr = atomic_step_cost cost_N_IInt_bls12_381_z_fr

    let neg_bls12_381_g1 = atomic_step_cost cost_N_INeg_bls12_381_g1

    let neg_bls12_381_g2 = atomic_step_cost cost_N_INeg_bls12_381_g2

    let neg_bls12_381_fr = atomic_step_cost cost_N_INeg_bls12_381_fr

    let neq = atomic_step_cost cost_N_INeq

    let pairing_check_bls12_381 (l : 'a Script_typed_ir.boxed_list) =
      atomic_step_cost (cost_N_IPairing_check_bls12_381 l.length)

    let comb n = atomic_step_cost (cost_N_IComb n)

    let uncomb n = atomic_step_cost (cost_N_IUncomb n)

    let comb_get n = atomic_step_cost (cost_N_IComb_get n)

    let comb_set n = atomic_step_cost (cost_N_IComb_set n)

    let dupn n = atomic_step_cost (cost_N_IDupN n)

    let sapling_verify_update ~inputs ~outputs ~bound_data =
      atomic_step_cost (cost_N_ISapling_verify_update inputs outputs bound_data)

    let sapling_verify_update_deprecated ~inputs ~outputs =
      atomic_step_cost (cost_N_ISapling_verify_update inputs outputs 0)

    let sapling_empty_state = atomic_step_cost cost_N_ISapling_empty_state

    let halt = atomic_step_cost cost_N_IHalt

    let const = atomic_step_cost cost_N_IConst

    let empty_big_map = atomic_step_cost cost_N_IEmpty_big_map

    let lt = atomic_step_cost cost_N_ILt

    let le = atomic_step_cost cost_N_ILe

    let gt = atomic_step_cost cost_N_IGt

    let ge = atomic_step_cost cost_N_IGe

    let exec = atomic_step_cost cost_N_IExec

    let apply = atomic_step_cost cost_N_IApply

    let lambda = atomic_step_cost cost_N_ILambda

    let address = atomic_step_cost cost_N_IAddress

    let contract = atomic_step_cost cost_N_IContract

    let transfer_tokens = atomic_step_cost cost_N_ITransfer_tokens

    let implicit_account = atomic_step_cost cost_N_IImplicit_account

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

    let chain_id = atomic_step_cost cost_N_IChainId

    let ticket = atomic_step_cost cost_N_ITicket

    let read_ticket = atomic_step_cost cost_N_IRead_ticket

    let hash_key _ = atomic_step_cost cost_N_IHash_key

    let split_ticket _ amount_a amount_b =
      atomic_step_cost
        (cost_N_ISplit_ticket (int_bytes amount_a) (int_bytes amount_b))

    let open_chest ~chest ~time =
      let plaintext =
        Script_typed_ir.Script_timelock.get_plaintext_size chest
      in
      let log_time = Z.log2 Z.(add one time) in
      atomic_step_cost (cost_N_IOpen_chest ~chest:plaintext ~time:log_time)

    (* --------------------------------------------------------------------- *)
    (* Semi-hand-crafted models *)

    let compare_unit = atomic_step_cost (S.safe_int 10)

    let compare_pair_tag = atomic_step_cost (S.safe_int 10)

    let compare_union_tag = atomic_step_cost (S.safe_int 10)

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
      atomic_step_cost (cost_N_ICompare (int_bytes i1) (int_bytes i2))

    let compare_nat n1 n2 =
      atomic_step_cost (cost_N_ICompare (int_bytes n1) (int_bytes n2))

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

    (** TODO: https://gitlab.com/tezos/tezos/-/issues/2340
        Refine the gas model *)
    let compare_tx_rollup_l2_address = atomic_step_cost (cost_N_ICompare 48 48)

    let compare_chain_id = atomic_step_cost (S.safe_int 30)

    (* Defunctionalized CPS *)
    type cont =
      | Compare : 'a Script_typed_ir.comparable_ty * 'a * 'a * cont -> cont
      | Return : cont

    let compare : type a. a Script_typed_ir.comparable_ty -> a -> a -> cost =
     fun ty x y ->
      let rec compare :
          type a.
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
        | Tx_rollup_l2_address_t ->
            (apply [@tailcall]) Gas.(acc +@ compare_tx_rollup_l2_address) k
        | Chain_id_t -> (apply [@tailcall]) Gas.(acc +@ compare_chain_id) k
        | Pair_t (tl, tr, _, YesYes) ->
            (* Reasonable over-approximation of the cost of lexicographic comparison. *)
            let (xl, xr) = x in
            let (yl, yr) = y in
            (compare [@tailcall])
              tl
              xl
              yl
              Gas.(acc +@ compare_pair_tag)
              (Compare (tr, xr, yr, k))
        | Union_t (tl, tr, _, YesYes) -> (
            match (x, y) with
            | (L x, L y) ->
                (compare [@tailcall]) tl x y Gas.(acc +@ compare_union_tag) k
            | (L _, R _) -> (apply [@tailcall]) Gas.(acc +@ compare_union_tag) k
            | (R _, L _) -> (apply [@tailcall]) Gas.(acc +@ compare_union_tag) k
            | (R x, R y) ->
                (compare [@tailcall]) tr x y Gas.(acc +@ compare_union_tag) k)
        | Option_t (t, _, Yes) -> (
            match (x, y) with
            | (None, None) ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | (None, Some _) ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | (Some _, None) ->
                (apply [@tailcall]) Gas.(acc +@ compare_option_tag) k
            | (Some x, Some y) ->
                (compare [@tailcall]) t x y Gas.(acc +@ compare_option_tag) k)
      and apply cost k =
        match k with
        | Compare (ty, x, y, k) -> (compare [@tailcall]) ty x y cost k
        | Return -> cost
      in
      compare ty x y Gas.free Return
     [@@coq_axiom_with_reason "non top-level mutually recursive function"]

    let set_mem (type a) (elt : a) (set : a Script_typed_ir.set) =
      let open S_syntax in
      let (module Box) = Script_set.get set in
      let per_elt_cost = Box.OPS.elt_size elt |> Size.to_int |> S.safe_int in
      let size = S.safe_int Box.size in
      let intercept = atomic_step_cost (S.safe_int 115) in
      Gas.(intercept +@ (log2 size *@ per_elt_cost))

    let set_update (type a) (elt : a) (set : a Script_typed_ir.set) =
      let open S_syntax in
      let (module Box) = Script_set.get set in
      let per_elt_cost = Box.OPS.elt_size elt |> Size.to_int |> S.safe_int in
      let size = S.safe_int Box.size in
      let intercept = atomic_step_cost (S.safe_int 130) in
      (* The 2 factor reflects the update vs mem overhead as benchmarked
         on non-structured data *)
      Gas.(intercept +@ (S.safe_int 2 * log2 size *@ per_elt_cost))

    let map_mem (type k v) (elt : k) (map : (k, v) Script_typed_ir.map) =
      let open S_syntax in
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt |> Size.to_int |> S.safe_int in
      let size = S.safe_int Box.size in
      let intercept = atomic_step_cost (S.safe_int 80) in
      Gas.(intercept +@ (log2 size *@ per_elt_cost))

    let map_get = map_mem

    let map_update (type k v) (elt : k) (map : (k, v) Script_typed_ir.map) =
      let open S_syntax in
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt |> Size.to_int |> S.safe_int in
      let size = S.safe_int Box.size in
      let intercept = atomic_step_cost (S.safe_int 80) in
      (* The 2 factor reflects the update vs mem overhead as benchmarked
         on non-structured data *)
      Gas.(intercept +@ (S.safe_int 2 * log2 size *@ per_elt_cost))

    let map_get_and_update (type k v) (elt : k)
        (map : (k, v) Script_typed_ir.map) =
      let open S_syntax in
      let (module Box) = Script_map.get_module map in
      let per_elt_cost = Box.OPS.key_size elt |> Size.to_int |> S.safe_int in
      let size = S.safe_int Box.size in
      let intercept = atomic_step_cost (S.safe_int 80) in
      (* The 3 factor reflects the update vs mem overhead as benchmarked
         on non-structured data *)
      Gas.(intercept +@ (S.safe_int 3 * log2 size *@ per_elt_cost))

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
        +@ add_nat ticket_a.amount ticket_b.amount)

    (* Continuations *)
    module Control = struct
      let nil = atomic_step_cost cost_N_KNil

      let cons = atomic_step_cost cost_N_KCons

      let return = atomic_step_cost cost_N_KReturn

      let view_exit = atomic_step_cost cost_N_KView_exit

      let map_head = atomic_step_cost const_N_KMap_head

      let undip = atomic_step_cost cost_N_KUndip

      let loop_in = atomic_step_cost cost_N_KLoop_in

      let loop_in_left = atomic_step_cost cost_N_KLoop_in_left

      let iter = atomic_step_cost cost_N_KIter

      let list_enter_body xs ys_len =
        atomic_step_cost (cost_N_KList_enter_body xs ys_len)

      let list_exit_body = atomic_step_cost cost_N_KList_exit_body

      let map_enter_body = atomic_step_cost cost_N_KMap_enter_body

      let map_exit_body (type k v) (key : k) (map : (k, v) Script_typed_ir.map)
          =
        map_update key map
    end

    (* --------------------------------------------------------------------- *)
    (* Hand-crafted models *)

    (* The cost functions below where not benchmarked, a cost model was derived
       from looking at similar instructions. *)

    (* Cost for Concat_string is paid in two steps: when entering the interpreter,
       the user pays for the cost of computing the information necessary to compute
       the actual gas (so it's meta-gas): indeed, one needs to run through the
       list of strings to compute the total allocated cost.
       [concat_string_precheck] corresponds to the meta-gas cost of this computation.
    *)
    let concat_string_precheck (l : 'a Script_typed_ir.boxed_list) =
      (* we set the precheck to be slightly more expensive than cost_N_IList_iter *)
      atomic_step_cost (S.mul (S.safe_int l.length) (S.safe_int 10))

    (* This is the cost of allocating a string and blitting existing ones into it. *)
    let concat_string total_bytes =
      atomic_step_cost
        S.(add (S.safe_int 100) (S.ediv total_bytes (S.safe_int 10)))

    (* Same story as Concat_string. *)
    let concat_bytes total_bytes =
      atomic_step_cost
        S.(add (S.safe_int 100) (S.ediv total_bytes (S.safe_int 10)))

    (* Cost of access taken care of in Contract_storage.get_balance_carbonated *)
    let balance = Gas.free

    (* Cost of Unpack pays two integer comparisons, and a Bytes slice *)
    let unpack bytes =
      let blen = Bytes.length bytes in
      let open S_syntax in
      atomic_step_cost (S.safe_int 260 + (S.safe_int blen lsr 3))

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
    open Generated_costs

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_secp256k1
                cost_DECODING_PUBLIC_KEY_p256))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1
                cost_B58CHECK_DECODING_PUBLIC_KEY_p256))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_DECODING_PUBLIC_KEY_HASH_secp256k1
                cost_DECODING_PUBLIC_KEY_HASH_p256))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1
                cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_DECODING_SIGNATURE_ed25519
             (max
                cost_DECODING_SIGNATURE_secp256k1
                cost_DECODING_SIGNATURE_p256))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_DECODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_DECODING_SIGNATURE_secp256k1
                cost_B58CHECK_DECODING_SIGNATURE_p256))

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

    let merge_cycle = atomic_step_cost cost_TY_EQ

    let parse_type_cycle = atomic_step_cost cost_PARSE_TYPE

    let parse_instr_cycle = atomic_step_cost cost_TYPECHECKING_CODE

    let parse_data_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    let comparable_ty_of_ty_cycle = atomic_step_cost cost_COMPARABLE_TY_OF_TY

    (* Cost of a cycle of checking that a type is dupable *)
    (* TODO: bench *)
    let check_dupable_cycle = atomic_step_cost cost_TYPECHECKING_DATA

    let find_entrypoint_cycle = atomic_step_cost cost_FIND_ENTRYPOINT

    let bool = free

    let unit = free

    let timestamp_readable = atomic_step_cost cost_TIMESTAMP_READABLE_DECODING

    (* Reasonable estimate. *)
    let contract = Gas.(S.safe_int 2 *@ public_key_readable)

    (** TODO: https://gitlab.com/tezos/tezos/-/issues/2340
        Refine the gas model *)
    let tx_rollup_l2_address = bls12_381_g1

    (* Balance stored at /contracts/index/hash/balance, on 64 bits *)
    let contract_exists =
      Gas.cost_of_repr @@ Storage_costs.read_access ~path_length:4 ~read_bytes:8

    (* Constructing proof arguments consists in a decreasing loop in the result
       monad, allocating at each step. We charge a reasonable overapproximation. *)
    let proof_argument n =
      atomic_step_cost (S.mul (S.safe_int n) (S.safe_int 50))

    let chest_key = atomic_step_cost cost_DECODING_Chest_key

    let chest ~bytes = atomic_step_cost (cost_DECODING_Chest ~bytes)
  end

  module Unparsing = struct
    open Generated_costs

    let public_key_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_secp256k1
                cost_ENCODING_PUBLIC_KEY_p256))

    let public_key_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1
                cost_B58CHECK_ENCODING_PUBLIC_KEY_p256))

    let key_hash_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_ENCODING_PUBLIC_KEY_HASH_secp256k1
                cost_ENCODING_PUBLIC_KEY_HASH_p256))

    let key_hash_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_ed25519
             (max
                cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_secp256k1
                cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_p256))

    let signature_optimized =
      atomic_step_cost
      @@ S.(
           max
             cost_ENCODING_SIGNATURE_ed25519
             (max
                cost_ENCODING_SIGNATURE_secp256k1
                cost_ENCODING_SIGNATURE_p256))

    let signature_readable =
      atomic_step_cost
      @@ S.(
           max
             cost_B58CHECK_ENCODING_SIGNATURE_ed25519
             (max
                cost_B58CHECK_ENCODING_SIGNATURE_secp256k1
                cost_B58CHECK_ENCODING_SIGNATURE_p256))

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
      atomic_step_cost
      @@ cost_UNPARSE_TYPE Script_typed_ir.(ty_size ty |> Type_size.to_int)

    let unparse_comparable_type comp_ty =
      atomic_step_cost
      @@ cost_UNPARSE_COMPARABLE_TYPE
           Script_typed_ir.(comparable_ty_size comp_ty |> Type_size.to_int)

    let unparse_instr_cycle = atomic_step_cost cost_UNPARSING_CODE

    let unparse_data_cycle = atomic_step_cost cost_UNPARSING_DATA

    let unparse_views (views : Script_typed_ir.view_map) =
      let (module Box) = Script_map.get_module views in
      atomic_step_cost @@ cost_UNPARSING_VIEWS Box.size

    let unparse_script = atomic_step_cost cost_UNPARSING_SCRIPT

    let unit = Gas.free

    (* Reasonable estimate. *)
    let contract = Gas.(S.safe_int 2 *@ public_key_readable)

    (** TODO: https://gitlab.com/tezos/tezos/-/issues/2340
        Refine the gas model *)
    let tx_rollup_l2_address = bls12_381_g1

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
      atomic_step_cost (cost_ENCODING_Chest ~plaintext_size)
  end
end
