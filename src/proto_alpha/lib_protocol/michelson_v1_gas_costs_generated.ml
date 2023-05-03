(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

module S = Saturation_repr
module S_syntax = S.Syntax

(* This file is planned to be automatically generated. *)
(* If you want to update the following functions, update the gas model instead. *)

(* model N_IAdd_bls12_381_fr *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IAdd_bls12_381_fr = S.safe_int 30

(* model N_IAdd_bls12_381_g1 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IAdd_bls12_381_g1 = S.safe_int 900

(* model N_IAdd_bls12_381_g2 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IAdd_bls12_381_g2 = S.safe_int 2_470

(* model N_IAdd_tez *)
let cost_N_IAdd_tez = S.safe_int 20

(* model N_IAddress *)
let cost_N_IAddress = S.safe_int 10

(* model N_IAmount *)
let cost_N_IAmount = S.safe_int 10

(* model N_IAnd *)
let cost_N_IAnd = S.safe_int 10

(* model N_IBalance *)
let cost_N_IBalance = S.safe_int 10

(* model N_IBlake2b *)
(* Approximating 1.120804 x term *)
let cost_N_IBlake2b size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 430 + v0 + (v0 lsr 3)

(* model N_IBytes_size *)
let cost_N_IBytes_size = S.safe_int 10

(* model N_IByte_nat *)
(* fun size -> (73.6648173983 + (2.97857716538 * size)) *)
let cost_N_IBytes_nat size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 75 + (v0 lsl 1) + v0

(* model N_INat_bytes *)
(* fun size -> (41.2414840649 + (2.47956362254 * size)) *)
let cost_N_INat_bytes size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 45 + (v0 lsl 1) + (v0 lsr 1)

(* model N_IBytes_int *)
(* fun size -> (87.7348173983 + (3.04617929025 * size)) *)
let cost_N_IBytes_int size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 90 + (v0 lsl 1) + v0

(* model N_IInt_bytes *)
(* fun size -> (17.8814840649 + (2.57086493257 * size)) *)
let cost_N_IInt_bytes size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 20 + (v0 lsl 1) + (v0 lsr 1)

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

(* model N_ICheck_signature_bls *)
(* fun size -> (1566529.36815 + (2.94695684559 * size)) *)
let cost_N_ICheck_signature_bls size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 1_570_000 + (v0 lsl 1) + v0

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
  S.safe_int 20 + (v0 lsr 1) + (v0 lsr 4)

(* model N_IComb_set *)
(* Approximating 1.287531 x term *)
let cost_N_IComb_set size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 20 + (v0 + (v0 lsr 2) + (v0 lsr 5))

(* Model N_ICompare *)
(* Approximating 0.024413 x term *)
let cost_N_ICompare size1 size2 =
  let open S_syntax in
  let v0 = S.safe_int (Compare.Int.min size1 size2) in
  S.safe_int 35 + ((v0 lsr 6) + (v0 lsr 7))

(* model N_ICons_list *)
let cost_N_ICons_list = S.safe_int 10

(* model N_ICons_none *)
let cost_N_ICons_none = S.safe_int 10

(* model N_ICons_pair *)
let cost_N_ICons_pair = S.safe_int 10

(* model N_ICons_some *)
let cost_N_ICons_some = S.safe_int 10

(* model N_IPush *)
let cost_N_IPush = S.safe_int 10

(* model N_IUnit *)
let cost_N_IUnit = S.safe_int 10

(* model N_IDig *)
(* Approximating 6.750442 x term *)
let cost_N_IDig size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 30 + ((S.safe_int 6 * v0) + (v0 lsr 1) + (v0 lsr 2))

(* model N_IView *)
let cost_N_IView = S.safe_int 1460

(* model N_IDrop *)
let cost_N_IDrop = S.safe_int 10

(* model N_IDug *)
(* Approximating 6.718396 x term *)
let cost_N_IDug size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 35 + ((S.safe_int 6 * v0) + (v0 lsr 1) + (v0 lsr 2))

(* model N_IDup *)
let cost_N_IDup = S.safe_int 10

(* model N_IDupN *)
(* Approximating 1.222263 x term *)
let cost_N_IDupN size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 20 + v0 + (v0 lsr 2)

(* model N_IEdiv_tez *)
let cost_N_IEdiv_tez = S.safe_int 80

(* model N_IEmpty_map *)
let cost_N_IEmpty_map = S.safe_int 300

(* model N_IEmpty_set *)
let cost_N_IEmpty_set = S.safe_int 300

(* model N_IEq *)
let cost_N_IEq = S.safe_int 10

(* model N_IFailwith *)
(* let cost_N_IFailwith = S.safe_int 105 *)

(* model N_IGe *)
let cost_N_IGe = S.safe_int 10

(* model N_IGt *)
let cost_N_IGt = S.safe_int 10

(* model N_IHalt *)
let cost_N_IHalt = S.safe_int 15

(* model N_IHash_key *)
let cost_N_IHash_key = S.safe_int 605

(* model N_IImplicit_account *)
let cost_N_IImplicit_account = S.safe_int 10

(* model N_IInt_bls12_381_z_fr *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IInt_bls12_381_z_fr = S.safe_int 115

(* model N_IInt_nat *)
let cost_N_IInt_nat = S.safe_int 10

(* model N_IIs_nat *)
let cost_N_IIs_nat = S.safe_int 10

(* model N_IKeccak *)
(* Approximating 8.276352 x term *)
let cost_N_IKeccak size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 1350 + ((S.safe_int 8 * v0) + (v0 lsr 2))

(* model N_ILambda *)
let cost_N_ILambda = S.safe_int 10

(* model N_ILe *)
let cost_N_ILe = S.safe_int 10

(* model N_ILeft *)
let cost_N_ILeft = S.safe_int 10

(* model N_ILevel *)
let cost_N_ILevel = S.safe_int 10

(* model N_IList_map *)
let cost_N_IList_map = S.safe_int 20

(* model N_IList_size *)
let cost_N_IList_size = S.safe_int 10

(* model N_ILt *)
let cost_N_ILt = S.safe_int 10

(* model N_IMap_get *)
(* Approximating 0.048359 x term *)
let cost_N_IMap_get size1 size2 =
  let open S_syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 45 + (v0 lsr 5) + (v0 lsr 6)

(* model N_IMap_get_and_update *)
(* Approximating 0.145661 x term *)
let cost_N_IMap_get_and_update size1 size2 =
  let open S_syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 75 + (v0 lsr 3) + (v0 lsr 6)

(* model N_IMap_iter *)
(* Approximating 7.621331 x term *)
let cost_N_IMap_iter size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 50 + (S.safe_int 7 * v0) + (v0 lsr 1) + (v0 lsr 3)

(* model N_IMap_map *)
(* Approximating 8.38965386732 x term *)
let cost_N_IMap_map size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 40 + ((S.safe_int 8 * v0) + (v0 lsr 1))

(* model N_IMap_mem *)
(* Approximating 0.048446 x term *)
let cost_N_IMap_mem size1 size2 =
  let open S_syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 45 + (v0 lsr 5) + (v0 lsr 6)

(* model N_IMap_size *)
let cost_N_IMap_size = S.safe_int 10

(* model N_IMap_update *)
(* Approximating 0.097072 x term *)
let cost_N_IMap_update size1 size2 =
  let open S_syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 55 + (v0 lsr 4) + (v0 lsr 5)

(* model N_IMul_nattez *)
let cost_N_IMul_nattez = S.safe_int 50

(* model N_IMul_teznat *)
let cost_N_IMul_teznat = S.safe_int 50

(* model N_IEdiv_teznat *)
let cost_N_IEdiv_teznat = S.safe_int 70

(* model N_IMul_bls12_381_fr *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IMul_bls12_381_fr = S.safe_int 45

(* model N_IMul_bls12_381_fr_z *)
(* Approximating 1.059386 x term *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IMul_bls12_381_fr_z size1 =
  let open S_syntax in
  let v0 = S.safe_int size1 in
  S.safe_int 265 + v0 + (v0 lsr 4)

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
  S.safe_int 265 + v0 + (v0 lsr 4)

let cost_mul size1 size2 =
  let open S_syntax in
  let a = S.add (S.safe_int size1) (S.safe_int size2) in
  let v0 = a * log2 a in
  S.safe_int 55 + (v0 lsr 1) + (v0 lsr 2) + (v0 lsr 4)

(* model N_IMul_int *)
(* Approximating 0.857931 x term *)
let cost_N_IMul_int = cost_mul

(* model N_IMul_nat *)
(* Approximating 0.861823 x term *)
let cost_N_IMul_nat = cost_mul

(* model N_INeg_bls12_381_fr *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_INeg_bls12_381_fr = S.safe_int 30

(* model N_INeg_bls12_381_g1 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_INeg_bls12_381_g1 = S.safe_int 50

(* model N_INeg_bls12_381_g2 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_INeg_bls12_381_g2 = S.safe_int 70

(* model N_INeg *)
(* Allocates [size] bytes *)
let cost_N_INeg size =
  let open S_syntax in
  S.safe_int 25 + (S.safe_int size lsr 1)

(* model N_INeq *)
let cost_N_INeq = S.safe_int 10

(* model N_INil *)
let cost_N_INil = S.safe_int 10

(* model N_INot *)
let cost_N_INot = S.safe_int 10

(* model N_INot_int *)
(* Allocates [size] bytes *)
let cost_N_INot_int size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 25 + (v0 lsr 1)

(* model N_INot_bytes *)
(* Allocates [size] bytes *)
let cost_N_INot_bytes size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 30 + (v0 lsr 1)

(* model N_INow *)
let cost_N_INow = S.safe_int 10

(* model N_IMin_block_time *)
let cost_N_IMin_block_time = S.safe_int 20

(* model N_IOpen_chest *)
(* 612000 + chest * 19 + time * 19050 *)
let cost_N_IOpen_chest time chest =
  let open S_syntax in
  let v0 = S.safe_int chest in
  let v1 = S.safe_int time in
  S.safe_int 612_000 + (S.safe_int 19 * v0) + (S.safe_int 19050 * v1)

(* model N_IOr *)
let cost_N_IOr = S.safe_int 10

(* model N_IPairing_check_bls12_381 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IPairing_check_bls12_381 size =
  S.add (S.safe_int 450_000) (S.mul (S.safe_int 342_500) (S.safe_int size))

(* model N_IRead_ticket *)
let cost_N_IRead_ticket = S.safe_int 10

(* model N_IRight *)
let cost_N_IRight = S.safe_int 10

(* model N_ISelf_address *)
let cost_N_ISelf_address = S.safe_int 10

(* model N_ISelf *)
let cost_N_ISelf = S.safe_int 10

(* model N_ISender *)
let cost_N_ISender = S.safe_int 10

(* model N_ISet_delegate *)
let cost_N_ISet_delegate = S.safe_int 60

(* model N_ISet_iter *)
(* Approximating 7.633555 x term *)
let cost_N_ISet_iter size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 50 + (S.safe_int 7 * v0) + (v0 lsr 1) + (v0 lsr 3)

(* model N_ISet_size *)
let cost_N_ISet_size = S.safe_int 10

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

(* model N_ISource *)
let cost_N_ISource = S.safe_int 10

(* model N_IString_size *)
let cost_N_IString_size = S.safe_int 15

(* model N_ISub_tez *)
let cost_N_ISub_tez = S.safe_int 15

(* model N_ISub_tez_legacy *)
let cost_N_ISub_tez_legacy = S.safe_int 20

(* model N_ISwap *)
let cost_N_ISwap = S.safe_int 10

(* model N_ITicket *)
let cost_N_ITicket = S.safe_int 10

(* model N_ITotal_voting_power *)
let cost_N_ITotal_voting_power = S.safe_int 450

(* model N_IUncomb *)
(* Approximating 3.944710 x term *)
let cost_N_IUncomb size =
  let open S_syntax in
  let v0 = S.safe_int size in
  S.safe_int 25 + (S.safe_int 4 * v0)

(* model N_IUnpair *)
let cost_N_IUnpair = S.safe_int 10

(* model N_IVoting_power *)
let cost_N_IVoting_power = S.safe_int 640

(* model N_IXor *)
let cost_N_IXor = S.safe_int 15

(* model N_KLoop_in *)
let cost_N_KLoop_in = S.safe_int 10

(* model N_KLoop_in_left *)
let cost_N_KLoop_in_left = S.safe_int 10

(* model N_KNil *)
let cost_N_KNil = S.safe_int 15

(* model N_KReturn *)
let cost_N_KReturn = S.safe_int 10

(* model N_KView_exit *)
let cost_N_KView_exit = S.safe_int 20

(* model N_KMap_head *)
let cost_N_KMap_head = S.safe_int 20

(* model N_KUndip *)
let cost_N_KUndip = S.safe_int 10

(* model DECODING_BLS_FR *)
(* when benchmarking, compile bls12-381 without ADX, see
   https://gitlab.com/dannywillems/ocaml-bls12-381/-/blob/71d0b4d467fbfaa6452d702fcc408d7a70916a80/README.md#install
*)
let cost_DECODING_BLS_FR = S.safe_int 120

(* model DECODING_BLS_G1 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_DECODING_BLS_G1 = S.safe_int 54_600

(* model DECODING_BLS_G2 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_DECODING_BLS_G2 = S.safe_int 69_000

(* model B58CHECK_DECODING_CHAIN_ID *)
let cost_B58CHECK_DECODING_CHAIN_ID = S.safe_int 1_600

(* model B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 3_300

(* model B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 3_300

(* model B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 3_300

(* model B58CHECK_DECODING_PUBLIC_KEY_HASH_bls *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_bls = S.safe_int 3_600

(* model B58CHECK_DECODING_PUBLIC_KEY_ed25519 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 4_200

(* model B58CHECK_DECODING_PUBLIC_KEY_p256 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_p256 = S.safe_int 325_000

(* model B58CHECK_DECODING_PUBLIC_KEY_secp256k1 *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 9_000

(* model B58CHECK_DECODING_PUBLIC_KEY_bls *)
let cost_B58CHECK_DECODING_PUBLIC_KEY_bls = S.safe_int 79_000

(* model B58CHECK_DECODING_SIGNATURE_ed25519 *)
let cost_B58CHECK_DECODING_SIGNATURE_ed25519 = S.safe_int 6_400

(* model B58CHECK_DECODING_SIGNATURE_p256 *)
let cost_B58CHECK_DECODING_SIGNATURE_p256 = S.safe_int 6_400

(* model B58CHECK_DECODING_SIGNATURE_secp256k1 *)
let cost_B58CHECK_DECODING_SIGNATURE_secp256k1 = S.safe_int 6_400

(* model B58CHECK_DECODING_SIGNATURE_bls *)
let cost_B58CHECK_DECODING_SIGNATURE_bls = S.safe_int 6_400

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

(* model B58CHECK_ENCODING_PUBLIC_KEY_HASH_bls *)
let cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_bls = S.safe_int 3_200

(* model B58CHECK_ENCODING_PUBLIC_KEY_ed25519 *)
let cost_B58CHECK_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 4_500

(* model B58CHECK_ENCODING_PUBLIC_KEY_p256 *)
let cost_B58CHECK_ENCODING_PUBLIC_KEY_p256 = S.safe_int 4_550

(* model B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 *)
let cost_B58CHECK_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 4_950

(* model B58CHECK_ENCODING_PUBLIC_KEY_bls *)
let cost_B58CHECK_ENCODING_PUBLIC_KEY_bls = S.safe_int 5_900

(* model B58CHECK_ENCODING_SIGNATURE_ed25519 *)
let cost_B58CHECK_ENCODING_SIGNATURE_ed25519 = S.safe_int 8_300

(* model B58CHECK_ENCODING_SIGNATURE_p256 *)
let cost_B58CHECK_ENCODING_SIGNATURE_p256 = S.safe_int 8_300

(* model B58CHECK_ENCODING_SIGNATURE_secp256k1 *)
let cost_B58CHECK_ENCODING_SIGNATURE_secp256k1 = S.safe_int 8_300

(* model B58CHECK_ENCODING_SIGNATURE_bls *)
let cost_B58CHECK_ENCODING_SIGNATURE_bls = S.safe_int 8_300

(* model DECODING_CHAIN_ID *)
let cost_DECODING_CHAIN_ID = S.safe_int 50

(* model DECODING_PUBLIC_KEY_HASH_ed25519 *)
let cost_DECODING_PUBLIC_KEY_HASH_ed25519 = S.safe_int 60

(* model DECODING_PUBLIC_KEY_HASH_p256 *)
let cost_DECODING_PUBLIC_KEY_HASH_p256 = S.safe_int 60

(* model DECODING_PUBLIC_KEY_HASH_secp256k1 *)
let cost_DECODING_PUBLIC_KEY_HASH_secp256k1 = S.safe_int 60

(* model DECODING_PUBLIC_KEY_HASH_bls *)
let cost_DECODING_PUBLIC_KEY_HASH_bls = S.safe_int 60

(* model DECODING_PUBLIC_KEY_ed25519 *)
let cost_DECODING_PUBLIC_KEY_ed25519 = S.safe_int 60

(* model DECODING_PUBLIC_KEY_p256 *)
let cost_DECODING_PUBLIC_KEY_p256 = S.safe_int 18_800

(* model DECODING_PUBLIC_KEY_secp256k1 *)
let cost_DECODING_PUBLIC_KEY_secp256k1 = S.safe_int 4_900

(* model DECODING_PUBLIC_KEY_bls *)
let cost_DECODING_PUBLIC_KEY_bls = S.safe_int 74_000

(* model DECODING_SIGNATURE_ed25519 *)
let cost_DECODING_SIGNATURE_ed25519 = S.safe_int 35

(* model DECODING_SIGNATURE_p256 *)
let cost_DECODING_SIGNATURE_p256 = S.safe_int 35

(* model DECODING_SIGNATURE_secp256k1 *)
let cost_DECODING_SIGNATURE_secp256k1 = S.safe_int 35

(* model DECODING_SIGNATURE_bls *)
let cost_DECODING_SIGNATURE_bls = S.safe_int 40

(* model DECODING_Chest_key *)
let cost_DECODING_Chest_key = S.safe_int 5900

(* model DECODING_Chest *)
(* Approximating 0.039349 x term *)
let cost_DECODING_Chest bytes =
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

(* model ENCODING_PUBLIC_KEY_HASH_bls *)
let cost_ENCODING_PUBLIC_KEY_HASH_bls = S.safe_int 80

(* model ENCODING_PUBLIC_KEY_ed25519 *)
let cost_ENCODING_PUBLIC_KEY_ed25519 = S.safe_int 80

(* model ENCODING_PUBLIC_KEY_p256 *)
let cost_ENCODING_PUBLIC_KEY_p256 = S.safe_int 90

(* model ENCODING_PUBLIC_KEY_secp256k1 *)
let cost_ENCODING_PUBLIC_KEY_secp256k1 = S.safe_int 455

(* model ENCODING_PUBLIC_KEY_bls *)
let cost_ENCODING_PUBLIC_KEY_bls = S.safe_int 90

(* model ENCODING_SIGNATURE_ed25519 *)
let cost_ENCODING_SIGNATURE_ed25519 = S.safe_int 45

(* model ENCODING_SIGNATURE_p256 *)
let cost_ENCODING_SIGNATURE_p256 = S.safe_int 45

(* model ENCODING_SIGNATURE_secp256k1 *)
let cost_ENCODING_SIGNATURE_secp256k1 = S.safe_int 45

(* model ENCODING_SIGNATURE_bls *)
let cost_ENCODING_SIGNATURE_bls = S.safe_int 55

(* model ENCODING_Chest_key *)
let cost_ENCODING_Chest_key = S.safe_int 10_000

(* model ENCODING_Chest *)
(* Approximating 0.120086 x term *)
let cost_ENCODING_Chest plaintext_size =
  let open S_syntax in
  let v0 = S.safe_int plaintext_size in
  S.safe_int 12_200 + (v0 lsr 3)

(* model TIMESTAMP_READABLE_DECODING *)
(* Approximating 0.045400 x term *)
let cost_TIMESTAMP_READABLE_DECODING bytes =
  let open S_syntax in
  let b = S.safe_int bytes in
  let v0 = S.mul (S.sqrt b) b in
  S.safe_int 105 + ((v0 lsr 5) + (v0 lsr 6))

(* model TIMESTAMP_READABLE_ENCODING *)
let cost_TIMESTAMP_READABLE_ENCODING = S.safe_int 820
