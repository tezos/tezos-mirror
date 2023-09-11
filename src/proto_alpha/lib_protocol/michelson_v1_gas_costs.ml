(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022-2023 DaiLambda, Inc. <contact@dailambda.jp>            *)
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

include Michelson_v1_gas_costs_generated
module S = Saturation_repr

(** Hand-edited/written cost functions *)

(* Functions to be replaced by the generated code.

   The codegen cannot generate exactly the same code here. They have to
   be replaced by the generated versions.
*)

(* generated code is not usable: not possible to generate the same coeff *)
(* model N_IPairing_check_bls12_381 *)
(* when benchmarking, compile bls12-381 without ADX *)
let cost_N_IPairing_check_bls12_381 size =
  S.add (S.safe_int 450_000) (S.mul (S.safe_int 342_500) (S.safe_int size))

(* generated code is not usable: the const is not on a grid point *)
(* model N_ILsl_nat *)
(* Allocates at most [size + 256] bytes *)
let cost_N_ILsl_nat size =
  let open S.Syntax in
  let v0 = S.safe_int size in
  S.safe_int 128 + (v0 lsr 1)

(* generated code is not usable: the const is not on a grid point *)
(* model CHECK_PRINTABLE *)
(* Inferred: fun size -> (0. + (1.42588022179 * size)) *)
let cost_CHECK_PRINTABLE size =
  let open S.Syntax in
  S.safe_int 14 + (S.safe_int 10 * S.safe_int size)

(* generated code is not usable: the actual code and the model differ *)
(* model N_ILsl_bytes *)
(* Allocates [size + shift / 8] bytes *)
(* fun size1 -> fun size2 -> ((63.0681507316 + (0.667539714647 * size1)) + (0. * size2)) *)
let cost_N_ILsl_bytes size shift =
  let open S.Syntax in
  let v1 = S.safe_int size in
  let v0 = S.safe_int shift in
  S.safe_int 65 + (v1 lsr 1) + (v1 lsr 2) + (v0 lsr 4)

(* generated code is not usable: the actual code and the model differ *)
(* Model N_ICompare *)
(* Approximating 0.024413 x term *)
let cost_N_ICompare size1 size2 =
  let open S.Syntax in
  let v0 = S.safe_int (Compare.Int.min size1 size2) in
  S.safe_int 35 + ((v0 lsr 6) + (v0 lsr 7))

(* generated code is not usable: the actual code and the model differ *)
(* used for big_map_get *)
(* model N_IMap_get *)
(* Approximating 0.048359 x term *)
let cost_N_IMap_get size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 45 + (v0 lsr 5) + (v0 lsr 6)

(* generated code is not usable: the actual code and the model differ *)
(* used for big_map_get_and_update *)
(* model N_IMap_get_and_update *)
(* Approximating 0.145661 x term *)
let cost_N_IMap_get_and_update size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 75 + (v0 lsr 3) + (v0 lsr 6)

(* generated code is not usable: the actual code and the model differ *)
(* used for map_get_and_update *)
(* model N_IMap_get_and_update *)
let cost_N_IMap_get_and_update' size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let intercept = S.safe_int 80 in
  (* The 3 factor reflects the update vs mem overhead as benchmarked
     on non-structured data *)
  intercept + (S.safe_int 3 * log2 size2 * size1)

(* generated code is not usable: the actual code and the model differ *)
(* used for bigmap_update *)
(* model N_IMap_update *)
(* Approximating 0.097072 x term *)
let cost_N_IMap_update size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 55 + (v0 lsr 4) + (v0 lsr 5)

(* generated code is not usable: the actual code and the model differ *)
(* used for map_update *)
(* model N_IMap_update *)
let cost_N_IMap_update' size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let intercept = S.safe_int 80 in
  (* The 2 factor reflects the update vs mem overhead as benchmarked
     on non-structured data *)
  intercept + (S.safe_int 2 * log2 size2 * size1)

(* generated code is not usable: the actual code and the model differ *)
(* used for bigmap_mem *)
(* model N_IMap_mem *)
(* Approximating 0.048446 x term *)
let cost_N_IMap_mem size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v0 = size1 * log2 size2 in
  S.safe_int 45 + (v0 lsr 5) + (v0 lsr 6)

(* generated code is not usable: the actual code and the model differ *)
(* used for map_mem *)
(* model N_IMap_mem *)
let cost_N_IMap_mem' size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let intercept = S.safe_int 80 in
  intercept + (log2 size2 * size1)

(* generated code is not usable: the actual code and the model differ *)
(* model interpreter/N_ISet_mem *)
let cost_N_ISet_mem size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let intercept = S.safe_int 115 in
  intercept + (log2 size2 * size1)

(* generated code is not usable: the actual code and the model differ *)
let cost_N_ISet_update size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let intercept = S.safe_int 130 in
  (* The 2 factor reflects the update vs mem overhead as benchmarked
     on non-structured data *)
  intercept + (S.safe_int 2 * log2 size2 * size1)

let cost_mul size1 size2 =
  let open S.Syntax in
  let a = S.add (S.safe_int size1) (S.safe_int size2) in
  let v0 = a * log2 a in
  S.safe_int 55 + (v0 lsr 1) + (v0 lsr 2) + (v0 lsr 4)

(* generated code is not usable: the actual code and the model differ *)
(* model N_IMul_int *)
(* Approximating 0.857931 x term *)
let cost_N_IMul_int = cost_mul

(* generated code is not usable: the actual code and the model differ *)
(* model N_IMul_nat *)
(* Approximating 0.861823 x term *)
let cost_N_IMul_nat = cost_mul

(* generated code is not usable: not possible to generate the same coeff *)
(* model interpreter/N_ISapling_verify_update *)
(* fun size1 -> fun size2 -> ((432200.469784 + (5738377.05148 * size1)) + (4634026.28586 * size2)) *)
let cost_N_ISapling_verify_update size1 size2 =
  let open S.Syntax in
  let size1 = S.safe_int size1 in
  let size2 = S.safe_int size2 in
  let v1 = size1 in
  let v0 = size2 in
  S.safe_int 432500 + (v1 * S.safe_int 5740000) + (v0 * S.safe_int 4635000)

(* ------------------------------------------------------------------------ *)

(* N_ISapling_verify_update_with_blake2b
   This function depends on another cost function cost_N_IBlake2b.
   Such code can't be generated by the current Snoop. *)
let cost_N_ISapling_verify_update_with_blake2b size1 size2 bound_data =
  let open S.Syntax in
  cost_N_IBlake2b bound_data + cost_N_ISapling_verify_update size1 size2

(* N_IApply
   The current generated model receives int as a flag,
   but it should receive bool. *)
(* model N_IApply *)
(* fun size -> if (size = 0) then 140 else 220 *)
let cost_N_IApply rec_flag = if rec_flag then S.safe_int 220 else S.safe_int 140

(* N_KMap_enter_body
   Removed conversion of [size] for optimization *)
(* model N_KMap_enter_body *)
let cost_N_KMap_enter_body size =
  if Compare.Int.(size = 0) then S.safe_int 10 else S.safe_int 80

(* N_KList_enter_body
   The generated model receives the length of `xs` as the first argument
   and branches on whether it is 0 or not.
   However, calculating the length makes the performance worse.
   The model should be changed to receive `xs_is_nil` as the first argument. *)
(* model N_KList_enter_body *)
(* Approximating 1.797068 x term *)
let cost_N_KList_enter_body xs size_ys =
  match xs with
  | [] ->
      let open S.Syntax in
      let v0 = S.safe_int size_ys in
      S.safe_int 30 + (v0 + (v0 lsr 1) + (v0 lsr 2) + (v0 lsr 4))
  | _ :: _ -> S.safe_int 30

(* model translator/TY_EQ *)
(* fun size -> (31.1882471167 + (21.8805791266 * size)) *)
let cost_TY_EQ size =
  let open S.Syntax in
  let v0 = size in
  S.safe_int 35 + (v0 * S.safe_int 22)

(* model PARSE_TYPE
   This is the cost of one iteration of parse_ty, extracted by hand from the
   parameter fit for the PARSE_TYPE benchmark. *)
let cost_PARSE_TYPE1 = cost_PARSE_TYPE 1

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
let cost_UNPARSING_DATA = S.safe_int 65

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2264
   Benchmark.
   Currently approximated by 2 comparisons of the longest entrypoint. *)
let cost_FIND_ENTRYPOINT = cost_N_ICompare 31 31

(* ------------------------------------------------------------------------ *)

(* These functions lack the corresponding models. *)

(* model SAPLING_TRANSACTION_ENCODING *)
let cost_SAPLING_TRANSACTION_ENCODING ~inputs ~outputs ~bound_data =
  S.safe_int (1500 + (inputs * 160) + (outputs * 320) + (bound_data lsr 3))

(* model SAPLING_DIFF_ENCODING *)
let cost_SAPLING_DIFF_ENCODING ~nfs ~cms = S.safe_int ((nfs * 22) + (cms * 215))

(* ------------------------------------------------------------------------ *)

(* model N_ILoop_{in,out} *)
let cost_N_ILoop = S.max cost_N_ILoop_in cost_N_ILoop_out

(* model N_ILoop_left_{in,out} *)
let cost_N_ILoop_left = S.max cost_N_ILoop_left_in cost_N_ILoop_left_out

(* model N_IOpt_map_{some,none} *)
let cost_N_IOpt_map = S.max cost_N_IOpt_map_none cost_N_IOpt_map_some

(* model N_ILambda_{lam,lamrec} *)
let cost_N_ILambda = S.max cost_N_ILambda_lam cost_N_ILambda_lamrec

(* model N_KIter_{empty,nonempty} *)
let cost_N_KIter = S.max cost_N_KIter_empty cost_N_KIter_nonempty

(* ------------------------------------------------------------------------ *)

(* IDropN and IDupN use non affine models with multiple cases. The inferred
   cost functions are more complex than the following affine functions. *)

(* model N_IDropN *)
(* Approximating 2.713108 x term *)
let cost_N_IDropN size =
  let open S.Syntax in
  let v0 = S.safe_int size in
  S.safe_int 30 + (S.safe_int 2 * v0) + (v0 lsr 1) + (v0 lsr 3)

(* model N_IDupN *)
(* Approximating 1.222263 x term *)
let cost_N_IDupN size =
  let open S.Syntax in
  let v0 = S.safe_int size in
  S.safe_int 20 + v0 + (v0 lsr 2)

(* ------------------------------------------------------------------------ *)

(* Following functions are partially carbonated: they charge some gas
   by themselves.  Their inferred gas parameters cannot be directly
   used since they should contain the partial carbonation.
*)

(* model N_IContract *)
(* Inferred value: 703.26072741 *)
(* Most computation happens in [parse_contract_for_script], which is
   carbonated. *)
let cost_N_IContract = S.safe_int 30

(* model N_ICreate_contract *)
(* Inferred value: 814.154060743 *)
(* Most computation happens in [create_contract], which is carbonated. *)
let cost_N_ICreate_contract = S.safe_int 60

(* model N_ITransfer_tokens *)
(* Inferred value: 230.707394077 *)
(* Most computation happens in [transfer], which is carbonated. *)
let cost_N_ITransfer_tokens = S.safe_int 60

(* model IEmit *)
(* Inferred value: 244.687394077 *)
(* Most computation happens in [emit_event], which is carbonated. *)
let cost_N_IEmit = S.safe_int 30

(* --------------------------------------------------------------------- *)

(* The cost functions below where not benchmarked, a cost model was derived
    from looking at similar instructions. *)
(* Cost for Concat_string is paid in two steps: when entering the interpreter,
    the user pays for the cost of computing the information necessary to compute
    the actual gas (so it's meta-gas): indeed, one needs to run through the
    list of strings to compute the total allocated cost.
    [concat_string_precheck] corresponds to the meta-gas cost of this computation.
*)

let cost_N_IConcat_string_precheck length =
  (* we set the precheck to be slightly more expensive than cost_N_IList_iter *)
  let open S.Syntax in
  let length = S.safe_int length in
  length * S.safe_int 10

(* This is the cost of allocating a string and blitting existing ones into it. *)
let cost_N_IConcat_string total_bytes =
  let open S.Syntax in
  S.safe_int 100 + (total_bytes lsr 1)

(* Same story as Concat_string. *)
let cost_N_IConcat_bytes total_bytes =
  let open S.Syntax in
  S.safe_int 100 + (total_bytes lsr 1)

(* A partially carbonated instruction,
   so its model does not correspond to this function *)
(* Cost of Unpack pays two integer comparisons, and a Bytes slice *)
let cost_N_IUnpack total_bytes =
  let open S.Syntax in
  let total_bytes = S.safe_int total_bytes in
  S.safe_int 260 + (total_bytes lsr 1)
