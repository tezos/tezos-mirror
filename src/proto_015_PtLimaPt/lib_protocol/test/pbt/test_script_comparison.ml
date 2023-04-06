(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Testing
    -------
    Component:    Script_comparison
    Invocation:   dune exec src/proto_015_PtLimaPt/lib_protocol/test/pbt/main.exe
    Subject:      PBT of the Script_comparable.compare_comparable function.
*)

open Protocol
open Alpha_context
open Script_typed_ir
open Qcheck2_helpers

(* Reference implementation *)

let normalize_compare c =
  let open Compare.Int in
  if c > 0 then 1 else if c < 0 then -1 else 0

(* This reference implementation of the Michelson comparison function is a
   simplified version of the Script_ir_translator.compare_comparable function
   that was used in the Florence protocol, before a refactoring broke it in
   Granada. *)
let rec reference_compare_comparable : type a. a comparable_ty -> a -> a -> int
    =
 fun ty x y ->
  match (ty, x, y) with
  | Unit_t, (), () -> 0
  | Never_t, _, _ -> .
  | Signature_t, x, y -> normalize_compare @@ Script_signature.compare x y
  | String_t, x, y -> normalize_compare @@ Script_string.compare x y
  | Bool_t, x, y -> normalize_compare @@ Compare.Bool.compare x y
  | Mutez_t, x, y -> normalize_compare @@ Tez.compare x y
  | Key_hash_t, x, y ->
      normalize_compare @@ Tezos_crypto.Signature.V0.Public_key_hash.compare x y
  | Key_t, x, y ->
      normalize_compare @@ Tezos_crypto.Signature.V0.Public_key.compare x y
  | Int_t, x, y -> normalize_compare @@ Script_int.compare x y
  | Nat_t, x, y -> normalize_compare @@ Script_int.compare x y
  | Timestamp_t, x, y -> normalize_compare @@ Script_timestamp.compare x y
  | Address_t, x, y ->
      normalize_compare @@ Script_comparable.compare_address x y
  | Tx_rollup_l2_address_t, x, y ->
      normalize_compare @@ Script_comparable.compare_tx_rollup_l2_address x y
  | Bytes_t, x, y -> normalize_compare @@ Compare.Bytes.compare x y
  | Chain_id_t, x, y -> normalize_compare @@ Script_chain_id.compare x y
  | Pair_t (tl, tr, _, YesYes), (lx, rx), (ly, ry) ->
      let cl = reference_compare_comparable tl lx ly in
      if Compare.Int.(cl = 0) then reference_compare_comparable tr rx ry else cl
  | Union_t (tl, _, _, YesYes), L x, L y -> reference_compare_comparable tl x y
  | Union_t _, L _, R _ -> -1
  | Union_t _, R _, L _ -> 1
  | Union_t (_, tr, _, YesYes), R x, R y -> reference_compare_comparable tr x y
  | Option_t _, None, None -> 0
  | Option_t _, None, Some _ -> -1
  | Option_t _, Some _, None -> 1
  | Option_t (t, _, Yes), Some x, Some y -> reference_compare_comparable t x y

(* Generation of one to three values of the same comparable type. *)

type ex_comparable_data =
  | Ex_comparable_data : 'a comparable_ty * 'a -> ex_comparable_data

type ex_comparable_data_2 =
  | Ex_comparable_data_2 : 'a comparable_ty * 'a * 'a -> ex_comparable_data_2

type ex_comparable_data_3 =
  | Ex_comparable_data_3 :
      'a comparable_ty * 'a * 'a * 'a
      -> ex_comparable_data_3

(* We use the Michelson samplers from lib_benchmark and turn them into QCheck2
   generators *)
module Parameters = struct
  let atom_size_range : Tezos_benchmark.Base_samplers.range =
    {min = 0; max = 10}

  let other_size : Tezos_benchmark.Base_samplers.range = {min = 0; max = 100}

  let parameters : Michelson_samplers.parameters =
    {
      base_parameters =
        {
          int_size = atom_size_range;
          string_size = atom_size_range;
          bytes_size = atom_size_range;
        };
      list_size = other_size;
      set_size = other_size;
      map_size = other_size;
    }
end

module Crypto_samplers =
Tezos_benchmark.Crypto_samplers.V0.Make_finite_key_pool (struct
  let size = 1000

  let algo = `Default
end)

module Samplers : Michelson_samplers.S =
  Michelson_samplers.Make (Parameters) (Crypto_samplers)

let ex_comparable_data_sampler :
    ex_comparable_data Tezos_benchmark.Base_samplers.sampler =
 fun random_state ->
  let size =
    Tezos_benchmark.Base_samplers.sample_in_interval
      ~range:{min = 1; max = 20}
      random_state
  in
  let (Ex_comparable_ty ty) =
    Samplers.Random_type.m_comparable_type ~size random_state
  in
  let x = Samplers.Random_value.comparable ty random_state in
  Ex_comparable_data (ty, x)

let ex_comparable_data_2_sampler :
    ex_comparable_data_2 Tezos_benchmark.Base_samplers.sampler =
 fun random_state ->
  let size =
    Tezos_benchmark.Base_samplers.sample_in_interval
      ~range:{min = 1; max = 20}
      random_state
  in
  let (Ex_comparable_ty ty) =
    Samplers.Random_type.m_comparable_type ~size random_state
  in
  let x = Samplers.Random_value.comparable ty random_state in
  let y = Samplers.Random_value.comparable ty random_state in
  Ex_comparable_data_2 (ty, x, y)

let ex_comparable_data_3_sampler :
    ex_comparable_data_3 Tezos_benchmark.Base_samplers.sampler =
 fun random_state ->
  let size =
    Tezos_benchmark.Base_samplers.sample_in_interval
      ~range:{min = 1; max = 20}
      random_state
  in
  let (Ex_comparable_ty ty) =
    Samplers.Random_type.m_comparable_type ~size random_state
  in
  let x = Samplers.Random_value.comparable ty random_state in
  let y = Samplers.Random_value.comparable ty random_state in
  let z = Samplers.Random_value.comparable ty random_state in
  Ex_comparable_data_3 (ty, x, y, z)

let comparable_data_generator =
  QCheck2.Gen.make_primitive ~gen:ex_comparable_data_sampler ~shrink:(fun _ ->
      Seq.empty)

let comparable_data_2_generator =
  QCheck2.Gen.make_primitive ~gen:ex_comparable_data_2_sampler ~shrink:(fun _ ->
      Seq.empty)

let comparable_data_3_generator =
  QCheck2.Gen.make_primitive ~gen:ex_comparable_data_3_sampler ~shrink:(fun _ ->
      Seq.empty)

(* We need a context because packing (used in one of the tests) and unparsing
   (used for pretty-printing error messages) Michelson data are carbonated
   operations. But since we don't care about gas consumption here we use the
   same value of type context everywhere instead of threading it through the
   error monad. *)

let assert_ok = function Ok x -> x | Error _ -> assert false

let assert_return x = assert_ok (Lwt_main.run x)

let ctxt =
  assert_return
    (let open Lwt_result_syntax in
    let* b, _cs = Context.init3 () in
    let* v = Incremental.begin_construction b in
    return (Incremental.alpha_ctxt v))

let unparse_comparable_ty ty =
  Micheline.strip_locations
    (fst (assert_ok Script_ir_unparser.(unparse_ty ~loc:() ctxt ty)))

let unparse_comparable_data ty x =
  fst (assert_return Script_ir_translator.(unparse_data ctxt Readable ty x))

let pack_comparable_data ty x =
  fst (assert_return Script_ir_translator.(pack_data ctxt ty x))

let unpack_comparable_data ty bytes =
  fst (assert_return (Script_interpreter_defs.unpack ctxt ~ty ~bytes))

let pp_comparable_ty fmt ty =
  Michelson_v1_printer.print_expr fmt (unparse_comparable_ty ty)

let pp_comparable_data ty fmt x =
  Michelson_v1_printer.print_expr fmt (unparse_comparable_data ty x)

let pp ty x y pp_c fmt c =
  Format.fprintf
    fmt
    "Compare(ty=%a, %a, %a) = %a"
    pp_comparable_ty
    ty
    (pp_comparable_data ty)
    x
    (pp_comparable_data ty)
    y
    pp_c
    c

let compare_through_pack ty x y =
  Bytes.compare (pack_comparable_data ty x) (pack_comparable_data ty y) = 0

let qcheck_compare_comparable ~expected ty x y =
  qcheck_eq
    ~pp:(pp ty x y Format.pp_print_int)
    expected
    (Script_comparable.compare_comparable ty x y)

let qcheck_compare_comparable_eq ~expected ty x y =
  qcheck_eq
    ~pp:(pp ty x y Format.pp_print_bool)
    expected
    (Script_comparable.compare_comparable ty x y = 0)

(* Test.
 * Tests that compare_comparable returns the same values than the reference
 * implementation.
 *)
let test_compatible_with_reference =
  QCheck2.Test.make
    ~name:"compatible_with_reference"
    comparable_data_2_generator
    (fun (Ex_comparable_data_2 (ty, x, y)) ->
      qcheck_compare_comparable
        ~expected:(reference_compare_comparable ty x y)
        ty
        x
        y)

(* Test.
 * Tests that compare_comparable returns 0 iff packing then comparing the
 * resulting bytes returns 0.
 *)
let test_compatible_with_packing =
  QCheck2.Test.make
    ~name:"compatible_with_packing"
    comparable_data_2_generator
    (fun (Ex_comparable_data_2 (ty, x, y)) ->
      qcheck_compare_comparable_eq
        ~expected:(compare_through_pack ty x y)
        ty
        x
        y)

(* Test.
 * Tests that compare_comparable is reflexive.
 *)
let test_reflexivity =
  QCheck2.Test.make
    ~name:"reflexivity"
    comparable_data_generator
    (fun (Ex_comparable_data (ty, x)) ->
      qcheck_compare_comparable ~expected:0 ty x x)

(* Test.
 * Tests that compare_comparable is symmetric.
 *)
let test_symmetry =
  QCheck2.Test.make
    ~name:"symmetry"
    comparable_data_2_generator
    (fun (Ex_comparable_data_2 (ty, x, y)) ->
      qcheck_compare_comparable
        ~expected:(-Script_comparable.compare_comparable ty x y)
        ty
        y
        x)

(* Test.
 * Tests that compare_comparable is transitive.
 *)
let test_transitivity =
  QCheck2.Test.make
    ~name:"transitivity"
    comparable_data_3_generator
    (fun (Ex_comparable_data_3 (ty, x, y, z)) ->
      let cxy = Script_comparable.compare_comparable ty x y in
      let cyz = Script_comparable.compare_comparable ty y z in
      match (cxy, cyz) with
      | 0, n | n, 0 -> qcheck_compare_comparable ~expected:n ty x z
      | -1, -1 -> qcheck_compare_comparable ~expected:(-1) ty x z
      | 1, 1 -> qcheck_compare_comparable ~expected:1 ty x z
      | _ -> QCheck2.assume_fail ())

(* Test.
 * Tests the round-trip property for PACK and UNPACK (modulo compare_comparable).
 *)
let test_pack_unpack =
  QCheck2.Test.make
    ~count:100_000
      (* We run this test on many more cases than the default (100) because this
         is a very important property. Packing and then unpacking happens each
         time data is sent from a contract to another and also each time storage
         is saved at the end of a smart contract call and restored at the next
         call of the same contract. Also, injectivity of packing (which is a
         direct consequence of this) is an important property for big maps
         (because the keys are packed and then hashed). *)
    ~name:"pack_unpack"
    comparable_data_generator
    (fun (Ex_comparable_data (ty, x)) ->
      let oty =
        match comparable_option_t (-1) ty with
        | Ok ty -> ty
        | Error _ -> assert false
      in
      qcheck_eq
        ~cmp:(Script_comparable.compare_comparable oty)
        ~pp:(pp_comparable_data oty)
        (Some x)
        (unpack_comparable_data ty (pack_comparable_data ty x)))

let () =
  Alcotest.run
    ~__FILE__
    "protocol > pbt > script_comparison"
    [
      ( Protocol.name ^ ": compatible_with_reference",
        qcheck_wrap [test_compatible_with_reference] );
      ( Protocol.name ^ ": compatible_with_packing",
        qcheck_wrap [test_compatible_with_packing] );
      (Protocol.name ^ ": reflexivity", qcheck_wrap [test_reflexivity]);
      (Protocol.name ^ ": symmetry", qcheck_wrap [test_symmetry]);
      (Protocol.name ^ ": transitivity", qcheck_wrap [test_transitivity]);
      (Protocol.name ^ ": pack_unpack", qcheck_wrap [test_pack_unpack]);
    ]
