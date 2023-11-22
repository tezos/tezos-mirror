(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Michelson translator and ir_size
    Invocation:   dune exec src/proto_018_Proxford/lib_protocol/test/pbt/main.exe \
                  -- --file test_script_roundtrip.ml
    Subject:      PBT of the roundrip property of Michelson storages.
*)

open Protocol
open Alpha_context
open Script_typed_ir
open Qcheck2_helpers

(* Generation of a type and value of that type. *)

type ex_data = Ex_data : ('a, _) ty * 'a -> ex_data

(* We use the Michelson samplers from lib_benchmark. They are later turned into
   QCheck2 generators (see [data_generator]). *)
module Parameters = struct
  let atom_size_range : Tezos_benchmark.Base_samplers.range =
    {min = 0; max = 10}

  let other_size : Tezos_benchmark.Base_samplers.range = {min = 0; max = 2}
  (* Anything larger than max=2 leads to rare very large values which
     cannot be packed within Data_encoding limits. *)

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
Tezos_benchmark.Crypto_samplers.Make_finite_key_pool (struct
  let size = 1000

  let algo = `Default
end)

module Samplers : Michelson_samplers.S =
  Michelson_samplers.Make (Parameters) (Crypto_samplers)

let assert_some = function Some x -> x | None -> assert false

let assert_ok = function Ok x -> x | Error _ -> assert false

let assert_return x = assert_ok (Lwt_main.run x)

(* We need a context because we test carbonated functions. But since
   we don't care about gas consumption here we use the same value of
   type context everywhere instead of threading it through the error
   monad. *)

let ctxt =
  assert_return
    (let open Lwt_result_syntax in
    let* b, _cs = Context.init3 () in
    let* v = Incremental.begin_construction b in
    return (Incremental.alpha_ctxt v))

let ex_data_sampler : ex_data Tezos_benchmark.Base_samplers.sampler =
 fun random_state ->
  let size =
    Tezos_benchmark.Base_samplers.sample_in_interval
      ~range:{min = 1; max = 20}
      random_state
  in
  let blacklist = function
    | `TUnit | `TInt | `TNat | `TSignature | `TString | `TBytes | `TMutez
    | `TKey_hash | `TKey | `TTimestamp | `TAddress | `TBool | `TPair | `TOr
    | `TOption | `TList | `TSet | `TMap | `TChain_id | `TBls12_381_g1
    | `TBls12_381_g2 | `TBls12_381_fr | `TBig_map | `TTicket ->
        false
    | `TOperation (* Forbidden in storage *)
    | `TContract (* Forbidden in storage *)
    | `TSapling_transaction (* Not yet supported *)
    | `TSapling_transaction_deprecated (* Not yet supported *)
    | `TSapling_state (* Not yet supported *)
    | `TLambda (* Not yet supported *) ->
        true
  in
  let (Ex_ty ty) =
    Samplers.Random_type.m_type ~size ~blacklist () random_state
  in
  let x = Samplers.Random_value.value ty random_state in
  Ex_data (ty, x)

let big_map_data_sampler : ex_data Tezos_benchmark.Base_samplers.sampler =
 fun random_state ->
  let size =
    Tezos_benchmark.Base_samplers.sample_in_interval
      ~range:{min = 1; max = 20}
      random_state
  in
  let (Ex_comparable_ty kty) =
    Samplers.Random_type.m_comparable_type ~size random_state
  in
  let (Ex_comparable_ty vty) =
    Samplers.Random_type.m_comparable_type ~size random_state
  in
  let ty = assert_ok @@ big_map_t 0 kty vty in
  let x = Samplers.Random_value.value ty random_state in
  Ex_data (ty, x)

(* There is no particular reason not to define proper shrinkers here,
   we just haven't needed them yet. *)
let data_generator =
  QCheck2.Gen.make_primitive ~gen:ex_data_sampler ~shrink:(fun _ -> Seq.empty)

let big_map_data_generator =
  QCheck2.Gen.make_primitive ~gen:ex_data_sampler ~shrink:(fun _ -> Seq.empty)

let dummy_code unparsed_ty =
  let open Micheline in
  let open Michelson_v1_primitives in
  strip_locations
  @@ Seq
       ( (),
         [
           Prim ((), K_parameter, [Prim ((), T_never, [], [])], []);
           Prim ((), K_storage, [unparsed_ty], []);
           Prim
             ( (),
               K_code,
               [
                 Seq ((), [Prim ((), I_CAR, [], []); Prim ((), I_NEVER, [], [])]);
               ],
               [] );
         ] )

let roundtrip (ty : ('a, 'ac) ty) (x : 'a) lazy_storage_diff ctxt =
  let open Lwt_result_syntax in
  let dummy_address = Contract_hash.zero in
  let* storage, ctxt = Script_ir_translator.unparse_data ctxt Optimized ty x in
  let*? storage_type, ctxt = Script_ir_unparser.unparse_ty ~loc:() ctxt ty in
  let storage = Script.lazy_expr storage in
  let* code, ctxt =
    Script_ir_translator.unparse_code
      ctxt
      Optimized
      (Micheline.root (dummy_code storage_type))
  in
  let code = Script.lazy_expr code in
  let script = {Script.code; storage} in
  let* ctxt =
    Contract.raw_originate
      ctxt
      ~prepaid_bootstrap_storage:false
      dummy_address
      ~script:(script, lazy_storage_diff)
  in
  let* ctxt, script = Contract.get_script ctxt dummy_address in
  let script = assert_some script in
  let elab_conf = Script_ir_translator_config.make ~legacy:true () in
  let allow_forged_in_storage = true in
  let* Ex_script (Script {storage_type; storage; _}), ctxt =
    Script_ir_translator.parse_script
      ctxt
      script
      ~elab_conf
      ~allow_forged_in_storage
  in
  let*? eq, _ctxt =
    Gas_monad.run ctxt
    @@ Script_ir_translator.ty_eq ~error_details:(Informative 0) ty storage_type
  in
  let safe_cast (type a ac b bc) (y : b)
      (eq : ((a, ac) ty, (b, bc) ty) Script_ir_translator.eq) : a =
    let Eq = eq in
    y
  in
  let*? eq in
  return @@ safe_cast storage eq

let unparse_ty ty =
  Micheline.strip_locations @@ fst @@ assert_ok
  @@ Script_ir_unparser.(unparse_ty ~loc:() ctxt ty)

let unparse_data ty x =
  fst (assert_return Script_ir_translator.(unparse_data ctxt Readable ty x))

let value_size ty x = Script_typed_ir_size.value_size ty x

let pp_ty fmt ty = Michelson_v1_printer.print_expr fmt (unparse_ty ty)

let pp_data ty fmt x = Michelson_v1_printer.print_expr fmt (unparse_data ty x)

let pp_data_with_size ty fmt x =
  let nodes, size = value_size ty x in
  Format.fprintf
    fmt
    "(%a, ty=%a, nodes=%d, size=%d)"
    (pp_data ty)
    x
    pp_ty
    ty
    (Cache_memory_helpers.Nodes.to_int nodes)
    (Saturation_repr.to_int size)

let size_compare (nodes1, size1) (nodes2, size2) =
  compare
    (Cache_memory_helpers.Nodes.to_int nodes1, Saturation_repr.to_int size1)
    (Cache_memory_helpers.Nodes.to_int nodes2, Saturation_repr.to_int size2)

let value_size_compare ty x1 x2 =
  size_compare (value_size ty x1) (value_size ty x2)

let extract_lazy_storage_diff (ty : ('a, 'ac) ty) (x : 'a) ctxt =
  let open Lwt_result_syntax in
  let*? to_duplicate, ctxt =
    Script_ir_translator.collect_lazy_storage ctxt ty x
  in
  Script_ir_translator.extract_lazy_storage_diff
    ctxt
    Optimized
    ~temporary:false
    ~to_duplicate
    ~to_update:Script_ir_translator.no_lazy_storage_id
    ty
    x

(* Test.
 * Tests that round-tripping a contract storage through the context does not
 * alter its size.
 *)
let test_roundtrip_size =
  QCheck2.Test.make
    ~count:100
    ~name:"roundtrip_size"
    data_generator
    (fun (Ex_data (ty, x)) ->
      let x, lazy_storage_diff, _ctxt =
        assert_return @@ extract_lazy_storage_diff ty x ctxt
      in
      qcheck_eq
        ~cmp:(value_size_compare ty)
        ~pp:(pp_data_with_size ty)
        x
        (assert_return @@ roundtrip ty x lazy_storage_diff ctxt))

(* Same but on big maps.
 *)
let test_roundtrip_size_big_map =
  QCheck2.Test.make
    ~count:100
    ~name:"roundtrip_size_big_map"
    big_map_data_generator
    (fun (Ex_data (ty, x)) ->
      let x, lazy_storage_diff, _ctxt =
        assert_return @@ extract_lazy_storage_diff ty x ctxt
      in
      qcheck_eq
        ~cmp:(value_size_compare ty)
        ~pp:(pp_data_with_size ty)
        x
        (assert_return @@ roundtrip ty x lazy_storage_diff ctxt))

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [
      ("roundtrip_size", qcheck_wrap [test_roundtrip_size]);
      ("roundtrip_size_big_map", qcheck_wrap [test_roundtrip_size_big_map]);
    ]
