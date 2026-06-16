(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Michelson translator
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/pbt/main.exe \
                  -- --file test_script_normalization.ml
    Subject:      PBT that normalization defined in plugin is consistent with the protocol.
*)

open Protocol
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

module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (struct
  let size = 1000

  let algo = `Default
end)

module Samplers : Michelson_samplers.S =
  Michelson_samplers.Make (Parameters) (Crypto_samplers)

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
    | `TSapling_state (* Not yet supported *) | `TLambda (* Not yet supported *)
      ->
        true
  in
  let (Ex_ty ty) =
    Samplers.Random_type.m_type ~size ~blacklist () random_state
  in
  let x = Samplers.Random_value.value ty random_state in
  Ex_data (ty, x)

(* There is no particular reason not to define proper shrinkers here,
   we just haven't needed them yet. *)
let data_generator =
  QCheck2.Gen.make_primitive ~gen:ex_data_sampler ~shrink:(fun _ -> Seq.empty)

let elab_conf = Script_ir_translator_config.make ~legacy:true ()

let normalize_proto ~unparsing_mode typ ctxt e =
  let open Lwt_result_syntax in
  let* data, ctxt =
    Script_ir_translator.parse_data
      ctxt
      ~elab_conf
      ~allow_forged_tickets:true
      ~allow_forged_lazy_storage_id:true
      typ
      e
  in
  let+ normalized, _ctxt =
    Script_ir_translator.unparse_data ctxt unparsing_mode typ data
  in
  normalized

let pp_ty fmt ty =
  let ty, _ = assert_ok @@ Script_ir_unparser.unparse_ty ~loc:() ctxt ty in
  Michelson_v1_printer.print_expr fmt (Micheline.strip_locations ty)

let test_normalize =
  QCheck2.Test.make
    ~count:1000
    ~name:"normalize"
    data_generator
    (fun (Ex_data (ty, x)) ->
      let open Lwt_result_syntax in
      let unparsing_modes =
        Script_ir_unparser.[Readable; Optimized; Optimized_legacy]
      in
      let pp = Michelson_v1_printer.print_expr in
      let mode_to_string = function
        | Script_ir_unparser.Readable -> "Readable"
        | Optimized -> "Optimized"
        | Optimized_legacy -> "Legacy"
      in
      let pp_mode fmt mode = Format.pp_print_string fmt (mode_to_string mode) in

      assert_return
      @@ List.for_all_es
           (fun origin_unparsing_mode ->
             List.for_all_es
               (fun dest_unparsing_mode ->
                 let* origin, ctxt =
                   Script_ir_translator.unparse_data
                     ctxt
                     origin_unparsing_mode
                     ty
                     x
                 in
                 let e = Micheline.root origin in
                 Log.debug
                   "Origin mode: %a, Dest mode: %a@.Origin: %a@.Type: %a@."
                   pp_mode
                   origin_unparsing_mode
                   pp_mode
                   dest_unparsing_mode
                   pp
                   origin
                   pp_ty
                   ty ;
                 let normalized_plugin =
                   RPC.Scripts.Normalize_data.normalize_data
                     ~unparsing_mode:dest_unparsing_mode
                     ty
                     ctxt
                     e
                 in
                 let normalized_plugin =
                   Micheline.strip_locations normalized_plugin
                 in
                 Log.debug "Normalized (plugin): %a@." pp normalized_plugin ;
                 let+ normalized_proto =
                   normalize_proto ~unparsing_mode:dest_unparsing_mode ty ctxt e
                 in
                 Log.debug "Normalized (proto): %a@." pp normalized_proto ;
                 qcheck_eq ~pp ~__LOC__ normalized_plugin normalized_proto)
               unparsing_modes)
           unparsing_modes)

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [("normalize", qcheck_wrap [test_normalize])]
