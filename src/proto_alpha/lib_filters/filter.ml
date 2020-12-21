(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

open Protocol
open Alpha_context
module Proto = Registerer.Registered

module Mempool = struct
  type nanotez = Q.t

  let nanotez_enc : nanotez Data_encoding.t =
    let open Data_encoding in
    def
      "nanotez"
      ~title:"A thousandth of a mutez"
      ~description:"One thousand nanotez make a mutez (1 tez = 1e9 nanotez)"
      (conv
         (fun q -> (q.Q.num, q.Q.den))
         (fun (num, den) -> {Q.num; den})
         (tup2 z z))

  type config = {
    minimal_fees : Tez.t;
    minimal_nanotez_per_gas_unit : nanotez;
    minimal_nanotez_per_byte : nanotez;
    allow_script_failure : bool;
  }

  let default_minimal_fees =
    match Tez.of_mutez 100L with None -> assert false | Some t -> t

  let default_minimal_nanotez_per_gas_unit = Q.of_int 100

  let default_minimal_nanotez_per_byte = Q.of_int 1000

  let config_encoding : config Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { minimal_fees;
             minimal_nanotez_per_gas_unit;
             minimal_nanotez_per_byte;
             allow_script_failure } ->
        ( minimal_fees,
          minimal_nanotez_per_gas_unit,
          minimal_nanotez_per_byte,
          allow_script_failure ))
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             allow_script_failure ) ->
        {
          minimal_fees;
          minimal_nanotez_per_gas_unit;
          minimal_nanotez_per_byte;
          allow_script_failure;
        })
      (obj4
         (dft "minimal_fees" Tez.encoding default_minimal_fees)
         (dft
            "minimal_nanotez_per_gas_unit"
            nanotez_enc
            default_minimal_nanotez_per_gas_unit)
         (dft
            "minimal_nanotez_per_byte"
            nanotez_enc
            default_minimal_nanotez_per_byte)
         (dft "allow_script_failure" bool true))

  let default_config =
    {
      minimal_fees = default_minimal_fees;
      minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
      allow_script_failure = true;
    }

  let get_manager_operation_gas_and_fee contents =
    let open Operation in
    let l = to_list (Contents_list contents) in
    List.fold_left
      (fun acc -> function
        | Contents (Manager_operation {fee; gas_limit; _}) -> (
          match acc with
          | Error _ as e ->
              e
          | Ok (total_fee, total_gas) -> (
            match Tez.(total_fee +? fee) with
            | Ok total_fee ->
                Ok (total_fee, Gas.Arith.add total_gas gas_limit)
            | Error _ as e ->
                e ) ) | _ -> acc)
      (Ok (Tez.zero, Gas.Arith.zero))
      l

  let pre_filter_manager :
      type t. config -> t Kind.manager contents_list -> int -> bool =
   fun config op size ->
    match get_manager_operation_gas_and_fee op with
    | Error _ ->
        false
    | Ok (fee, gas) ->
        let fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
        in
        let minimal_fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez config.minimal_fees)) (Q.of_int 1000)
        in
        let minimal_fees_for_gas_in_nanotez =
          Q.mul
            config.minimal_nanotez_per_gas_unit
            (Q.of_bigint @@ Gas.Arith.integral_to_z gas)
        in
        let minimal_fees_for_size_in_nanotez =
          Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
        in
        Q.compare
          fees_in_nanotez
          (Q.add
             minimal_fees_in_nanotez
             (Q.add
                minimal_fees_for_gas_in_nanotez
                minimal_fees_for_size_in_nanotez))
        >= 0

  let pre_filter config
      (Operation_data {contents; _} as op : Operation.packed_protocol_data) =
    let bytes =
      Data_encoding.Binary.fixed_length_exn
        Tezos_base.Operation.shell_header_encoding
      + Data_encoding.Binary.length Operation.protocol_data_encoding op
    in
    match contents with
    | Single (Endorsement _) ->
        true
    | Single (Seed_nonce_revelation _) ->
        true
    | Single (Double_endorsement_evidence _) ->
        true
    | Single (Double_baking_evidence _) ->
        true
    | Single (Activate_account _) ->
        true
    | Single (Proposals _) ->
        true
    | Single (Ballot _) ->
        true
    | Single (Manager_operation _) as op ->
        pre_filter_manager config op bytes
    | Cons (Manager_operation _, _) as op ->
        pre_filter_manager config op bytes

  open Apply_results

  let rec post_filter_manager :
      type t.
      Alpha_context.t ->
      t Kind.manager contents_result_list ->
      config ->
      bool Lwt.t =
   fun ctxt op config ->
    match op with
    | Single_result (Manager_operation_result {operation_result; _}) -> (
      match operation_result with
      | Applied _ ->
          Lwt.return_true
      | Skipped _ | Failed _ | Backtracked _ ->
          Lwt.return config.allow_script_failure )
    | Cons_result (Manager_operation_result res, rest) -> (
        post_filter_manager
          ctxt
          (Single_result (Manager_operation_result res))
          config
        >>= function
        | false ->
            Lwt.return_false
        | true ->
            post_filter_manager ctxt rest config )

  let post_filter config ~validation_state_before:_
      ~validation_state_after:({ctxt; _} : validation_state) (_op, receipt) =
    match receipt with
    | No_operation_metadata ->
        assert false (* only for multipass validator *)
    | Operation_metadata {contents} -> (
      match contents with
      | Single_result (Endorsement_result _) ->
          Lwt.return_true
      | Single_result (Seed_nonce_revelation_result _) ->
          Lwt.return_true
      | Single_result (Double_endorsement_evidence_result _) ->
          Lwt.return_true
      | Single_result (Double_baking_evidence_result _) ->
          Lwt.return_true
      | Single_result (Activate_account_result _) ->
          Lwt.return_true
      | Single_result Proposals_result ->
          Lwt.return_true
      | Single_result Ballot_result ->
          Lwt.return_true
      | Single_result (Manager_operation_result _) as op ->
          post_filter_manager ctxt op config
      | Cons_result (Manager_operation_result _, _) as op ->
          post_filter_manager ctxt op config )
end

module RPC = struct
  open Environment

  let path = RPC_path.(open_root / "helpers")

  let unparsing_mode_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [ case
          (Tag 0)
          ~title:"Readable"
          (constant "Readable")
          (function
            | Script_ir_translator.Readable ->
                Some ()
            | Script_ir_translator.Optimized
            | Script_ir_translator.Optimized_legacy ->
                None)
          (fun () -> Script_ir_translator.Readable);
        case
          (Tag 1)
          ~title:"Optimized"
          (constant "Optimized")
          (function
            | Script_ir_translator.Optimized ->
                Some ()
            | Script_ir_translator.Readable
            | Script_ir_translator.Optimized_legacy ->
                None)
          (fun () -> Script_ir_translator.Optimized);
        case
          (Tag 2)
          ~title:"Optimized_legacy"
          (constant "Optimized_legacy")
          (function
            | Script_ir_translator.Optimized_legacy ->
                Some ()
            | Script_ir_translator.Readable | Script_ir_translator.Optimized ->
                None)
          (fun () -> Script_ir_translator.Optimized_legacy) ]

  let normalize_data =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Normalizes some data expression using the requested unparsing mode"
      ~input:
        (obj4
           (req "data" Script.expr_encoding)
           (req "type" Script.expr_encoding)
           (req "unparsing_mode" unparsing_mode_encoding)
           (opt "legacy" bool))
      ~output:(obj1 (req "normalized" Script.expr_encoding))
      ~query:RPC_query.empty
      RPC_path.(path / "normalize_data")

  let rpc_services =
    let patched_services =
      let register0_fullctxt s f =
        RPC_directory.register RPC_directory.empty s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)
      in
      let register0 s f =
        register0_fullctxt s (fun {context; _} -> f context)
      in
      register0
        normalize_data
        (fun ctxt () (expr, typ, unparsing_mode, legacy) ->
          let open Script_ir_translator in
          let legacy = Option.value ~default:false legacy in
          let ctxt = Gas.set_unlimited ctxt in
          (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
          Script_ir_translator.parse_ty
            ctxt
            ~legacy
            ~allow_lazy_storage:true
            ~allow_operation:true
            ~allow_contract:true
            ~allow_ticket:true
            (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data ctxt ~legacy ~allow_forged:true typ (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.unparse_data ctxt unparsing_mode typ data
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized)
    in
    RPC_directory.merge rpc_services patched_services

  let normalize_data ctxt block ?legacy ~data ~ty ~unparsing_mode =
    RPC_context.make_call0
      normalize_data
      ctxt
      block
      ()
      (data, ty, unparsing_mode, legacy)
end
