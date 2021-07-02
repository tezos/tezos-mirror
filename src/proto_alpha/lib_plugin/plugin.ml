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

  let pre_filter config ?validation_state_before:_
      (Operation_data {contents; _} as op : Operation.packed_protocol_data) =
    let bytes =
      ( WithExceptions.Option.get ~loc:__LOC__
      @@ Data_encoding.Binary.fixed_length
           Tezos_base.Operation.shell_header_encoding )
      + Data_encoding.Binary.length Operation.protocol_data_encoding op
    in
    match contents with
    | Single (Endorsement _) ->
        false (* legacy format *)
    | Single (Failing_noop _) ->
        false
    | Single (Endorsement_with_slot _) ->
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
          Lwt.return_false (* legacy format *)
      | Single_result (Endorsement_with_slot_result _) ->
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

  module Unparse_types = struct
    (* Same as the unparsing functions for types in Script_ir_translator but
       does not consume gas and never folds (pair a (pair b c)) *)

    open Script_ir_translator
    open Micheline
    open Michelson_v1_primitives
    open Script_ir_annot
    open Script_typed_ir

    let rec unparse_comparable_ty : type a. a comparable_ty -> Script.node =
      function
      | Unit_key tname ->
          Prim (-1, T_unit, [], unparse_type_annot tname)
      | Never_key tname ->
          Prim (-1, T_never, [], unparse_type_annot tname)
      | Int_key tname ->
          Prim (-1, T_int, [], unparse_type_annot tname)
      | Nat_key tname ->
          Prim (-1, T_nat, [], unparse_type_annot tname)
      | Signature_key tname ->
          Prim (-1, T_signature, [], unparse_type_annot tname)
      | String_key tname ->
          Prim (-1, T_string, [], unparse_type_annot tname)
      | Bytes_key tname ->
          Prim (-1, T_bytes, [], unparse_type_annot tname)
      | Mutez_key tname ->
          Prim (-1, T_mutez, [], unparse_type_annot tname)
      | Bool_key tname ->
          Prim (-1, T_bool, [], unparse_type_annot tname)
      | Key_hash_key tname ->
          Prim (-1, T_key_hash, [], unparse_type_annot tname)
      | Key_key tname ->
          Prim (-1, T_key, [], unparse_type_annot tname)
      | Timestamp_key tname ->
          Prim (-1, T_timestamp, [], unparse_type_annot tname)
      | Address_key tname ->
          Prim (-1, T_address, [], unparse_type_annot tname)
      | Chain_id_key tname ->
          Prim (-1, T_chain_id, [], unparse_type_annot tname)
      | Pair_key ((l, al), (r, ar), pname) ->
          let tl = add_field_annot al None (unparse_comparable_ty l) in
          let tr = add_field_annot ar None (unparse_comparable_ty r) in
          Prim (-1, T_pair, [tl; tr], unparse_type_annot pname)
      | Union_key ((l, al), (r, ar), tname) ->
          let tl = add_field_annot al None (unparse_comparable_ty l) in
          let tr = add_field_annot ar None (unparse_comparable_ty r) in
          Prim (-1, T_or, [tl; tr], unparse_type_annot tname)
      | Option_key (t, tname) ->
          Prim
            (-1, T_option, [unparse_comparable_ty t], unparse_type_annot tname)

    (* Uncomment when rebasing on top of Baking account *)
    (* | Baker_hash_key tname ->
     *     Prim (-1, T_baker_hash, [], unparse_type_annot tname)
     * | Pvss_key_key tname ->
     *     Prim (-1, T_pvss_key, [], unparse_type_annot tname) *)

    let unparse_memo_size memo_size =
      let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
      Int (-1, z)

    let rec unparse_ty : type a. a ty -> Script.node =
     fun ty ->
      let return (name, args, annot) = Prim (-1, name, args, annot) in
      match ty with
      | Unit_t tname ->
          return (T_unit, [], unparse_type_annot tname)
      | Int_t tname ->
          return (T_int, [], unparse_type_annot tname)
      | Nat_t tname ->
          return (T_nat, [], unparse_type_annot tname)
      | Signature_t tname ->
          return (T_signature, [], unparse_type_annot tname)
      | String_t tname ->
          return (T_string, [], unparse_type_annot tname)
      | Bytes_t tname ->
          return (T_bytes, [], unparse_type_annot tname)
      | Mutez_t tname ->
          return (T_mutez, [], unparse_type_annot tname)
      | Bool_t tname ->
          return (T_bool, [], unparse_type_annot tname)
      | Key_hash_t tname ->
          return (T_key_hash, [], unparse_type_annot tname)
      | Key_t tname ->
          return (T_key, [], unparse_type_annot tname)
      | Timestamp_t tname ->
          return (T_timestamp, [], unparse_type_annot tname)
      | Address_t tname ->
          return (T_address, [], unparse_type_annot tname)
      | Operation_t tname ->
          return (T_operation, [], unparse_type_annot tname)
      | Chain_id_t tname ->
          return (T_chain_id, [], unparse_type_annot tname)
      | Never_t tname ->
          return (T_never, [], unparse_type_annot tname)
      | Bls12_381_g1_t tname ->
          return (T_bls12_381_g1, [], unparse_type_annot tname)
      | Bls12_381_g2_t tname ->
          return (T_bls12_381_g2, [], unparse_type_annot tname)
      | Bls12_381_fr_t tname ->
          return (T_bls12_381_fr, [], unparse_type_annot tname)
      | Contract_t (ut, tname) ->
          let t = unparse_ty ut in
          return (T_contract, [t], unparse_type_annot tname)
      | Pair_t ((utl, l_field, l_var), (utr, r_field, r_var), tname) ->
          let annot = unparse_type_annot tname in
          let utl = unparse_ty utl in
          let tl = add_field_annot l_field l_var utl in
          let utr = unparse_ty utr in
          let tr = add_field_annot r_field r_var utr in
          return (T_pair, [tl; tr], annot)
      | Union_t ((utl, l_field), (utr, r_field), tname) ->
          let annot = unparse_type_annot tname in
          let utl = unparse_ty utl in
          let tl = add_field_annot l_field None utl in
          let utr = unparse_ty utr in
          let tr = add_field_annot r_field None utr in
          return (T_or, [tl; tr], annot)
      | Lambda_t (uta, utr, tname) ->
          let ta = unparse_ty uta in
          let tr = unparse_ty utr in
          return (T_lambda, [ta; tr], unparse_type_annot tname)
      | Option_t (ut, tname) ->
          let annot = unparse_type_annot tname in
          let ut = unparse_ty ut in
          return (T_option, [ut], annot)
      | List_t (ut, tname) ->
          let t = unparse_ty ut in
          return (T_list, [t], unparse_type_annot tname)
      | Ticket_t (ut, tname) ->
          let t = unparse_comparable_ty ut in
          return (T_ticket, [t], unparse_type_annot tname)
      | Set_t (ut, tname) ->
          let t = unparse_comparable_ty ut in
          return (T_set, [t], unparse_type_annot tname)
      | Map_t (uta, utr, tname) ->
          let ta = unparse_comparable_ty uta in
          let tr = unparse_ty utr in
          return (T_map, [ta; tr], unparse_type_annot tname)
      | Big_map_t (uta, utr, tname) ->
          let ta = unparse_comparable_ty uta in
          let tr = unparse_ty utr in
          return (T_big_map, [ta; tr], unparse_type_annot tname)
      | Sapling_transaction_t (memo_size, tname) ->
          return
            ( T_sapling_transaction,
              [unparse_memo_size memo_size],
              unparse_type_annot tname )
      | Sapling_state_t (memo_size, tname) ->
          return
            ( T_sapling_state,
              [unparse_memo_size memo_size],
              unparse_type_annot tname )

    (* Uncomment when rebasing on top of Baking account *)
    (* | Baker_hash_t tname ->
     *     return (T_baker_hash, [], unparse_type_annot tname)
     * | Pvss_key_t tname ->
     *     return (T_pvss_key, [], unparse_type_annot tname)
     * | Baker_operation_t tname ->
     *     return (T_baker_operation, [], unparse_type_annot tname) *)
  end

  let helpers_path = RPC_path.(open_root / "helpers" / "scripts")

  let normalize_type =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Normalizes some Michelson type by expanding `pair a b c` as `pair a \
         (pair b c)"
      ~input:(obj1 (req "type" Script.expr_encoding))
      ~output:(obj1 (req "normalized" Script.expr_encoding))
      ~query:RPC_query.empty
      RPC_path.(helpers_path / "normalize_type")

  let rpc_services =
    let patched_services =
      ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)
    in
    let register0_fullctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)
    in
    let register0 s f = register0_fullctxt s (fun {context; _} -> f context) in
    register0 normalize_type (fun ctxt () typ ->
        let open Script_ir_translator in
        let ctxt = Gas.set_unlimited ctxt in
        (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
        Script_ir_translator.parse_ty
          ctxt
          ~legacy:true
          ~allow_lazy_storage:true
          ~allow_operation:true
          ~allow_contract:true
          ~allow_ticket:true
          (Micheline.root typ)
        >>?= fun (Ex_ty typ, _ctxt) ->
        let normalized = Unparse_types.unparse_ty typ in
        return @@ Micheline.strip_locations normalized) ;
    RPC_directory.merge rpc_services !patched_services

  let normalize_type ctxt block ~ty =
    RPC_context.make_call0 normalize_type ctxt block () ty
end
