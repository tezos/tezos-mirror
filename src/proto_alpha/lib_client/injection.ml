(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
open Apply_results
open Apply_operation_result
open Apply_internal_results
open Protocol_client_context

let get_branch (rpc_config : #Protocol_client_context.full) ~chain
    ~(block : Block_services.block) branch =
  (* The default branch is set to HEAD~2, because with Tenderbake the
     same transaction may be included again in another block candidate
     at the same level, so the operation branch should not point to
     the current head. It's not a good idea if it points to the head's
     predecessor as well, as the predecessor hash may still change
     because of potential reorgs (only the predecessor payload is
     finalized, not the whole block). *)
  let open Lwt_result_syntax in
  let branch = Option.value ~default:0 branch in
  let* block =
    (* TODO export parameter *)
    match block with
    | `Head 0 ->
        (* Default client's block value: we branch to head's grandfather *)
        return (`Head (2 + branch))
    | `Head n -> return (`Head (n + branch))
    | `Hash (h, n) -> return (`Hash (h, n + branch))
    | `Alias (a, n) -> return (`Alias (a, n))
    | `Genesis -> return `Genesis
    | `Level i -> return (`Level i)
  in
  let* hash = Shell_services.Blocks.hash rpc_config ~chain ~block () in
  let* chain_id = Shell_services.Chain.chain_id rpc_config ~chain () in
  return (chain_id, hash)

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

type 'kind result = Operation_hash.t * 'kind contents * 'kind contents_result

let get_manager_operation_gas_and_fee (contents : packed_contents_list) =
  let l = Operation.to_list contents in
  List.fold_left
    (fun acc -> function
      | Contents (Manager_operation {fee; gas_limit; _}) -> (
          match acc with
          | Error _ as e -> e
          | Ok (total_fee, total_gas) -> (
              match Tez.(total_fee +? fee) with
              | Ok total_fee -> Ok (total_fee, Gas.Arith.add total_gas gas_limit)
              | Error _ as e -> e))
      | _ -> acc)
    (Ok (Tez.zero, Gas.Arith.zero))
    l

type fee_parameter = {
  minimal_fees : Tez.t;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : Tez.t;
  burn_cap : Tez.t;
}

(* Rounding up (see Z.cdiv) *)
let z_mutez_of_q_nanotez (ntz : Q.t) =
  let q_mutez = Q.div ntz (Q.of_int 1000) in
  Z.cdiv q_mutez.Q.num q_mutez.Q.den

let check_fees :
    type t.
    #Protocol_client_context.full ->
    fee_parameter ->
    t contents_list ->
    int ->
    unit Lwt.t =
  let open Lwt_result_syntax in
  fun cctxt config op size ->
    match Contents_list op |> get_manager_operation_gas_and_fee with
    | Error _ -> assert false (* FIXME *)
    | Ok (fee, gas) ->
        if Tez.compare fee config.fee_cap > 0 then
          let*! () =
            cctxt#error
              "The proposed fee (%s%a) are higher than the configured fee cap \
               (%s%a).@\n\
              \ Use `--fee-cap %a` to emit this operation anyway."
              Operation_result.tez_sym
              Tez.pp
              fee
              Operation_result.tez_sym
              Tez.pp
              config.fee_cap
              Tez.pp
              fee
          in
          exit 1
        else
          let fees_in_nanotez =
            Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
          in
          let minimal_fees_in_nanotez =
            Q.mul
              (Q.of_int64 (Tez.to_mutez config.minimal_fees))
              (Q.of_int 1000)
          in
          let minimal_fees_for_gas_in_nanotez =
            Q.mul
              config.minimal_nanotez_per_gas_unit
              (Q.of_bigint (Gas.Arith.integral_to_z gas))
          in
          let minimal_fees_for_size_in_nanotez =
            Q.mul config.minimal_nanotez_per_byte (Q.of_int size)
          in
          let estimated_fees_in_nanotez =
            Q.add
              minimal_fees_in_nanotez
              (Q.add
                 minimal_fees_for_gas_in_nanotez
                 minimal_fees_for_size_in_nanotez)
          in
          let estimated_fees_in_mutez =
            z_mutez_of_q_nanotez estimated_fees_in_nanotez
          in
          let estimated_fees =
            match Tez.of_mutez (Z.to_int64 estimated_fees_in_mutez) with
            | None -> assert false
            | Some fee -> fee
          in
          if
            (not config.force_low_fee)
            && Q.compare fees_in_nanotez estimated_fees_in_nanotez < 0
          then
            let*! () =
              cctxt#error
                "The proposed fee (%s%a) are lower than the fee that baker \
                 expect by default (%s%a).@\n\
                \ Use `--force-low-fee` to emit this operation anyway."
                Operation_result.tez_sym
                Tez.pp
                fee
                Operation_result.tez_sym
                Tez.pp
                estimated_fees
            in
            exit 1
          else Lwt.return_unit

let print_for_verbose_signing ppf ~watermark ~bytes ~branch ~contents =
  let open Format in
  pp_open_vbox ppf 0 ;
  let item f =
    pp_open_hovbox ppf 4 ;
    pp_print_string ppf "  * " ;
    f ppf () ;
    pp_close_box ppf () ;
    pp_print_cut ppf ()
  in
  let hash_pp l =
    fprintf
      ppf
      "%s"
      (Tezos_crypto.Base58.raw_encode
         Tezos_crypto.Blake2B.(hash_bytes l |> to_string))
  in
  item (fun ppf () ->
      pp_print_text ppf "Branch: " ;
      Block_hash.pp ppf branch) ;
  item (fun ppf () ->
      fprintf
        ppf
        "Watermark: `%a` (0x%s)"
        Signature.pp_watermark
        watermark
        (Hex.of_bytes (Signature.bytes_of_watermark watermark) |> Hex.show)) ;
  item (fun ppf () ->
      pp_print_text ppf "Operation bytes: " ;
      TzString.fold_left (* We split the bytes into lines for display: *)
        (fun n c ->
          pp_print_char ppf c ;
          if
            n < 72
            (* is the email-body standard width, ideal for copy-pasting. *)
          then n + 1
          else (
            pp_print_space ppf () ;
            0))
        0
        (Hex.of_bytes bytes |> Hex.show)
      |> ignore) ;
  item (fun ppf () ->
      pp_print_text ppf "Blake 2B Hash (raw): " ;
      hash_pp [bytes]) ;
  item (fun ppf () ->
      pp_print_text
        ppf
        "Blake 2B Hash (ledger-style, with operation watermark): " ;
      hash_pp [Signature.bytes_of_watermark watermark; bytes]) ;
  let json =
    Data_encoding.Json.construct
      Operation.unsigned_encoding_with_legacy_attestation_name
      ({branch}, Contents_list contents)
  in
  item (fun ppf () ->
      pp_print_text ppf "JSON encoding: " ;
      Data_encoding.Json.pp ppf json) ;
  pp_close_box ppf ()

let preapply (type t) (cctxt : #Protocol_client_context.full) ~chain ~block
    ?(verbose_signing = false) ?fee_parameter ?branch ?src_sk
    (contents : t contents_list) =
  let open Lwt_result_syntax in
  let* _chain_id, branch = get_branch cctxt ~chain ~block branch in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  let* signature =
    match src_sk with
    | None -> return_none
    | Some src_sk ->
        let watermark =
          match contents with
          (* TODO-TB sign endorsement? *)
          | _ -> Signature.Generic_operation
        in
        let*! () =
          if verbose_signing then
            cctxt#message
              "Pre-signature information (verbose signing):@.%t%!"
              (print_for_verbose_signing ~watermark ~bytes ~branch ~contents)
          else Lwt.return_unit
        in
        let* signature = Client_keys.sign cctxt ~watermark src_sk bytes in
        return_some signature
  in
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature}}
  in
  let oph = Operation.hash op in
  let packed_op =
    {shell = {branch}; protocol_data = Operation_data {contents; signature}}
  in
  let size = Data_encoding.Binary.length Operation.encoding packed_op in
  let*! () =
    match fee_parameter with
    | Some fee_parameter -> check_fees cctxt fee_parameter contents size
    | None -> Lwt.return_unit
  in
  let* operations_opt =
    Protocol_client_context.Alpha_block_services.Helpers.Preapply.operations
      cctxt
      ~chain
      ~block
      [Operation.pack op]
  in
  match operations_opt with
  | [(Operation_data op', Operation_metadata result)] -> (
      match
        ( Operation.equal op {shell = {branch}; protocol_data = op'},
          Apply_results.kind_equal_list contents result.contents )
      with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result")
  | _ -> failwith "Unexpected result"

let simulate (type t) (cctxt : #Protocol_client_context.full) ~chain ~block
    ?(successor_level = false) ?branch
    ?(latency = Plugin.RPC.default_operation_inclusion_latency)
    (contents : t contents_list) =
  let open Lwt_result_syntax in
  let* _chain_id, branch = get_branch cctxt ~chain ~block branch in
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature = None}}
  in
  let oph = Operation.hash op in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let* operations_opt =
    Plugin.RPC.Scripts.simulate_operation
      cctxt
      (chain, block)
      ~successor_level
      ~op:(Operation.pack op)
      ~chain_id
      ~latency
  in
  match operations_opt with
  | Operation_data op', Operation_metadata result -> (
      match
        ( Operation.equal op {shell = {branch}; protocol_data = op'},
          Apply_results.kind_equal_list contents result.contents )
      with
      | Some Operation.Eq, Some Apply_results.Eq ->
          return ((oph, op, result) : t preapply_result)
      | _ -> failwith "Unexpected result")
  | _ -> failwith "Unexpected result"

let estimated_gas_single (type kind)
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let open Result_syntax in
  let consumed_gas (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | Transaction_result
            ( Transaction_to_contract_result {consumed_gas; _}
            | Transaction_to_sc_rollup_result {consumed_gas; _}
            | Transaction_to_zk_rollup_result {consumed_gas; _} )
        | Origination_result {consumed_gas; _}
        | Reveal_result {consumed_gas}
        | Delegation_result {consumed_gas; _}
        | Register_global_constant_result {consumed_gas; _}
        | Set_deposits_limit_result {consumed_gas}
        | Update_consensus_key_result {consumed_gas; _}
        | Increase_paid_storage_result {consumed_gas; _}
        | Transfer_ticket_result {consumed_gas; _}
        | Dal_publish_slot_header_result {consumed_gas; _}
        | Sc_rollup_originate_result {consumed_gas; _}
        | Sc_rollup_add_messages_result {consumed_gas; _}
        | Sc_rollup_cement_result {consumed_gas; _}
        | Sc_rollup_publish_result {consumed_gas; _}
        | Sc_rollup_refute_result {consumed_gas; _}
        | Sc_rollup_timeout_result {consumed_gas; _}
        | Sc_rollup_execute_outbox_message_result {consumed_gas; _}
        | Sc_rollup_recover_bond_result {consumed_gas; _} ->
            Ok consumed_gas
        | Zk_rollup_origination_result {consumed_gas; _} -> Ok consumed_gas
        | Zk_rollup_publish_result {consumed_gas; _} -> Ok consumed_gas
        | Zk_rollup_update_result {consumed_gas; _} -> Ok consumed_gas)
    | Skipped _ ->
        error_with "Cannot estimate gas of skipped operation"
        (* There must be another error for this to happen, and it should not
           surface (the force mode catches it). *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let internal_consumed_gas (type kind)
      (result : kind internal_operation_result) =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | ITransaction_result
            ( Transaction_to_contract_result {consumed_gas; _}
            | Transaction_to_sc_rollup_result {consumed_gas; _}
            | Transaction_to_zk_rollup_result {consumed_gas; _} )
        | IOrigination_result {consumed_gas; _}
        | IDelegation_result {consumed_gas; _}
        | IEvent_result {consumed_gas} ->
            Ok consumed_gas)
    | Skipped _ ->
        Ok Gas.Arith.zero (* there must be another error for this to happen *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let* gas = consumed_gas operation_result in
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      let* gas = internal_consumed_gas r in
      Ok (Gas.Arith.add acc gas))
    gas
    internal_operation_results

let estimated_storage_single (type kind) ~origination_size
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let open Result_syntax in
  let storage_size_diff (type kind) (result : kind manager_operation_result) =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | Transaction_result
            (Transaction_to_contract_result
              {paid_storage_size_diff; allocated_destination_contract; _}) ->
            if allocated_destination_contract then
              Ok (Z.add paid_storage_size_diff origination_size)
            else Ok paid_storage_size_diff
        | Origination_result {paid_storage_size_diff; _} ->
            Ok (Z.add paid_storage_size_diff origination_size)
        | Register_global_constant_result {size_of_constant; _} ->
            Ok size_of_constant
        | Update_consensus_key_result _ -> Ok Z.zero
        | Sc_rollup_execute_outbox_message_result {paid_storage_size_diff; _}
        | Transfer_ticket_result {paid_storage_size_diff; _}
        | Zk_rollup_publish_result {paid_storage_size_diff; _}
        | Zk_rollup_update_result {paid_storage_size_diff; _}
        | Transaction_result
            (Transaction_to_zk_rollup_result {paid_storage_size_diff; _}) ->
            Ok paid_storage_size_diff
        | Sc_rollup_originate_result {size; _} -> Ok size
        | Zk_rollup_origination_result {storage_size; _} -> Ok storage_size
        | Transaction_result (Transaction_to_sc_rollup_result _)
        | Reveal_result _ | Delegation_result _ | Set_deposits_limit_result _
        | Increase_paid_storage_result _ | Dal_publish_slot_header_result _
        | Sc_rollup_add_messages_result _
        (* The following Sc_rollup operations have zero storage cost because we
           consider them to be paid in the stake deposit.

           TODO: https://gitlab.com/tezos/tezos/-/issues/2686
           Document why this is safe.
        *)
        | Sc_rollup_cement_result _ | Sc_rollup_publish_result _
        | Sc_rollup_refute_result _ | Sc_rollup_timeout_result _
        | Sc_rollup_recover_bond_result _ ->
            Ok Z.zero)
    | Skipped _ ->
        error_with "Cannot estimate storage of skipped operation"
        (* There must be another error for this to happen, and it should not
           surface (the force mode catches it). *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let internal_storage_size_diff (type kind)
      (result : kind internal_operation_result) =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | ITransaction_result
            (Transaction_to_contract_result
              {paid_storage_size_diff; allocated_destination_contract; _}) ->
            if allocated_destination_contract then
              Ok (Z.add paid_storage_size_diff origination_size)
            else Ok paid_storage_size_diff
        | IOrigination_result {paid_storage_size_diff; _} ->
            Ok (Z.add paid_storage_size_diff origination_size)
        | ITransaction_result
            (Transaction_to_zk_rollup_result {paid_storage_size_diff; _}) ->
            Ok paid_storage_size_diff
        | ITransaction_result (Transaction_to_sc_rollup_result _)
        | IDelegation_result _ | IEvent_result _ ->
            Ok Z.zero)
    | Skipped _ ->
        Ok Z.zero (* there must be another error for this to happen *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let* storage = storage_size_diff operation_result in
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      let* storage = internal_storage_size_diff r in
      Ok (Z.add acc storage))
    storage
    internal_operation_results

let estimated_storage ~origination_size res =
  let open Result_syntax in
  let rec estimated_storage : type kind. kind contents_result_list -> _ =
    function
    | Single_result (Manager_operation_result _ as res) ->
        estimated_storage_single ~origination_size res
    | Single_result _ -> Ok Z.zero
    | Cons_result (res, rest) ->
        let* storage1 = estimated_storage_single ~origination_size res in
        let* storage2 = estimated_storage rest in
        Ok (Z.add storage1 storage2)
  in
  let* diff = estimated_storage res in
  Ok (Z.max Z.zero diff)

let originated_contracts_single (type kind)
    (Manager_operation_result {operation_result; internal_operation_results; _} :
      kind Kind.manager contents_result) =
  let open Result_syntax in
  let originated_contracts (type kind) (result : kind manager_operation_result)
      =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | Transaction_result
            (Transaction_to_contract_result {originated_contracts; _})
        | Origination_result {originated_contracts; _} ->
            Ok originated_contracts
        | Transaction_result
            ( Transaction_to_sc_rollup_result _
            | Transaction_to_zk_rollup_result _ )
        | Register_global_constant_result _ | Reveal_result _
        | Delegation_result _ | Set_deposits_limit_result _
        | Update_consensus_key_result _ | Increase_paid_storage_result _
        | Transfer_ticket_result _ | Dal_publish_slot_header_result _
        | Sc_rollup_originate_result _ | Sc_rollup_add_messages_result _
        | Sc_rollup_cement_result _ | Sc_rollup_publish_result _
        | Sc_rollup_refute_result _ | Sc_rollup_timeout_result _
        | Sc_rollup_execute_outbox_message_result _
        | Sc_rollup_recover_bond_result _ | Zk_rollup_origination_result _
        | Zk_rollup_publish_result _ | Zk_rollup_update_result _ ->
            return_nil)
    | Skipped _ ->
        error_with "Cannot know originated contracts of skipped operation"
        (* There must be another error for this to happen, and it should not
           surface (the force mode catches it). *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let internal_originated_contracts (type kind)
      (result : kind internal_operation_result) =
    match result with
    | Applied res | Backtracked (res, _) -> (
        match res with
        | ITransaction_result
            (Transaction_to_contract_result {originated_contracts; _})
        | IOrigination_result {originated_contracts; _} ->
            Ok originated_contracts
        | ITransaction_result
            ( Transaction_to_sc_rollup_result _
            | Transaction_to_zk_rollup_result _ )
        | IDelegation_result _ | IEvent_result _ ->
            return_nil)
    | Skipped _ ->
        return_nil (* there must be another error for this to happen *)
    | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
  in
  let* contracts = originated_contracts operation_result in
  let contracts = List.rev contracts in
  List.fold_left_e
    (fun acc (Internal_operation_result (_, r)) ->
      let* contracts = internal_originated_contracts r in
      Ok (List.rev_append contracts acc))
    contracts
    internal_operation_results

let rec originated_contracts : type kind. kind contents_result_list -> _ =
  let open Result_syntax in
  function
  | Single_result (Manager_operation_result _ as res) ->
      let* l = originated_contracts_single res in
      return @@ List.rev l
  | Single_result _ -> return_nil
  | Cons_result (res, rest) ->
      let* contracts1 = originated_contracts_single res in
      let* contracts2 = originated_contracts rest in
      Ok (List.rev_append contracts1 contracts2)

let estimated_storage_single ~force ~origination_size result =
  match estimated_storage_single ~origination_size result with
  | Error _ when force -> Ok Z.zero
  | res -> res

let estimated_storage ~force ~origination_size result =
  match estimated_storage ~origination_size result with
  | Error _ when force -> Ok Z.zero
  | res -> res

(* When --force is used, we don't want [originated_contracts] to fail as
   it would stop the client before the injection of the operation. *)
let originated_contracts ~force results =
  match originated_contracts results with
  | Error _ when force -> Result_syntax.return_nil
  | e -> e

let detect_script_failure : type kind. kind operation_metadata -> _ =
  let open Result_syntax in
  let rec detect_script_failure : type kind. kind contents_result_list -> _ =
    let detect_script_failure_single (type kind)
        (Manager_operation_result
           {operation_result; internal_operation_results; _} :
          kind Kind.manager contents_result) =
      let detect_script_failure (type kind)
          (result : (kind, _, _) operation_result) =
        match result with
        | Applied _ -> return_unit
        | Skipped _ -> assert false
        | Backtracked (_, None) ->
            (* there must be another error for this to happen *)
            return_unit
        | Backtracked (_, Some errs) ->
            record_trace
              (error_of_fmt "The transfer simulation failed.")
              (Error (Environment.wrap_tztrace errs))
        | Failed (_, errs) ->
            record_trace
              (error_of_fmt "The transfer simulation failed.")
              (Error (Environment.wrap_tztrace errs))
      in
      let* () = detect_script_failure operation_result in
      List.iter_e
        (fun (Internal_operation_result (_, r)) -> detect_script_failure r)
        internal_operation_results
    in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ -> return_unit
    | Cons_result (res, rest) ->
        let* () = detect_script_failure_single res in
        detect_script_failure rest
  in
  fun {contents} -> detect_script_failure contents

let signature_size_of_algo : Signature.algo -> int = function
  | Ed25519 -> Signature.Ed25519.size
  | Secp256k1 -> Signature.Secp256k1.size
  | P256 -> Signature.P256.size
  | Bls ->
      (* BLS signatures in operations are encoded with 2 extra bytes: a [ff]
         prefix and a tag [03]. *)
      Signature.Bls.size + 2

(* This value is used as a safety guard for gas limit. *)
let default_safety_guard = Gas.Arith.(integral_of_int_exn 100)

(*

   {2 High-level description of the automatic gas patching algorithm}

   When the user wants to inject a list of operations, some of which
   might have unspecified gas, fees or storage limit, the client
   performs a {e simulation} to estimate those limits and assign
   sensible values to them.

   The simulation works as follows:
   1. limits are assigned to dummy, high values to ensure that the operations
      can be simulated
      - 1.a) when a list of operations is partially specified, the algorithm
        allocates to each unspecified operation an equal portion of the
        maximum gas per block minus the gas consumed by the operations that
        do specify their limit
   2. the algorithm retrieves the effectively consumed gas and storage from the
      receipt
   3. the algorithm assigns slight over-approximations to the operation
   4. a default fee is computed and set

*)

let may_patch_limits (type kind) (cctxt : #Protocol_client_context.full)
    ~fee_parameter ~signature_algo ~chain ~block ?successor_level ?branch
    ?(force = false) ?(simulation = false) ?safety_guard
    (annotated_contents : kind Annotated_manager_operation.annotated_list) :
    kind Kind.manager contents_list tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* () =
    Tezos_client_base.Client_confirmations.wait_for_bootstrapped cctxt
  in
  let* {
         parametric =
           {
             hard_gas_limit_per_operation;
             hard_gas_limit_per_block;
             hard_storage_limit_per_operation;
             origination_size;
             cost_per_byte;
             _;
           };
         _;
       } =
    Alpha_services.Constants.all cctxt (chain, block)
  in
  let user_gas_limit_needs_patching user_gas_limit =
    Limit.fold user_gas_limit ~unknown:true ~known:(fun user_gas_limit ->
        Gas.Arith.(
          user_gas_limit < zero || hard_gas_limit_per_operation < user_gas_limit))
  in
  let user_storage_limit_needs_patching user_storage_limit =
    Limit.fold
      user_storage_limit
      ~unknown:true
      ~known:(fun user_storage_limit ->
        Z.Compare.(
          user_storage_limit < Z.zero
          || hard_storage_limit_per_operation < user_storage_limit))
  in
  let gas_patching_stats (Annotated_manager_operation.Manager_info c)
      need_patching gas_consumed =
    if user_gas_limit_needs_patching c.gas_limit then
      (need_patching + 1, gas_consumed)
    else
      ( need_patching,
        Gas.Arith.add
          gas_consumed
          (Limit.value ~when_unknown:Gas.Arith.zero c.gas_limit) )
  in
  let rec gas_patching_stats_list :
      type kind.
      kind Annotated_manager_operation.annotated_list ->
      int ->
      Gas.Arith.integral ->
      int * Gas.Arith.integral =
   fun op need_patching gas_consumed ->
    match op with
    | Single_manager minfo ->
        gas_patching_stats minfo need_patching gas_consumed
    | Cons_manager (minfo, rest) ->
        let need_patching, gas_consumed =
          gas_patching_stats minfo need_patching gas_consumed
        in
        gas_patching_stats_list rest need_patching gas_consumed
  in
  let gas_limit_per_patched_op =
    let need_gas_patching, gas_consumed =
      gas_patching_stats_list annotated_contents 0 Gas.Arith.zero
    in
    if need_gas_patching = 0 then hard_gas_limit_per_operation
    else
      let remaining_gas = Gas.Arith.sub hard_gas_limit_per_block gas_consumed in
      let average_per_operation_gas =
        Gas.Arith.integral_exn
        @@ Z.div
             (Gas.Arith.integral_to_z remaining_gas)
             (Z.of_int need_gas_patching)
      in
      Gas.Arith.min hard_gas_limit_per_operation average_per_operation_gas
  in
  let may_need_patching_single :
      type kind.
      kind Annotated_manager_operation.t ->
      kind Annotated_manager_operation.t option =
   fun op ->
    match op with
    | Manager_info c ->
        let needs_patching =
          Limit.is_unknown c.fee
          || user_gas_limit_needs_patching c.gas_limit
          || user_storage_limit_needs_patching c.storage_limit
        in
        if not needs_patching then None
        else
          (* Set limits for simulation purposes *)
          let gas_limit =
            if user_gas_limit_needs_patching c.gas_limit then
              Limit.known gas_limit_per_patched_op
            else c.gas_limit
          in
          let storage_limit =
            if user_storage_limit_needs_patching c.storage_limit then
              Limit.known hard_storage_limit_per_operation
            else c.storage_limit
          in
          let fee = Limit.value ~when_unknown:Tez.zero c.fee in
          Some
            (Manager_info
               {c with gas_limit; storage_limit; fee = Limit.known fee})
  in
  let may_need_patching ops =
    let rec loop :
        type kind.
        kind Annotated_manager_operation.annotated_list ->
        kind Annotated_manager_operation.annotated_list option = function
      | Single_manager annotated_op ->
          Option.map (fun op -> Annotated_manager_operation.Single_manager op)
          @@ may_need_patching_single annotated_op
      | Cons_manager (annotated_op, rest) -> (
          let annotated_op_opt = may_need_patching_single annotated_op in
          let rest_opt = loop rest in
          match (annotated_op_opt, rest_opt) with
          | None, None -> None
          | _ ->
              let op = Option.value ~default:annotated_op annotated_op_opt in
              let rest = Option.value ~default:rest rest_opt in
              Some (Cons_manager (op, rest)))
    in
    loop ops
  in
  (*
    The recursion here handles the case where an increased fee might increase the
    size of the operation, and so require a recalculation of the gas costs.
    Rationale for termination:
    - the fee for size increases linearly with the size of the operation.
    - however, when the size of the operation increase to make space for an
      increased fee, the amount of new fee that can be added without increasing
      the size of the block again increases exponentially.
    - hence, there will eventually be a increase of size that will fit any new
      fee without having to increase the size of the operation again.
  *)
  let rec patch_fee : type kind. first:bool -> kind contents -> kind contents =
   fun ~first -> function
    | Manager_operation c as op -> (
        let size =
          if first then
            (WithExceptions.Option.get ~loc:__LOC__
            @@ Data_encoding.Binary.fixed_length
                 Tezos_base.Operation.shell_header_encoding)
            + Data_encoding.Binary.length
                Operation.contents_encoding
                (Contents op)
            + signature_size_of_algo signature_algo
          else
            Data_encoding.Binary.length
              Operation.contents_encoding
              (Contents op)
        in
        let minimal_fees_in_nanotez =
          Q.mul
            (Q.of_int64 (Tez.to_mutez fee_parameter.minimal_fees))
            (Q.of_int 1000)
        in
        let minimal_fees_for_gas_in_nanotez =
          Q.mul
            fee_parameter.minimal_nanotez_per_gas_unit
            (Q.of_bigint @@ Gas.Arith.integral_to_z c.gas_limit)
        in
        let minimal_fees_for_size_in_nanotez =
          Q.mul fee_parameter.minimal_nanotez_per_byte (Q.of_int size)
        in
        let fees_in_nanotez =
          Q.add minimal_fees_in_nanotez
          @@ Q.add
               minimal_fees_for_gas_in_nanotez
               minimal_fees_for_size_in_nanotez
        in
        let fees_in_mutez = z_mutez_of_q_nanotez fees_in_nanotez in
        match Tez.of_mutez (Z.to_int64 fees_in_mutez) with
        | None -> assert false
        | Some fee ->
            if Tez.(fee <= c.fee) then op
            else patch_fee ~first (Manager_operation {c with fee}))
    | c -> c
  in
  let patch :
      type kind.
      first:bool ->
      kind Annotated_manager_operation.t * kind Kind.manager contents_result ->
      kind Kind.manager contents tzresult Lwt.t =
   fun ~first -> function
    | (Manager_info c as op), (Manager_operation_result _ as result) ->
        let* op =
          if user_gas_limit_needs_patching c.gas_limit then
            let*! gas = Lwt.return (estimated_gas_single result) in
            match gas with
            | Error _ when force ->
                (* When doing a simulation, set gas to the maximum possible value
                   so as to not change the error. When force injecting a failing
                   operation, set gas to zero to not pay fees for this
                   operation. *)
                let gas =
                  if simulation then gas_limit_per_patched_op
                  else Gas.Arith.zero
                in
                return
                  (Annotated_manager_operation.set_gas_limit
                     (Limit.known gas)
                     op)
            | Error _ as res -> Lwt.return res
            | Ok gas ->
                if Gas.Arith.(gas = zero) then
                  let*! () = cctxt#message "Estimated gas: none" in
                  return
                    (Annotated_manager_operation.set_gas_limit
                       (Limit.known Gas.Arith.zero)
                       op)
                else
                  let default_safety_guard =
                    match c.operation with
                    | Transaction {destination = Implicit _; _}
                    | Reveal _ | Delegation _ | Set_deposits_limit _
                    | Increase_paid_storage _ ->
                        Gas.Arith.zero
                    | _ -> default_safety_guard
                  in
                  let safety_guard =
                    Option.value safety_guard ~default:default_safety_guard
                  in
                  let*! () =
                    cctxt#message
                      "Estimated gas: %a units (will add %a for safety)"
                      Gas.Arith.pp
                      gas
                      Gas.Arith.pp
                      safety_guard
                  in
                  let safe_gas = Gas.Arith.(add (ceil gas) safety_guard) in
                  let patched_gas =
                    Gas.Arith.min safe_gas hard_gas_limit_per_operation
                  in
                  return
                    (Annotated_manager_operation.set_gas_limit
                       (Limit.known patched_gas)
                       op)
          else return op
        in
        let* op =
          if user_storage_limit_needs_patching c.storage_limit then
            let*? storage =
              estimated_storage_single
                ~origination_size:(Z.of_int origination_size)
                ~force
                result
            in
            if Z.equal storage Z.zero then
              let*! () = cctxt#message "Estimated storage: no bytes added" in
              return
                (Annotated_manager_operation.set_storage_limit
                   (Limit.known Z.zero)
                   op)
            else
              let*! () =
                cctxt#message
                  "Estimated storage: %s bytes added (will add 20 for safety)"
                  (Z.to_string storage)
              in
              let storage_limit =
                Z.min
                  (Z.add storage (Z.of_int 20))
                  hard_storage_limit_per_operation
              in
              return
                (Annotated_manager_operation.set_storage_limit
                   (Limit.known storage_limit)
                   op)
          else return op
        in
        if Limit.is_unknown c.fee then
          (* Setting a dummy fee is required for converting to manager op *)
          let op =
            Annotated_manager_operation.set_fee (Limit.known Tez.zero) op
          in
          let*? cm = Annotated_manager_operation.manager_from_annotated op in
          return (patch_fee ~first cm)
        else Lwt.return (Annotated_manager_operation.manager_from_annotated op)
  in
  let rec patch_list :
      type kind.
      bool ->
      kind Annotated_manager_operation.annotated_list ->
      kind Kind.manager contents_result_list ->
      kind Kind.manager contents_list tzresult Lwt.t =
   fun first annotated_list result_list ->
    match (annotated_list, result_list) with
    | Single_manager annotated, Single_result res ->
        let* op = patch ~first (annotated, res) in
        return (Single op)
    | Cons_manager (annotated, annotated_rest), Cons_result (res, res_rest) ->
        let* op = patch ~first (annotated, res) in
        let* rest = patch_list false annotated_rest res_rest in
        return (Cons (op, rest))
    | _ -> assert false
  in
  match may_need_patching annotated_contents with
  | Some annotated_for_simulation ->
      let*? contents_for_simulation =
        Annotated_manager_operation.manager_list_from_annotated
          annotated_for_simulation
      in
      let* _, _, result =
        simulate
          cctxt
          ~chain
          ~block
          ?successor_level
          ?branch
          contents_for_simulation
      in
      let* () =
        match detect_script_failure result with
        | Ok () -> return_unit
        | Error _ ->
            let*! () =
              cctxt#message
                "@[<v 2>This simulation failed:@,%a@]"
                Operation_result.pp_operation_result
                (contents_for_simulation, result.contents)
            in
            return_unit
      in
      let* () =
        let*? storage =
          estimated_storage
            ~origination_size:(Z.of_int origination_size)
            ~force
            result.contents
        in
        let*? burn =
          Environment.wrap_tzresult Tez.(cost_per_byte *? Z.to_int64 storage)
        in
        if Tez.(burn > fee_parameter.burn_cap) then
          let*! () =
            cctxt#error
              "The operation will burn %s%a which is higher than the \
               configured burn cap (%s%a).@\n\
              \ Use `--burn-cap %a` to emit this operation."
              Operation_result.tez_sym
              Tez.pp
              burn
              Operation_result.tez_sym
              Tez.pp
              fee_parameter.burn_cap
              Tez.pp
              burn
          in
          exit 1
        else return_unit
      in
      patch_list true annotated_contents result.contents
  | None ->
      Lwt.return
        (Annotated_manager_operation.manager_list_from_annotated
           annotated_contents)

let tenderbake_finality_confirmations = 1

let tenderbake_adjust_confirmations (cctxt : #Client_context.full) =
  let open Lwt_result_syntax in
  function
  | None -> Lwt.return_none
  | Some cli_confirmations ->
      if cli_confirmations > tenderbake_finality_confirmations then
        let*! () =
          cctxt#message
            "Tenderbake needs at most %d confirmations for finality (%d \
             given). Using %d confirmations."
            tenderbake_finality_confirmations
            cli_confirmations
            tenderbake_finality_confirmations
        in
        Lwt.return_some tenderbake_finality_confirmations
      else Lwt.return_some cli_confirmations

(* For Tenderbake we restrain the interval of confirmations to be [0,
   tenderbake_finality_confirmations]

   Any value greater than the tenderbake_finality_confirmations is treated as if it
   were tenderbake_finality_confirmations.
*)
let inject_operation_internal (type kind) cctxt ~chain ~block ?confirmations
    ?(dry_run = false) ?(simulation = false) ?(force = false) ?successor_level
    ?branch ?src_sk ?verbose_signing ?fee_parameter
    (contents : kind contents_list) =
  let open Lwt_result_syntax in
  let* _oph, op, result =
    if simulation then
      simulate cctxt ~chain ~block ?successor_level ?branch contents
    else
      preapply
        cctxt
        ~chain
        ~block
        ?fee_parameter
        ?verbose_signing
        ?branch
        ?src_sk
        contents
  in
  let* () =
    match detect_script_failure result with
    | Ok () -> return_unit
    | Error _ as res ->
        let*! () =
          cctxt#message
            "@[<v 2>This simulation failed (force = %b):@,%a@]"
            force
            Operation_result.pp_operation_result
            (op.protocol_data.contents, result.contents)
        in
        if force then return_unit else Lwt.return res
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn Operation.encoding (Operation.pack op)
  in
  if dry_run || simulation then
    let oph = Operation_hash.hash_bytes [bytes] in
    let*! () =
      cctxt#message
        "@[<v 0>Operation: 0x%a@,Operation hash is '%a'@]"
        Hex.pp
        (Hex.of_bytes bytes)
        Operation_hash.pp
        oph
    in
    let*! () =
      cctxt#message
        "@[<v 2>Simulation result:@,%a@]"
        Operation_result.pp_operation_result
        (op.protocol_data.contents, result.contents)
    in
    return (oph, op, result.contents)
  else
    let* oph = Shell_services.Injection.operation cctxt ~chain bytes in
    let*! () = cctxt#message "Operation successfully injected in the node." in
    let*! () = cctxt#message "Operation hash is '%a'" Operation_hash.pp oph in
    (* Adjust user-provided confirmations with respect to Alpha protocol finality properties *)
    let*! confirmations = tenderbake_adjust_confirmations cctxt confirmations in
    let* result =
      match confirmations with
      | None ->
          let*! () =
            cctxt#message
              "@[<v 0>NOT waiting for the operation to be included.@,\
               Use command@,\
              \  octez-client wait for %a to be included --confirmations %d \
               --branch %a@,\
               and/or an external block explorer to make sure that it has been \
               included.@]"
              Operation_hash.pp
              oph
              tenderbake_finality_confirmations
              Block_hash.pp
              op.shell.branch
          in
          return result
      | Some confirmations -> (
          let*! () =
            cctxt#message "Waiting for the operation to be included..."
          in
          let* h, i, j =
            Client_confirmations.wait_for_operation_inclusion
              ~branch:op.shell.branch
              ~confirmations
              cctxt
              ~chain
              oph
          in
          let* op' =
            Alpha_block_services.Operations.operation
              cctxt
              ~chain
              ~block:(`Hash (h, 0))
              i
              j
          in
          match op'.receipt with
          | Empty -> failwith "Internal error: pruned metadata."
          | Too_large -> failwith "Internal error: too large metadata."
          | Receipt No_operation_metadata ->
              let*! () =
                cctxt#message
                  "The operation metadata was not stored because it was too \
                   big, thus the failure to display the receipt."
              in
              failwith "Internal error: unexpected receipt."
          | Receipt (Operation_metadata receipt) -> (
              match Apply_results.kind_equal_list contents receipt.contents with
              | Some Apply_results.Eq ->
                  return (receipt : kind operation_metadata)
              | None -> failwith "Internal error: unexpected receipt."))
    in
    let*! () =
      cctxt#message
        "@[<v 2>This sequence of operations was run:@,%a@]"
        Operation_result.pp_operation_result
        (op.protocol_data.contents, result.contents)
    in
    let*? contracts = originated_contracts result.contents ~force in
    let*! () =
      List.iter_s
        (fun c ->
          cctxt#message "New contract %a originated." Contract_hash.pp c)
        contracts
    in
    let*! () =
      match confirmations with
      | None -> Lwt.return_unit
      | Some number ->
          if number >= tenderbake_finality_confirmations then
            cctxt#message
              "The operation was included in a block %d blocks ago."
              number
          else
            cctxt#message
              "@[<v 0>The operation has only been included %d blocks ago.@,\
               We recommend to wait more.@,\
               Use command@,\
              \  octez-client wait for %a to be included --confirmations %d \
               --branch %a@,\
               and/or an external block explorer.@]"
              number
              Operation_hash.pp
              oph
              tenderbake_finality_confirmations
              Block_hash.pp
              op.shell.branch
    in
    return (oph, op, result.contents)

let inject_operation (type kind) cctxt ~chain ~block ?confirmations
    ?(dry_run = false) ?(simulation = false) ?successor_level ?branch ?src_sk
    ?verbose_signing ?fee_parameter (contents : kind contents_list) =
  let open Lwt_result_syntax in
  let* () =
    Tezos_client_base.Client_confirmations.wait_for_bootstrapped cctxt
  in
  let* oph, op, result =
    inject_operation_internal
      cctxt
      ~chain
      ~block
      ?confirmations
      ~dry_run
      ~simulation
      ?successor_level
      ?branch
      ?src_sk
      ?verbose_signing
      ?fee_parameter
      (contents : kind contents_list)
  in
  return (oph, op.protocol_data.contents, result)

let prepare_manager_operation ~fee ~gas_limit ~storage_limit operation =
  Annotated_manager_operation.Manager_info
    {source = None; fee; gas_limit; storage_limit; counter = None; operation}

let reveal_error_message =
  "Requested operation requires to perform a public key revelation beforehand.\n\
   This cannot be done automatically when a custom fee or storage limit is \
   given.\n\
   If you wish to use a custom fee or storage limit, please first perform the \
   reveal operation separately using the dedicated command.\n\
   Otherwise, please do not specify custom fee or storage parameters."

let reveal_error (cctxt : #Protocol_client_context.full) =
  cctxt#error "%s" reveal_error_message

(* This function first gets the pending operations in the prevalidator. Then,
   it filters those that have an applied status from the given src. *)
let pending_applied_operations_of_source (cctxt : #full) chain src :
    packed_contents_list list Lwt.t =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2273
     Be able to get pending/validated operation of an implicit account.
  *)
  let open Lwt_result_syntax in
  let*! ops_opt =
    Alpha_block_services.Mempool.pending_operations cctxt ~chain ()
  in
  match ops_opt with
  | Error e ->
      let*! () =
        cctxt#error
          "Error while fetching pending operations: %a@."
          Error_monad.pp_print_trace
          e
      in
      exit 1
  | Ok ops ->
      Lwt.return
      @@ List.fold_left
           (fun acc (_oph, {protocol_data = Operation_data {contents; _}; _}) ->
             match contents with
             | Single (Manager_operation {source; _} as _op)
               when Signature.Public_key_hash.equal source src ->
                 Contents_list contents :: acc
             | Cons (Manager_operation {source; _}, _rest) as _op
               when Signature.Public_key_hash.equal source src ->
                 Contents_list contents :: acc
             | _ -> acc)
           []
           ops.Alpha_block_services.Mempool.validated

(* Given the gas and fee of an applied operation in the mempool, and the
   estimated gas of a new operation to inject, this function returns
   the amount of fee to put in the new operation to be able to replace
   the one already in the mempool *)
let compute_replacement_fees =
  let open Lwt_result_syntax in
  let q_fee_from_tez f = Tez.to_mutez f |> Z.of_int64 |> Q.of_bigint in
  let q_gas g = Gas.Arith.integral_to_z g |> Q.of_bigint in
  fun (cctxt : #full) old_op_fee old_op_gas new_op_gas ->
    (* convert quantities to rationals *)
    let old_op_fee = q_fee_from_tez old_op_fee in
    let old_op_gas = q_gas old_op_gas in
    let new_op_gas = q_gas new_op_gas in

    (* compute the fee / gas ratio of the old operation *)
    let old_op_ratio = Q.div old_op_fee old_op_gas in

    (* compute the equivalent (proportional) in fees of the new operation using
       the old operation's ratio *)
    let proportional_fee = Q.mul old_op_ratio new_op_gas in

    (* Fees cannot be smaller than estimated fees of the old or new op *)
    let max_fee = Q.max proportional_fee old_op_fee in

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2274
       Get the factor 1.05% from the plugin via an RPC *)
    let repl_q_fee = Q.mul max_fee (Q.make (Z.of_int 105) (Z.of_int 100)) in
    let repl_z_fee = Z.cdiv (Q.num repl_q_fee) (Q.den repl_q_fee) in
    try
      match Z.to_int64 repl_z_fee |> Tez.of_mutez with
      | Some replacement_fee -> Lwt.return replacement_fee
      | None ->
          let*! () =
            cctxt#error "Tez underflow while computing replacement fee@."
          in
          exit 1
    with Z.Overflow ->
      let*! () = cctxt#error "Tez overflow while computing replacement fee@." in
      exit 1

(* Given an operation to inject whose gas and fee are set, and the amount
   of fee the operation should pay to replace an existing applied operation
   in the mempool, this function computes the delta "replacement fee -
   operation's fee" and adds it to the operation. *)
let bump_manager_op_fee =
  (* Internal function to bump the fee of a manager operation by a delta *)
  let open Lwt_result_syntax in
  let bump_manager (cctxt : #full) delta = function
    | Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit} ->
        let* fee =
          match Tez.( +? ) fee delta with
          | Error _ ->
              let* () =
                cctxt#error "Tez overflow while computing replacement fee@."
              in
              exit 1
          | Ok new_fee -> return new_fee
        in
        return
        @@ Manager_operation
             {source; fee; counter; operation; gas_limit; storage_limit}
  in
  fun (type kind)
      (cctxt : #full)
      (contents : kind Kind.manager contents_list)
      new_fee
      replacement_fee
      ~user_fee ->
    (* We compute delta replacement_fee - new_fee *)
    match Tez.sub_opt replacement_fee new_fee with
    | None ->
        (* This can happen for instance if the user provided fee with
           command-line that are higher than the replacement threshold *)
        Lwt.return_ok contents
    | Some delta ->
        if Tez.equal delta Tez.zero then
          (* This can happen for instance if the user provided fee with
             command-line that are equal to the replacement threshold *)
          Lwt.return_ok contents
        else if Limit.is_unknown user_fee then
          match contents with
          | Single (Manager_operation _ as op) ->
              (* We add the delta to the op in case it's a Single *)
              let* op = bump_manager (cctxt : #full) delta op in
              Lwt.return_ok @@ Single op
          | Cons ((Manager_operation _ as op), rest) ->
              (* We add the delta to the first op in case it's a batch *)
              let* op = bump_manager (cctxt : #full) delta op in
              Lwt.return_ok @@ Cons (op, rest)
        else
          let* () =
            cctxt#error
              "The fee provided by the user is lower than the expected \
               replacement fee. Threshold is %a but got %a.@."
              Tez.pp
              replacement_fee
              Tez.pp
              new_fee
            (* New fee in this case correspond to provided user fee *)
          in
          exit 1

(* Bump the fee of the given operation whose fee have been computed by
   simulation, to be able to replace an existing applied operation in the
   mempool from the same source *)
let replace_operation (type kind) (cctxt : #full) chain source
    (contents : kind Kind.manager contents_list) ~user_fee :
    kind Kind.manager contents_list tzresult Lwt.t =
  let open Lwt_result_syntax in
  let exit_err ~is_new_op e =
    let* () =
      cctxt#error
        "Unexpected error while getting gas and fees of user's %s operation.@.\n\
         Error: %a@."
        (if is_new_op then "new" else "old")
        Error_monad.pp_print_trace
        (Environment.wrap_tztrace e)
    in
    exit 1
  in
  match Contents_list contents |> get_manager_operation_gas_and_fee with
  | Error e -> exit_err ~is_new_op:true e
  | Ok (new_op_fee, new_op_gas) -> (
      let*! contents_list =
        pending_applied_operations_of_source cctxt chain source
      in
      match contents_list with
      | [] ->
          let*! () =
            cctxt#error
              "Cannot replace! No validated manager operation found for %a in \
               mempool@."
              Signature.Public_key_hash.pp
              source
          in
          exit 1
      | _ :: _ :: _ as l ->
          let*! () =
            cctxt#error
              "More than one validated manager operation found for %a in \
               mempool. Found %d operations.@."
              Signature.Public_key_hash.pp
              source
              (List.length l)
          in
          exit 1
      | [old_contents] -> (
          get_manager_operation_gas_and_fee old_contents |> function
          | Error e -> exit_err ~is_new_op:false e
          | Ok (old_op_fee, old_op_gas) ->
              let*! delta =
                compute_replacement_fees cctxt old_op_fee old_op_gas new_op_gas
              in
              bump_manager_op_fee cctxt contents new_op_fee ~user_fee delta))

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2276
   https://gitlab.com/tezos/tezos/-/issues/2276 *)
let may_replace_operation (type kind) (cctxt : #full) chain from
    ~replace_by_fees ~user_fee (contents : kind Kind.manager contents_list) :
    kind Kind.manager contents_list tzresult Lwt.t =
  if replace_by_fees then replace_operation cctxt chain from contents ~user_fee
  else (* No replace by fees requested *)
    Lwt.return_ok contents

let apply_specified_options counter op source fee gas_limit storage_limit =
  let open Result_syntax in
  let* op = Annotated_manager_operation.set_source source op in
  let* op = Annotated_manager_operation.set_counter counter op in
  let* op = Annotated_manager_operation.join_fee fee op in
  let* op = Annotated_manager_operation.join_gas_limit gas_limit op in
  Annotated_manager_operation.join_storage_limit storage_limit op

let rec build_contents :
    type kind.
    Manager_counter.t ->
    kind Annotated_manager_operation.annotated_list ->
    public_key_hash ->
    Tez.t Limit.t ->
    Gas.Arith.integral Limit.t ->
    Z.t Limit.t ->
    kind Annotated_manager_operation.annotated_list tzresult =
  let open Result_syntax in
  fun counter op source fee gas_limit storage_limit ->
    match op with
    | Single_manager op ->
        let* op =
          apply_specified_options counter op source fee gas_limit storage_limit
        in
        return (Annotated_manager_operation.Single_manager op)
    | Cons_manager (op, rest) ->
        let* op =
          apply_specified_options counter op source fee gas_limit storage_limit
        in
        let* rest =
          build_contents
            (Manager_counter.succ counter)
            rest
            source
            fee
            gas_limit
            storage_limit
        in
        return (Annotated_manager_operation.Cons_manager (op, rest))

let inject_manager_operation cctxt ~chain ~block ?successor_level ?branch
    ?confirmations ?dry_run ?verbose_signing ?simulation ?force ?safety_guard
    ~source ~(src_pk : public_key) ~src_sk ~fee ~gas_limit ~storage_limit
    ?counter ?(replace_by_fees = false) ~fee_parameter (type kind)
    (operations : kind Annotated_manager_operation.annotated_list) :
    (Operation_hash.t
    * packed_operation
    * kind Kind.manager contents_list
    * kind Kind.manager contents_result_list)
    tzresult
    Lwt.t =
  let open Lwt_result_syntax in
  let* counter =
    match counter with
    | None ->
        let* pcounter =
          Alpha_services.Contract.counter cctxt (chain, block) source
        in
        let counter = Manager_counter.succ pcounter in
        return counter
    | Some counter -> return counter
  in
  let* key = Alpha_services.Contract.manager_key cctxt (chain, block) source in
  (* [has_reveal] assumes that a Reveal operation only appears as the first of a batch *)
  let has_reveal :
      type kind. kind Annotated_manager_operation.annotated_list -> bool =
    function
    | Single_manager (Manager_info {operation = Reveal _; _}) -> true
    | Cons_manager (Manager_info {operation = Reveal _; _}, _) -> true
    | _ -> false
  in
  let signature_algo =
    match src_pk with
    | Ed25519 _ -> Signature.Ed25519
    | Secp256k1 _ -> Secp256k1
    | P256 _ -> P256
    | Bls _ -> Bls
  in
  match key with
  | None when not (has_reveal operations) -> (
      let* () =
        if not (Limit.is_unknown fee && Limit.is_unknown storage_limit) then
          reveal_error cctxt
        else return_unit
      in
      let reveal =
        prepare_manager_operation
          ~fee:Limit.unknown
          ~gas_limit:Limit.unknown
          ~storage_limit:Limit.unknown
          (Reveal src_pk)
      in
      let*? reveal = Annotated_manager_operation.set_source source reveal in
      let*? reveal = Annotated_manager_operation.set_counter counter reveal in
      let*? rest =
        build_contents
          (Manager_counter.succ counter)
          operations
          source
          fee
          gas_limit
          storage_limit
      in
      let contents = Annotated_manager_operation.Cons_manager (reveal, rest) in
      let* contents =
        let* contents =
          may_patch_limits
            cctxt
            ~fee_parameter
            ~signature_algo
            ~chain
            ~block
            ?force
            ?simulation
            ?safety_guard
            ?successor_level
            ?branch
            contents
        in
        (may_replace_operation
           cctxt
           chain
           source
           ~replace_by_fees
           ~user_fee:fee)
          contents
      in
      let* oph, op, result =
        inject_operation_internal
          cctxt
          ~chain
          ~block
          ?confirmations
          ?dry_run
          ?simulation
          ?force
          ~fee_parameter
          ?verbose_signing
          ?successor_level
          ?branch
          ~src_sk
          contents
      in
      match pack_contents_list op.protocol_data.contents result with
      | Cons_and_result (_, _, rest) ->
          let second_op, second_result = unpack_contents_list rest in
          return (oph, Operation.pack op, second_op, second_result)
      | _ -> assert false)
  | Some _ when has_reveal operations ->
      failwith "The manager key was previously revealed."
  | _ ->
      let*? contents =
        build_contents counter operations source fee gas_limit storage_limit
      in
      let* contents =
        let* contents =
          may_patch_limits
            cctxt
            ~fee_parameter
            ~signature_algo
            ~chain
            ~block
            ?force
            ?simulation
            ?safety_guard
            ?successor_level
            ?branch
            contents
        in
        (may_replace_operation
           cctxt
           chain
           source
           ~replace_by_fees
           ~user_fee:fee)
          contents
      in
      let* oph, op, result =
        inject_operation_internal
          cctxt
          ~chain
          ~block
          ?confirmations
          ?dry_run
          ?verbose_signing
          ?simulation
          ?force
          ~fee_parameter
          ?successor_level
          ?branch
          ~src_sk
          contents
      in
      return (oph, Operation.pack op, op.protocol_data.contents, result)
