(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Main Entry Points *)

open Alpha_context

type error += Wrong_voting_period of Voting_period.t * Voting_period.t

(* `Temporary *)

type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t

(* `Temporary *)

type error += Duplicate_endorsement of Baker_hash.t (* `Branch *)

type error += Invalid_endorsement_level

type error += Invalid_commitment of {expected : bool}

type error += Internal_operation_replay of packed_internal_operation

type error += Invalid_activation of {pkh : Ed25519.Public_key_hash.t}

type error += Multiple_revelation

type error += Gas_quota_exceeded_init_deserialize (* Permanent *)

type error +=
  | Not_enough_endorsements_for_priority of {
      required : int;
      priority : int;
      endorsements : int;
      timestamp : Time.t;
    }

type error += (* `Branch *) Double_injection_of_evidence

type error += (* `Branch *) Unrequired_evidence

type error += (* `Permanent *) Failing_noop_error

type error += Not_a_baker_contract

type error += Invalid_protocols of string list

type error += Not_an_active_consensus_key of Signature.Public_key_hash.t

let () =
  register_error_kind
    `Temporary
    ~id:"operation.wrong_endorsement_predecessor"
    ~title:"Wrong endorsement predecessor"
    ~description:
      "Trying to include an endorsement in a block that is not the successor \
       of the endorsed one"
    ~pp:(fun ppf (e, p) ->
      Format.fprintf
        ppf
        "Wrong predecessor %a, expected %a"
        Block_hash.pp
        p
        Block_hash.pp
        e)
    Data_encoding.(
      obj2
        (req "expected" Block_hash.encoding)
        (req "provided" Block_hash.encoding))
    (function
      | Wrong_endorsement_predecessor (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_endorsement_predecessor (e, p)) ;
  register_error_kind
    `Temporary
    ~id:"operation.wrong_voting_period"
    ~title:"Wrong voting period"
    ~description:
      "Trying to include a proposal or ballot meant for another voting period"
    ~pp:(fun ppf (e, p) ->
      Format.fprintf
        ppf
        "Wrong voting period %a, current is %a"
        Voting_period.pp
        p
        Voting_period.pp
        e)
    Data_encoding.(
      obj2
        (req "current" Voting_period.encoding)
        (req "provided" Voting_period.encoding))
    (function Wrong_voting_period (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_voting_period (e, p)) ;
  register_error_kind
    `Branch
    ~id:"operation.duplicate_endorsement"
    ~title:"Duplicate endorsement"
    ~description:"Two endorsements received from same baker"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "Duplicate endorsement from baker %a (possible replay attack)."
        Baker_hash.pp_short
        k)
    Data_encoding.(obj1 (req "baker" Baker_hash.encoding))
    (function Duplicate_endorsement k -> Some k | _ -> None)
    (fun k -> Duplicate_endorsement k) ;
  register_error_kind
    `Temporary
    ~id:"operation.invalid_endorsement_level"
    ~title:"Unexpected level in endorsement"
    ~description:
      "The level of an endorsement is inconsistent with the  provided block \
       hash."
    ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected level in endorsement.")
    Data_encoding.unit
    (function Invalid_endorsement_level -> Some () | _ -> None)
    (fun () -> Invalid_endorsement_level) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_commitment"
    ~title:"Invalid commitment in block header"
    ~description:"The block header has invalid commitment."
    ~pp:(fun ppf expected ->
      if expected then
        Format.fprintf ppf "Missing seed's nonce commitment in block header."
      else
        Format.fprintf
          ppf
          "Unexpected seed's nonce commitment in block header.")
    Data_encoding.(obj1 (req "expected" bool))
    (function Invalid_commitment {expected} -> Some expected | _ -> None)
    (fun expected -> Invalid_commitment {expected}) ;
  register_error_kind
    `Permanent
    ~id:"internal_operation_replay"
    ~title:"Internal operation replay"
    ~description:"An internal operation was emitted twice by a script"
    ~pp:
      (fun ppf
           ( Internal_manager_operation {nonce; _}
           | Internal_baker_operation {nonce; _} ) ->
      Format.fprintf
        ppf
        "Internal operation %d was emitted twice by a script"
        nonce)
    Operation.internal_operation_encoding
    (function Internal_operation_replay op -> Some op | _ -> None)
    (fun op -> Internal_operation_replay op) ;
  register_error_kind
    `Permanent
    ~id:"operation.invalid_activation"
    ~title:"Invalid activation"
    ~description:
      "The given key and secret do not correspond to any existing \
       preallocated contract"
    ~pp:(fun ppf pkh ->
      Format.fprintf
        ppf
        "Invalid activation. The public key %a does not match any commitment."
        Ed25519.Public_key_hash.pp
        pkh)
    Data_encoding.(obj1 (req "pkh" Ed25519.Public_key_hash.encoding))
    (function Invalid_activation {pkh} -> Some pkh | _ -> None)
    (fun pkh -> Invalid_activation {pkh}) ;
  register_error_kind
    `Permanent
    ~id:"block.multiple_revelation"
    ~title:"Multiple revelations were included in a manager operation"
    ~description:
      "A manager operation should not contain more than one revelation"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Multiple revelations were included in a manager operation")
    Data_encoding.empty
    (function Multiple_revelation -> Some () | _ -> None)
    (fun () -> Multiple_revelation) ;
  register_error_kind
    `Permanent
    ~id:"gas_exhausted.init_deserialize"
    ~title:"Not enough gas for initial deserialization of script expressions"
    ~description:
      "Gas limit was not high enough to deserialize the transaction \
       parameters or origination script code or initial storage, making the \
       operation impossible to parse within the provided gas bounds."
    Data_encoding.empty
    (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
    (fun () -> Gas_quota_exceeded_init_deserialize) ;
  register_error_kind
    `Permanent
    ~id:"operation.not_enough_endorsements_for_priority"
    ~title:"Not enough endorsements for priority"
    ~description:
      "The block being validated does not include the required minimum number \
       of endorsements for this priority."
    ~pp:(fun ppf (required, endorsements, priority, timestamp) ->
      Format.fprintf
        ppf
        "Wrong number of endorsements (%i) for priority (%i), %i are expected \
         at %a"
        endorsements
        priority
        required
        Time.pp_hum
        timestamp)
    Data_encoding.(
      obj4
        (req "required" int31)
        (req "endorsements" int31)
        (req "priority" int31)
        (req "timestamp" Time.encoding))
    (function
      | Not_enough_endorsements_for_priority
          {required; endorsements; priority; timestamp} ->
          Some (required, endorsements, priority, timestamp)
      | _ ->
          None)
    (fun (required, endorsements, priority, timestamp) ->
      Not_enough_endorsements_for_priority
        {required; endorsements; priority; timestamp}) ;
  register_error_kind
    `Branch
    ~id:"block.double_injection"
    ~title:"Double injection of evidence is not permitted"
    ~description:
      "This evidence has already been used against that delegate and cannot \
       be re-injected"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "This evidence has already been used and cannot be re-injected")
    Data_encoding.empty
    (function Double_injection_of_evidence -> Some () | _ -> None)
    (fun () -> Double_injection_of_evidence) ;
  register_error_kind
    `Temporary
    ~id:"block.unrequired_evidence"
    ~title:"Unrequired evidence"
    ~description:"An evidence is unrequired"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A valid evidence operation cannot be applied: the associated \
         delegate has previously been denunciated in this cycle.")
    Data_encoding.empty
    (function Unrequired_evidence -> Some () | _ -> None)
    (fun () -> Unrequired_evidence) ;
  register_error_kind
    `Permanent
    ~id:"operation.failing_noop"
    ~title:"Failing_noop operation are not executed by the protocol"
    ~description:
      "The failing_noop operation is an operation that is not and never will \
       be executed by the protocol."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The failing_noop operation cannot be executed by the protocol")
    Data_encoding.empty
    (function Failing_noop_error -> Some () | _ -> None)
    (fun () -> Failing_noop_error) ;
  register_error_kind
    `Temporary
    ~id:"operation.invalid_protocols"
    ~title:"Invalid protocol(s)"
    ~description:"One or more submitted proposals is not a valid protocol hash"
    ~pp:(fun ppf protocols ->
      Format.(
        fprintf
          ppf
          "One or more submitted proposals is not a valid protocol hash: %a"
          (pp_print_list pp_print_string)
          protocols))
    Data_encoding.(obj1 (req "protocols" (list string)))
    (function Invalid_protocols protocols -> Some protocols | _ -> None)
    (fun protocols -> Invalid_protocols protocols) ;
  register_error_kind
    `Permanent
    ~id:"operation.not_a_baker_contract"
    ~title:"Not a baker contract"
    ~description:"Operation is only valid for baker contracts"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Operation is only valid for baker contracts")
    Data_encoding.empty
    (function Not_a_baker_contract -> Some () | _ -> None)
    (fun () -> Not_a_baker_contract) ;
  register_error_kind
    `Temporary
    ~id:"operation.not_an_active_consensus_key"
    ~title:"Not an active baker consensus key"
    ~description:"The given key is not an active baker consensus key."
    ~pp:(fun ppf key ->
      Format.fprintf
        ppf
        "The given key %a is not an active baker consensus key."
        Signature.Public_key_hash.pp
        key)
    Data_encoding.(obj1 (req "key" Signature.Public_key_hash.encoding))
    (function Not_an_active_consensus_key k -> Some k | _ -> None)
    (fun k -> Not_an_active_consensus_key k)

(* Try to map the given key to a baker consensus key. Fails with
   [Not_an_active_consensus_key] when no matching consensus key is found. *)
let map_delegate ctxt delegate =
  Baker.is_consensus_key ctxt delegate
  >>=? function
  | None ->
      fail @@ Not_an_active_consensus_key delegate
  | Some baker_hash ->
      return baker_hash

open Apply_results

let apply_manager_operation_content :
    type kind.
    Alpha_context.t ->
    Script_ir_translator.unparsing_mode ->
    payer:Contract.t ->
    source:Contract.t ->
    chain_id:Chain_id.t ->
    internal:bool ->
    kind manager_operation ->
    ( context
    * kind successful_manager_operation_result
    * packed_internal_operation list )
    tzresult
    Lwt.t =
 fun ctxt mode ~payer ~source ~chain_id ~internal operation ->
  let before_operation =
    (* This context is not used for backtracking. Only to compute
         gas consumption and originations for the operation result. *)
    ctxt
  in
  Contract.must_exist ctxt source
  >>=? fun () ->
  Gas.consume ctxt Michelson_v1_gas.Cost_of.manager_operation
  >>?= fun ctxt ->
  let apply_origination ctxt ~(delegate : Baker_hash.t option)
      ~(script : Script.t) ~(preorigination : Contract.t option)
      ~(credit : Tez.t) =
    Script.force_decode_in_context ctxt script.storage
    >>?= fun (unparsed_storage, ctxt) ->
    (* see [note] *)
    Gas.consume ctxt (Script.deserialized_cost unparsed_storage)
    >>?= fun ctxt ->
    Script.force_decode_in_context ctxt script.code
    >>?= fun (unparsed_code, ctxt) ->
    (* see [note] *)
    Gas.consume ctxt (Script.deserialized_cost unparsed_code)
    >>?= fun ctxt ->
    Script_ir_translator.parse_script ctxt ~legacy:false script
    >>=? fun (Ex_originated_script parsed_script, ctxt) ->
    Script_ir_translator.collect_lazy_storage
      ctxt
      parsed_script.storage_type
      parsed_script.storage
    >>?= fun (to_duplicate, ctxt) ->
    let to_update = Script_ir_translator.no_lazy_storage_id in
    Script_ir_translator.extract_lazy_storage_diff
      ctxt
      Optimized
      parsed_script.storage_type
      parsed_script.storage
      ~to_duplicate
      ~to_update
      ~temporary:false
    >>=? fun (storage, lazy_storage_diff, ctxt) ->
    Script_ir_translator.unparse_data
      ctxt
      Optimized
      parsed_script.storage_type
      storage
    >>=? fun (storage, ctxt) ->
    let storage = Script.lazy_expr (Micheline.strip_locations storage) in
    let script = {script with storage} in
    Contract.spend ctxt source credit
    >>=? fun ctxt ->
    ( match preorigination with
    | Some contract ->
        assert internal ;
        (* The preorigination field is only used to early return
                 the address of an originated contract in Michelson.
                 It cannot come from the outside. *)
        ok (ctxt, contract)
    | None ->
        Contract.fresh_contract_from_current_nonce ctxt )
    >>?= fun (ctxt, contract) ->
    Contract.originate
      ctxt
      contract
      ~delegate
      ~balance:credit
      ~script:(script, lazy_storage_diff)
    >>=? fun ctxt ->
    Fees.origination_burn ctxt
    >>?= fun (ctxt, origination_burn) ->
    Fees.record_paid_storage_space ctxt contract
    >|=? fun (ctxt, storage_size, paid_storage_size_diff, fees) ->
    let balance_updates =
      Receipt.cleanup_balance_updates
        [ (Contract payer, Debited fees);
          (Contract payer, Debited origination_burn);
          (Contract source, Debited credit);
          (Contract contract, Credited credit) ]
    in
    let originated_contracts = [contract] in
    ( ctxt,
      lazy_storage_diff,
      balance_updates,
      originated_contracts,
      storage_size,
      paid_storage_size_diff )
  in
  match operation with
  | Reveal _ ->
      return
        (* No-op: action already performed by `precheck_manager_contents`. *)
        ( ctxt,
          ( Reveal_result
              {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt}
            : kind successful_manager_operation_result ),
          [] )
  | Transaction {amount; parameters; destination; entrypoint} -> (
      Contract.spend ctxt source amount
      >>=? fun ctxt ->
      ( match Contract.is_implicit destination with
      | None ->
          return (ctxt, [], false)
      | Some _ -> (
          Contract.allocated ctxt destination
          >>=? function
          | true ->
              return (ctxt, [], false)
          | false ->
              Lwt.return
                ( Fees.origination_burn ctxt
                >|? fun (ctxt, origination_burn) ->
                ( ctxt,
                  [(Receipt.Contract payer, Receipt.Debited origination_burn)],
                  true ) ) ) )
      >>=? fun (ctxt, maybe_burn_balance_update, allocated_destination_contract)
               ->
      Contract.credit ctxt destination amount
      >>=? fun ctxt ->
      Contract.get_script ctxt destination
      >>=? fun (ctxt, script) ->
      match script with
      | None ->
          Lwt.return
            ( ( match entrypoint with
              | "default" ->
                  ok_unit
              | entrypoint ->
                  error (Script_tc_errors.No_such_entrypoint entrypoint) )
            >>? fun () ->
            Script.force_decode_in_context ctxt parameters
            >>? fun (arg, ctxt) ->
            (* see [note] *)
            (* [note]: for toplevel ops, cost is nil since the
               lazy value has already been forced at precheck, so
               we compute and consume the full cost again *)
            let cost_arg = Script.deserialized_cost arg in
            Gas.consume ctxt cost_arg
            >>? fun ctxt ->
            ( match Micheline.root arg with
            | Prim (_, D_Unit, [], _) ->
                (* Allow [Unit] parameter to non-scripted contracts. *)
                ok ctxt
            | _ ->
                error (Script_interpreter.Bad_contract_parameter destination)
            )
            >|? fun ctxt ->
            let result =
              Transaction_result
                {
                  code = None;
                  storage = None;
                  lazy_storage_diff = None;
                  balance_updates =
                    Receipt.cleanup_balance_updates
                      ( [ (Receipt.Contract source, Receipt.Debited amount);
                          (Contract destination, Credited amount) ]
                      @ maybe_burn_balance_update );
                  originated_contracts = [];
                  consumed_gas =
                    Gas.consumed ~since:before_operation ~until:ctxt;
                  storage_size = Z.zero;
                  paid_storage_size_diff = Z.zero;
                  allocated_destination_contract;
                }
            in
            (ctxt, result, []) )
      | Some script ->
          Script.force_decode_in_context ctxt parameters
          >>?= fun (parameter, ctxt) ->
          (* see [note] *)
          let cost_parameter = Script.deserialized_cost parameter in
          Gas.consume ctxt cost_parameter
          >>?= fun ctxt ->
          let step_constants =
            let open Script_interpreter in
            {source; payer; self = destination; amount; chain_id}
          in
          Script_interpreter.execute
            ctxt
            mode
            step_constants
            ~script
            ~parameter
            ~entrypoint
          >>=? fun {ctxt; code; storage; lazy_storage_diff; operations} ->
          Contract.update_script_storage
            ctxt
            destination
            storage
            lazy_storage_diff
          >>=? fun ctxt ->
          Fees.record_paid_storage_space ctxt destination
          >>=? fun (ctxt, new_size, paid_storage_size_diff, fees) ->
          Contract.init_set_script_code_cached ctxt destination code
          |> fun ctxt ->
          Contract.originated_from_current_nonce
            ~since:before_operation
            ~until:ctxt
          >|=? fun originated_contracts ->
          let result =
            Transaction_result
              {
                code = Some code;
                storage = Some storage;
                lazy_storage_diff;
                balance_updates =
                  Receipt.cleanup_balance_updates
                    [ (Contract payer, Debited fees);
                      (Contract source, Debited amount);
                      (Contract destination, Credited amount) ];
                originated_contracts;
                consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
                storage_size = new_size;
                paid_storage_size_diff;
                allocated_destination_contract;
              }
          in
          (ctxt, result, operations) )
  | Origination_legacy {delegate; script; preorigination; credit} ->
      ( match delegate with
      | None ->
          return_none
      | Some delegate ->
          map_delegate ctxt delegate >|=? Option.some )
      >>=? fun delegate ->
      apply_origination ctxt ~delegate ~script ~preorigination ~credit
      >|=? fun ( ctxt,
                 lazy_storage_diff,
                 balance_updates,
                 originated_contracts,
                 storage_size,
                 paid_storage_size_diff ) ->
      let result =
        Origination_legacy_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size;
            paid_storage_size_diff;
          }
      in
      (ctxt, result, [])
  | Origination {delegate; script; preorigination; credit} ->
      apply_origination ctxt ~delegate ~script ~preorigination ~credit
      >|=? fun ( ctxt,
                 lazy_storage_diff,
                 balance_updates,
                 originated_contracts,
                 storage_size,
                 paid_storage_size_diff ) ->
      let result =
        Origination_result
          {
            lazy_storage_diff;
            balance_updates;
            originated_contracts;
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size;
            paid_storage_size_diff;
          }
      in
      (ctxt, result, [])
  | Delegation_legacy delegate ->
      ( match delegate with
      | None ->
          return_none
      | Some delegate ->
          map_delegate ctxt delegate >|=? Option.some )
      >>=? fun delegate ->
      Delegation.set ctxt source delegate
      >>=? fun ctxt ->
      return
        ( ctxt,
          Delegation_legacy_result
            {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
          [] )
  | Delegation delegate ->
      Delegation.set ctxt source delegate
      >>=? fun ctxt ->
      return
        ( ctxt,
          Delegation_result
            {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
          [] )
  | Baker_registration {credit; consensus_key; threshold; owner_keys} ->
      Contract.spend ctxt source credit
      >>=? fun ctxt ->
      Baker.register ctxt ~balance:credit ~threshold ~owner_keys ~consensus_key
      >>=? fun (ctxt, baker) ->
      let contract = Contract.baker_contract baker in
      Fees.origination_burn ctxt
      >>?= fun (ctxt, origination_burn) ->
      Fees.record_paid_storage_space ctxt contract
      >|=? fun (ctxt, size, paid_storage_size_diff, fees) ->
      let result =
        Baker_registration_result
          {
            balance_updates =
              Receipt.cleanup_balance_updates
                [ (Contract payer, Debited fees);
                  (Contract payer, Debited origination_burn);
                  (Contract source, Debited credit);
                  (Contract contract, Credited credit) ];
            registered_baker = baker;
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size = size;
            paid_storage_size_diff;
          }
      in
      (ctxt, result, [])

type success_or_failure = Success of context | Failure

let apply_baker_operation_content :
    type kind.
    Alpha_context.t ->
    baker:Baker_hash.t ->
    kind baker_operation ->
    ( context
    * kind successful_baker_operation_result
    * packed_internal_operation list )
    tzresult
    Lwt.t =
 fun ctxt ~baker operation ->
  let before_operation =
    (* This context is not used for backtracking. Only to compute
         gas consumption and originations for the operation result. *)
    ctxt
  in
  Contract.(must_exist ctxt @@ baker_contract baker)
  >>=? fun () ->
  Lwt.return (Gas.consume ctxt Michelson_v1_gas.Cost_of.baker_operation)
  >>=? fun ctxt ->
  match operation with
  | Baker_proposals {period; proposals} ->
      let level = Level.current ctxt in
      fail_unless
        Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period))
      >>=? fun () ->
      (* try to convert proposals to protocol hashes *)
      List.fold_left
        (fun acc proposal ->
          match (acc, Protocol_hash.of_b58check_opt proposal) with
          | (Ok acc, Some proposal) ->
              Ok (proposal :: acc)
          | (Ok _, None) ->
              Error [proposal]
          | (Error acc, None) ->
              Error (proposal :: acc)
          | (Error _, Some _) ->
              acc)
        (Ok [])
        proposals
      |> (function
           | Error invalid_proposals ->
               fail (Invalid_protocols invalid_proposals)
           | Ok p ->
               return p)
      >>=? fun proposals ->
      Amendment.record_proposals ctxt baker proposals
      >>=? fun ctxt ->
      return
        ( ctxt,
          ( Baker_proposals_result
              {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt}
            : kind successful_baker_operation_result ),
          [] )
  | Baker_ballot {period; proposal; ballot} ->
      let level = Level.current ctxt in
      fail_unless
        Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period))
      >>=? fun () ->
      ( match Protocol_hash.of_b58check_opt proposal with
      | None ->
          fail (Invalid_protocols [proposal])
      | Some proposal ->
          return proposal )
      >>=? fun proposal ->
      Amendment.record_ballot ctxt baker proposal ballot
      >>=? fun ctxt ->
      return
        ( ctxt,
          Baker_ballot_result
            {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
          [] )
  | Set_baker_active active ->
      Baker.set_active ctxt baker active
      >>=? fun ctxt ->
      return
        ( ctxt,
          Set_baker_active_result
            {
              active;
              consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            },
          [] )
  | Set_baker_consensus_key key ->
      Baker.set_consensus_key ctxt baker key
      >>=? fun ctxt ->
      return
        ( ctxt,
          Set_baker_consensus_key_result
            {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
          [] )
  | Set_baker_pvss_key key ->
      Baker.init_set_pvss_key ctxt baker key
      >>= fun ctxt ->
      return
        ( ctxt,
          Set_baker_pvss_key_result
            {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
          [] )

let apply_internal_operations ctxt mode ~payer ~chain_id ops =
  let skip =
    List.rev_map (function
        | Internal_manager_operation op ->
            Internal_manager_operation_result
              (op, Skipped (manager_kind op.operation))
        | Internal_baker_operation op ->
            Internal_baker_operation_result
              (op, Skipped (baker_kind op.operation)))
  in
  let rec apply ctxt applied worklist =
    match worklist with
    | [] ->
        Lwt.return (Success ctxt, List.rev applied)
    | Internal_manager_operation ({source; operation; nonce} as op) :: rest
      -> (
        ( if internal_nonce_already_recorded ctxt nonce then
          fail (Internal_operation_replay (Internal_manager_operation op))
        else
          let ctxt = record_internal_nonce ctxt nonce in
          apply_manager_operation_content
            ctxt
            mode
            ~source
            ~payer
            ~chain_id
            ~internal:true
            operation )
        >>= function
        | Error errors ->
            let result =
              Internal_manager_operation_result
                (op, Failed (manager_kind op.operation, errors))
            in
            let skipped = skip rest in
            Lwt.return (Failure, List.rev (skipped @ (result :: applied)))
        | Ok (ctxt, result, emitted) ->
            apply
              ctxt
              ( Internal_manager_operation_result (op, Applied result)
              :: applied )
              (rest @ emitted) )
    | Internal_baker_operation ({baker; operation; nonce} as op) :: rest -> (
        ( if internal_nonce_already_recorded ctxt nonce then
          fail (Internal_operation_replay (Internal_baker_operation op))
        else
          let ctxt = record_internal_nonce ctxt nonce in
          apply_baker_operation_content ctxt ~baker operation )
        >>= function
        | Error errors ->
            let result =
              Internal_baker_operation_result
                (op, Failed (baker_kind op.operation, errors))
            in
            let skipped = skip rest in
            Lwt.return (Failure, List.rev (skipped @ (result :: applied)))
        | Ok (ctxt, result, emitted) ->
            apply
              ctxt
              (Internal_baker_operation_result (op, Applied result) :: applied)
              (rest @ emitted) )
  in
  apply ctxt [] ops

let precheck_manager_contents (type kind) ctxt chain_id raw_operation
    (op : kind Kind.manager contents) : context tzresult Lwt.t =
  let (Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit}) =
    op
  in
  Gas.check_limit ctxt gas_limit
  >>?= fun () ->
  let ctxt = Gas.set_limit ctxt gas_limit in
  Fees.check_storage_limit ctxt storage_limit
  >>?= fun () ->
  Contract.must_be_allocated ctxt (Contract.implicit_contract source)
  >>=? fun () ->
  Contract.check_counter_increment ctxt source counter
  >>=? fun () ->
  let precheck_origination ~(script : Script.t) =
    (* Fail quickly if not enough gas for minimal deserialization cost *)
    Lwt.return
    @@ record_trace Gas_quota_exceeded_init_deserialize
    @@ ( Gas.consume ctxt (Script.minimal_deserialize_cost script.code)
       >>? fun ctxt ->
       Gas.check_enough ctxt (Script.minimal_deserialize_cost script.storage)
       >>? fun () ->
       (* Fail if not enough gas for complete deserialization cost *)
       Script.force_decode_in_context ctxt script.code
       >>? fun (_code, ctxt) ->
       Script.force_decode_in_context ctxt script.storage
       >|? fun (_storage, ctxt) -> ctxt )
  in
  ( match operation with
  | Reveal pk ->
      Contract.reveal_public_key ctxt source pk
  | Transaction {parameters; _} ->
      Lwt.return
      (* Fail quickly if not enough gas for minimal deserialization cost *)
      @@ record_trace Gas_quota_exceeded_init_deserialize
      @@ ( Gas.check_enough ctxt (Script.minimal_deserialize_cost parameters)
         >>? fun () ->
         (* Fail if not enough gas for complete deserialization cost *)
         Script.force_decode_in_context ctxt parameters
         >|? fun (_arg, ctxt) -> ctxt )
  | Origination_legacy {script; _} ->
      precheck_origination ~script
  | Origination {script; _} ->
      precheck_origination ~script
  | _ ->
      return ctxt )
  >>=? fun ctxt ->
  Contract.get_public_key ctxt source_contract
  >>=? fun public_key ->
  (* Currently, the `raw_operation` only contains one signature, so
     all operations are required to be from the same manager. This may
     change in the future, allowing several managers to group-sign a
     sequence of transactions.  *)
  Operation.check_signature public_key chain_id raw_operation
  >>?= fun () ->
  Contract.increment_counter ctxt source
  >>=? fun ctxt ->
  Contract.spend ctxt (Contract.implicit_contract source) fee
  >>=? fun ctxt -> Lwt.return @@ add_fees ctxt fee

let apply_manager_contents (type kind) ctxt mode chain_id
    (op : kind Kind.manager contents) :
    ( success_or_failure
    * kind manager_operation_result
    * packed_internal_operation_result list )
    Lwt.t =
  let (Manager_operation {source; operation; gas_limit; storage_limit}) = op in
  let ctxt = Gas.set_limit ctxt gas_limit in
  let ctxt = Fees.start_counting_storage_fees ctxt in
  let source = Contract.implicit_contract source in
  Contract.clear_script_code_cached ctxt
  |> Contract.clear_storage_cached
  |> fun ctxt ->
  apply_manager_operation_content
    ctxt
    mode
    ~source
    ~payer:source
    ~internal:false
    ~chain_id
    operation
  >>= function
  | Ok (ctxt, operation_results, internal_operations) -> (
      apply_internal_manager_operations
        ctxt
        mode
        ~payer:source
        ~chain_id
        internal_operations
      >>= function
      | (Success ctxt, internal_operations_results) -> (
          Fees.burn_storage_fees ctxt ~storage_limit ~payer:source
          >|= function
          | Ok ctxt ->
              ( Success ctxt,
                Applied operation_results,
                internal_operations_results )
          | Error errors ->
              ( Failure,
                Backtracked (operation_results, Some errors),
                internal_operations_results ) )
      | (Failure, internal_operations_results) ->
          Lwt.return
            (Failure, Applied operation_results, internal_operations_results) )
  | Error errors ->
      Lwt.return (Failure, Failed (manager_kind operation, errors), [])

let skipped_operation_result :
    type kind. kind manager_operation -> kind manager_operation_result =
  function
  | operation -> (
    match operation with
    | Reveal _ ->
        Applied
          ( Reveal_result {consumed_gas = Z.zero}
            : kind successful_manager_operation_result )
    | _ ->
        Skipped (manager_kind operation) )

let rec mark_skipped :
    type kind.
    baker:Baker_hash.t ->
    Level.t ->
    kind Kind.manager contents_list ->
    kind Kind.manager contents_result_list =
 fun ~baker level -> function
  | Single (Manager_operation {source; fee; operation}) ->
      let source = Contract.implicit_contract source in
      Single_result
        (Manager_operation_result
           {
             balance_updates =
               Receipt.cleanup_balance_updates
                 [ (Contract source, Debited fee);
                   (Fees (baker, level.cycle), Credited fee) ];
             operation_result = skipped_operation_result operation;
             internal_operation_results = [];
           })
  | Cons (Manager_operation {source; fee; operation}, rest) ->
      let source = Contract.implicit_contract source in
      Cons_result
        ( Manager_operation_result
            {
              balance_updates =
                Receipt.cleanup_balance_updates
                  [ (Contract source, Debited fee);
                    (Fees (baker, level.cycle), Credited fee) ];
              operation_result = skipped_operation_result operation;
              internal_operation_results = [];
            },
          mark_skipped ~baker level rest )

let rec precheck_manager_contents_list :
    type kind.
    Alpha_context.t ->
    Chain_id.t ->
    _ Operation.t ->
    kind Kind.manager contents_list ->
    context tzresult Lwt.t =
 fun ctxt chain_id raw_operation contents_list ->
  match contents_list with
  | Single (Manager_operation _ as op) ->
      precheck_manager_contents ctxt chain_id raw_operation op
  | Cons ((Manager_operation _ as op), rest) ->
      precheck_manager_contents ctxt chain_id raw_operation op
      >>=? fun ctxt ->
      precheck_manager_contents_list ctxt chain_id raw_operation rest

let rec apply_manager_contents_list_rec :
    type kind.
    Alpha_context.t ->
    Script_ir_translator.unparsing_mode ->
    baker_hash ->
    Chain_id.t ->
    kind Kind.manager contents_list ->
    (success_or_failure * kind Kind.manager contents_result_list) Lwt.t =
 fun ctxt mode baker chain_id contents_list ->
  let level = Level.current ctxt in
  match contents_list with
  | Single (Manager_operation {source; fee; _} as op) ->
      let source = Contract.implicit_contract source in
      apply_manager_contents ctxt mode chain_id op
      >|= fun (ctxt_result, operation_result, internal_operation_results) ->
      let result =
        Manager_operation_result
          {
            balance_updates =
              Receipt.cleanup_balance_updates
                [ (Contract source, Debited fee);
                  (Fees (baker, level.cycle), Credited fee) ];
            operation_result;
            internal_operation_results;
          }
      in
      (ctxt_result, Single_result result)
  | Cons ((Manager_operation {source; fee; _} as op), rest) -> (
      let source = Contract.implicit_contract source in
      apply_manager_contents ctxt mode chain_id op
      >>= function
      | (Failure, operation_result, internal_operation_results) ->
          let result =
            Manager_operation_result
              {
                balance_updates =
                  Receipt.cleanup_balance_updates
                    [ (Contract source, Debited fee);
                      (Fees (baker, level.cycle), Credited fee) ];
                operation_result;
                internal_operation_results;
              }
          in
          Lwt.return
            (Failure, Cons_result (result, mark_skipped ~baker level rest))
      | (Success ctxt, operation_result, internal_operation_results) ->
          let result =
            Manager_operation_result
              {
                balance_updates =
                  Receipt.cleanup_balance_updates
                    [ (Contract source, Debited fee);
                      (Fees (baker, level.cycle), Credited fee) ];
                operation_result;
                internal_operation_results;
              }
          in
          apply_manager_contents_list_rec ctxt mode baker chain_id rest
          >|= fun (ctxt_result, results) ->
          (ctxt_result, Cons_result (result, results)) )

let mark_backtracked results =
  let mark_manager_operation_result :
      type kind. kind manager_operation_result -> kind manager_operation_result
      = function
    | (Failed _ | Skipped _ | Backtracked _) as result ->
        result
    | Applied (Reveal_result _) as result ->
        result
    | Applied result ->
        Backtracked (result, None)
  in
  let mark_baker_operation_result :
      type kind. kind baker_operation_result -> kind baker_operation_result =
    function
    | (Failed _ | Skipped _ | Backtracked _) as result ->
        result
    | Applied result ->
        Backtracked (result, None)
  in
  let mark_internal_operation_results = function
    | Internal_manager_operation_result (kind, result) ->
        Internal_manager_operation_result
          (kind, mark_manager_operation_result result)
    | Internal_baker_operation_result (kind, result) ->
        Internal_baker_operation_result
          (kind, mark_baker_operation_result result)
  in
  let rec mark_contents_list :
      type kind.
      kind Kind.manager contents_result_list ->
      kind Kind.manager contents_result_list = function
    | Single_result (Manager_operation_result op) ->
        Single_result
          (Manager_operation_result
             {
               balance_updates = op.balance_updates;
               operation_result =
                 mark_manager_operation_result op.operation_result;
               internal_operation_results =
                 List.map
                   mark_internal_operation_results
                   op.internal_operation_results;
             })
    | Cons_result (Manager_operation_result op, rest) ->
        Cons_result
          ( Manager_operation_result
              {
                balance_updates = op.balance_updates;
                operation_result =
                  mark_manager_operation_result op.operation_result;
                internal_operation_results =
                  List.map
                    mark_internal_operation_results
                    op.internal_operation_results;
              },
            mark_contents_list rest )
  in
  mark_contents_list results
  [@@coq_axiom "non-top-level mutual recursion"]

let apply_manager_contents_list ctxt mode baker chain_id contents_list =
  apply_manager_contents_list_rec ctxt mode baker chain_id contents_list
  >>= fun (ctxt_result, results) ->
  match ctxt_result with
  | Failure ->
      Lwt.return (ctxt (* backtracked *), mark_backtracked results)
  | Success ctxt ->
      Lazy_storage.cleanup_temporaries ctxt >|= fun ctxt -> (ctxt, results)

let apply_contents_list (type kind) ctxt chain_id mode pred_block baker
    (operation : kind operation) (contents_list : kind contents_list) :
    (context * kind contents_result_list) tzresult Lwt.t =
  match contents_list with
  | Single (Endorsement {level}) ->
      let block = operation.shell.branch in
      error_unless
        (Block_hash.equal block pred_block)
        (Wrong_endorsement_predecessor (pred_block, block))
      >>?= fun () ->
      let current_level = (Level.current ctxt).level in
      error_unless
        Raw_level.(succ level = current_level)
        Invalid_endorsement_level
      >>?= fun () ->
      Baking.check_endorsement_rights ctxt chain_id operation
      >>=? fun (baker, slots, used) ->
      if used then fail (Duplicate_endorsement baker)
      else
        let ctxt = record_endorsement ctxt baker in
        let gap = List.length slots in
        Tez.(Constants.endorsement_security_deposit ctxt *? Int64.of_int gap)
        >>?= fun deposit ->
        Baker.freeze_deposit ctxt baker deposit
        >>=? fun ctxt ->
        Global.get_block_priority ctxt
        >>=? fun block_priority ->
        Baking.endorsing_reward ctxt ~block_priority gap
        >>?= fun reward ->
        Baker.freeze_rewards ctxt baker reward
        >|=? fun ctxt ->
        let level = Level.from_raw ctxt level in
        ( ctxt,
          Single_result
            (Endorsement_result
               {
                 balance_updates =
                   Receipt.cleanup_balance_updates
                     [ ( Contract (Contract.baker_contract baker),
                         Debited deposit );
                       (Deposits (baker, level.cycle), Credited deposit);
                       (Rewards (baker, level.cycle), Credited reward) ];
                 baker;
                 slots;
               }) )
  | Single (Seed_nonce_revelation {level; nonce}) ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce
      >>=? fun ctxt ->
      let seed_nonce_revelation_tip =
        Constants.seed_nonce_revelation_tip ctxt
      in
      Lwt.return
        ( add_rewards ctxt seed_nonce_revelation_tip
        >|? fun ctxt ->
        ( ctxt,
          Single_result
            (Seed_nonce_revelation_result
               [ ( Rewards (baker, level.cycle),
                   Credited seed_nonce_revelation_tip ) ]) ) )
  | Single (Double_endorsement_evidence _ as evidence) ->
      Cheating_proofs.prove_double_endorsement ctxt chain_id evidence
      >>=? fun (level, convicted_baker) ->
      Baker.Proof.mem ctxt convicted_baker level.level
      >>=? fun already_exists ->
      error_when already_exists Double_injection_of_evidence
      >>?= fun () ->
      Baker.has_frozen_balance ctxt convicted_baker level.cycle
      >>=? fun valid ->
      error_unless valid Unrequired_evidence
      >>?= fun () ->
      Baker.punish ctxt convicted_baker level.cycle
      >>=? fun (ctxt, balance) ->
      Tez.(balance.deposit +? balance.fees)
      >>?= fun burned ->
      let reward =
        match Tez.(burned /? 2L) with Ok v -> v | Error _ -> Tez.zero
      in
      add_rewards ctxt reward
      >>?= fun ctxt ->
      Baker.Proof.add ctxt convicted_baker level.level
      >|=? fun ctxt ->
      let current_cycle = (Level.current ctxt).cycle in
      ( ctxt,
        Single_result
          (Double_endorsement_evidence_result
             (Receipt.cleanup_balance_updates
                [ ( Deposits (convicted_baker, level.cycle),
                    Debited balance.deposit );
                  (Fees (convicted_baker, level.cycle), Debited balance.fees);
                  ( Rewards (convicted_baker, level.cycle),
                    Debited balance.rewards );
                  (Rewards (baker, current_cycle), Credited reward) ])) )
  | Single (Double_baking_evidence _ as evidence) ->
      Cheating_proofs.prove_double_baking ctxt chain_id evidence
      >>=? fun (level, convicted_baker) ->
      Baker.Proof.mem ctxt convicted_baker level.level
      >>=? fun already_exists ->
      error_when already_exists Double_injection_of_evidence
      >>?= fun () ->
      Baker.has_frozen_balance ctxt convicted_baker level.cycle
      >>=? fun valid ->
      error_unless valid Unrequired_evidence
      >>?= fun () ->
      Baker.punish ctxt convicted_baker level.cycle
      >>=? fun (ctxt, balance) ->
      Tez.(balance.deposit +? balance.fees)
      >>?= fun burned ->
      let reward =
        match Tez.(burned /? 2L) with Ok v -> v | Error _ -> Tez.zero
      in
      add_rewards ctxt reward
      >>?= fun ctxt ->
      Baker.Proof.add ctxt convicted_baker level.level
      >|=? fun ctxt ->
      let current_cycle = (Level.current ctxt).cycle in
      ( ctxt,
        Single_result
          (Double_baking_evidence_result
             (Receipt.cleanup_balance_updates
                [ ( Deposits (convicted_baker, level.cycle),
                    Debited balance.deposit );
                  (Fees (convicted_baker, level.cycle), Debited balance.fees);
                  ( Rewards (convicted_baker, level.cycle),
                    Debited balance.rewards );
                  (Rewards (baker, current_cycle), Credited reward) ])) )
  | Single (Activate_account {id = pkh; activation_code}) -> (
      let blinded_pkh =
        Blinded_public_key_hash.of_ed25519_pkh activation_code pkh
      in
      Commitment.get_opt ctxt blinded_pkh
      >>=? function
      | None ->
          fail (Invalid_activation {pkh})
      | Some amount ->
          Commitment.delete ctxt blinded_pkh
          >>=? fun ctxt ->
          let contract = Contract.implicit_contract (Signature.Ed25519 pkh) in
          Contract.(credit ctxt contract amount)
          >|=? fun ctxt ->
          ( ctxt,
            Single_result
              (Activate_account_result [(Contract contract, Credited amount)])
          ) )
  | Single (Proposals {source; period; proposals}) ->
      Roll.delegate_pubkey ctxt source
      >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation
      >>?= fun () ->
      let level = Level.current ctxt in
      error_unless
        Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period))
      >>?= fun () ->
      Amendment.record_proposals ctxt source proposals
      >|=? fun ctxt -> (ctxt, Single_result Proposals_result)
  | Single (Ballot {source; period; proposal; ballot}) ->
      Roll.delegate_pubkey ctxt source
      >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation
      >>?= fun () ->
      let level = Level.current ctxt in
      error_unless
        Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period))
      >>?= fun () ->
      Amendment.record_ballot ctxt source proposal ballot
      >|=? fun ctxt -> (ctxt, Single_result Ballot_result)
  | Single (Failing_noop _) ->
      (* Failing_noop _ always fails *)
      fail Failing_noop_error
  | Single (Manager_operation _) as op ->
      precheck_manager_contents_list ctxt chain_id operation op
      >>=? fun ctxt ->
      apply_manager_contents_list ctxt mode baker chain_id op >|= ok
  | Cons (Manager_operation _, _) as op ->
      precheck_manager_contents_list ctxt chain_id operation op
      >>=? fun ctxt ->
      apply_manager_contents_list ctxt mode baker chain_id op >|= ok

let apply_operation ctxt chain_id mode pred_block baker hash operation =
  let ctxt = Contract.init_origination_nonce ctxt hash in
  apply_contents_list
    ctxt
    chain_id
    mode
    pred_block
    baker
    operation
    operation.protocol_data.contents
  >|=? fun (ctxt, result) ->
  let ctxt = Gas.set_unlimited ctxt in
  let ctxt = Contract.unset_origination_nonce ctxt in
  (ctxt, {contents = result})

let may_snapshot_roll ctxt =
  let level = Alpha_context.Level.current ctxt in
  let blocks_per_roll_snapshot = Constants.blocks_per_roll_snapshot ctxt in
  if
    Compare.Int32.equal
      (Int32.rem level.cycle_position blocks_per_roll_snapshot)
      (Int32.pred blocks_per_roll_snapshot)
  then Alpha_context.Roll.snapshot_rolls ctxt
  else return ctxt

let may_start_new_cycle ctxt =
  Baking.dawn_of_a_new_cycle ctxt
  >>=? function
  | None ->
      return (ctxt, [], [])
  | Some last_cycle ->
      Seed.cycle_end ctxt last_cycle
      >>=? fun (ctxt, unrevealed) ->
      Roll.cycle_end ctxt last_cycle
      >>=? fun ctxt ->
      Baker.cycle_end ctxt last_cycle unrevealed
      >>=? fun (ctxt, update_balances, deactivated) ->
      Bootstrap.cycle_end ctxt last_cycle
      >|=? fun ctxt -> (ctxt, update_balances, deactivated)

let begin_full_construction ctxt pred_timestamp protocol_data =
  Alpha_context.Global.set_block_priority
    ctxt
    protocol_data.Block_header.priority
  >>=? fun ctxt ->
  Baking.check_baking_rights ctxt protocol_data pred_timestamp
  >>=? fun (baker, block_delay) ->
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None ->
      assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level
      >|=? fun rights ->
      let ctxt = init_endorsements ctxt rights in
      (ctxt, protocol_data, baker, block_delay)

let begin_partial_construction ctxt =
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None ->
      assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level
      >|=? fun rights -> init_endorsements ctxt rights

let begin_application ctxt chain_id block_header pred_timestamp =
  Alpha_context.Global.set_block_priority
    ctxt
    block_header.Block_header.protocol_data.contents.priority
  >>=? fun ctxt ->
  let current_level = Alpha_context.Level.current ctxt in
  Baking.check_proof_of_work_stamp ctxt block_header
  >>=? fun () ->
  Baking.check_fitness_gap ctxt block_header
  >>?= fun () ->
  Baking.check_baking_rights
    ctxt
    block_header.protocol_data.contents
    pred_timestamp
  >>=? fun (baker, block_delay) ->
  Baking.check_signature ctxt block_header chain_id baker
  >>=? fun () ->
  let has_commitment =
    match block_header.protocol_data.contents.seed_nonce_hash with
    | None ->
        false
    | Some _ ->
        true
  in
  error_unless
    Compare.Bool.(has_commitment = current_level.expected_commitment)
    (Invalid_commitment {expected = current_level.expected_commitment})
  >>?= fun () ->
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None ->
      assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level
      >|=? fun rights ->
      let ctxt = init_endorsements ctxt rights in
      (ctxt, baker, block_delay)

let check_minimum_endorsements ctxt protocol_data block_delay
    included_endorsements =
  let minimum = Baking.minimum_allowed_endorsements ctxt ~block_delay in
  let timestamp = Timestamp.current ctxt in
  error_unless
    Compare.Int.(included_endorsements >= minimum)
    (Not_enough_endorsements_for_priority
       {
         required = minimum;
         priority = protocol_data.Block_header.priority;
         endorsements = included_endorsements;
         timestamp;
       })

let finalize_application ctxt protocol_data baker ~block_delay
    migration_balance_updates =
  let included_endorsements = included_endorsements ctxt in
  check_minimum_endorsements
    ctxt
    protocol_data
    block_delay
    included_endorsements
  >>?= fun () ->
  let deposit = Constants.block_security_deposit ctxt in
  add_deposit ctxt baker deposit
  >>?= fun ctxt ->
  Baking.baking_reward
    ctxt
    ~block_priority:protocol_data.priority
    ~included_endorsements
  >>?= fun reward ->
  add_rewards ctxt reward
  >>?= fun ctxt ->
  Baker_hash.Map.fold
    (fun baker deposit ctxt ->
      ctxt >>=? fun ctxt -> Baker.freeze_deposit ctxt baker deposit)
    (get_deposits ctxt)
    (return ctxt)
  >>=? fun ctxt ->
  (* end of level (from this point nothing should fail) *)
  let fees = Alpha_context.get_fees ctxt in
  Baker.freeze_fees ctxt baker fees
  >>=? fun ctxt ->
  let rewards = Alpha_context.get_rewards ctxt in
  Baker.freeze_rewards ctxt baker rewards
  >>=? fun ctxt ->
  ( match protocol_data.Block_header.seed_nonce_hash with
  | None ->
      return ctxt
  | Some nonce_hash ->
      Nonce.record_hash ctxt {nonce_hash; baker; rewards; fees} )
  >>=? fun ctxt ->
  (* end of cycle *)
  may_snapshot_roll ctxt
  >>=? fun ctxt ->
  may_start_new_cycle ctxt
  >>=? fun (ctxt, balance_updates, deactivated) ->
  let balance_updates = migration_balance_updates @ balance_updates in
  Amendment.may_start_new_voting_period ctxt
  >>=? fun ctxt ->
  let cycle = (Level.current ctxt).cycle in
  let balance_updates =
    Receipt.(
      cleanup_balance_updates
        ( [ (Contract (Contract.baker_contract baker), Debited deposit);
            (Deposits (baker, cycle), Credited deposit);
            (Rewards (baker, cycle), Credited reward) ]
        @ balance_updates ))
  in
  let consumed_gas =
    Z.sub
      (Constants.hard_gas_limit_per_block ctxt)
      (Alpha_context.Gas.block_level ctxt)
  in
  Alpha_context.Vote.get_current_period_kind ctxt
  >|=? fun voting_period_kind ->
  let receipt =
    Apply_results.
      {
        baker;
        level = Level.current ctxt;
        voting_period_kind;
        nonce_hash = protocol_data.seed_nonce_hash;
        consumed_gas;
        deactivated;
        balance_updates;
      }
  in
  (ctxt, receipt)
