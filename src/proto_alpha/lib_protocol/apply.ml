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

type error += Wrong_voting_period of int32 * int32

(* `Temporary *)

type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t

(* `Temporary *)

type error += Duplicate_endorsement of Signature.Public_key_hash.t

(* `Branch *)

type error += Invalid_endorsement_level

type error += Invalid_endorsement_wrapper

type error += Invalid_commitment of {expected : bool}

type error += Internal_operation_replay of packed_internal_operation

type error += Invalid_double_endorsement_evidence (* `Permanent *)

type error +=
  | Inconsistent_double_endorsement_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }

(* `Permanent *)

type error += Unwrapped_endorsement (* `Permanent *)

type error += Unrequired_double_endorsement_evidence (* `Branch*)

type error +=
  | Too_early_double_endorsement_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }

(* `Temporary *)

type error +=
  | Outdated_double_endorsement_evidence of {
      level : Raw_level.t;
      last : Raw_level.t;
    }

(* `Permanent *)

type error +=
  | Invalid_double_baking_evidence of {
      hash1 : Block_hash.t;
      level1 : Int32.t;
      hash2 : Block_hash.t;
      level2 : Int32.t;
    }

(* `Permanent *)

type error +=
  | Inconsistent_double_baking_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }

(* `Permanent *)

type error += Unrequired_double_baking_evidence (* `Branch*)

type error +=
  | Too_early_double_baking_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }

(* `Temporary *)

type error +=
  | Outdated_double_baking_evidence of {level : Raw_level.t; last : Raw_level.t}

(* `Permanent *)

type error += Invalid_activation of {pkh : Ed25519.Public_key_hash.t}

type error += Multiple_revelation

type error += Gas_quota_exceeded_init_deserialize (* Permanent *)

type error += (* `Permanent *) Inconsistent_sources

type error += (* `Permanent *) Failing_noop_error

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
    (function Wrong_endorsement_predecessor (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_endorsement_predecessor (e, p)) ;
  register_error_kind
    `Temporary
    ~id:"operation.wrong_voting_period"
    ~title:"Wrong voting period"
    ~description:
      "Trying to include a proposal or ballot meant for another voting period"
    ~pp:(fun ppf (e, p) ->
      Format.fprintf ppf "Wrong voting period %ld, current is %ld" p e)
    Data_encoding.(
      obj2 (req "current_index" int32) (req "provided_index" int32))
    (function Wrong_voting_period (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_voting_period (e, p)) ;
  register_error_kind
    `Branch
    ~id:"operation.duplicate_endorsement"
    ~title:"Duplicate endorsement"
    ~description:"Two endorsements received from same delegate"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "Duplicate endorsement from delegate %a (possible replay attack)."
        Signature.Public_key_hash.pp_short
        k)
    Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
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
    `Temporary
    ~id:"operation.invalid_endorsement_wrapper"
    ~title:"Unexpected wrapper in endorsement"
    ~description:
      "The wrapper of an endorsement is inconsistent with the endorsement it \
       wraps."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The endorsement wrapper announces a block hash different from its \
         wrapped endorsement, or bears a signature.")
    Data_encoding.unit
    (function Invalid_endorsement_wrapper -> Some () | _ -> None)
    (fun () -> Invalid_endorsement_wrapper) ;
  register_error_kind
    `Temporary
    ~id:"operation.unwrapped_endorsement"
    ~title:"Unwrapped endorsement"
    ~description:
      "A legacy endorsement has been applied without its required slot-bearing \
       wrapper."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A legacy endorsement has been applied without its required \
         slot-bearing wrapper operation.")
    Data_encoding.unit
    (function Unwrapped_endorsement -> Some () | _ -> None)
    (fun () -> Unwrapped_endorsement) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_commitment"
    ~title:"Invalid commitment in block header"
    ~description:"The block header has invalid commitment."
    ~pp:(fun ppf expected ->
      if expected then
        Format.fprintf ppf "Missing seed's nonce commitment in block header."
      else
        Format.fprintf ppf "Unexpected seed's nonce commitment in block header.")
    Data_encoding.(obj1 (req "expected" bool))
    (function Invalid_commitment {expected} -> Some expected | _ -> None)
    (fun expected -> Invalid_commitment {expected}) ;
  register_error_kind
    `Permanent
    ~id:"internal_operation_replay"
    ~title:"Internal operation replay"
    ~description:"An internal operation was emitted twice by a script"
    ~pp:(fun ppf (Internal_operation {nonce; _}) ->
      Format.fprintf
        ppf
        "Internal operation %d was emitted twice by a script"
        nonce)
    Operation.internal_operation_encoding
    (function Internal_operation_replay op -> Some op | _ -> None)
    (fun op -> Internal_operation_replay op) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_double_endorsement_evidence"
    ~title:"Invalid double endorsement evidence"
    ~description:"A double-endorsement evidence is malformed"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Malformed double-endorsement evidence")
    Data_encoding.empty
    (function Invalid_double_endorsement_evidence -> Some () | _ -> None)
    (fun () -> Invalid_double_endorsement_evidence) ;
  register_error_kind
    `Permanent
    ~id:"block.inconsistent_double_endorsement_evidence"
    ~title:"Inconsistent double endorsement evidence"
    ~description:
      "A double-endorsement evidence is inconsistent  (two distinct delegates)"
    ~pp:(fun ppf (delegate1, delegate2) ->
      Format.fprintf
        ppf
        "Inconsistent double-endorsement evidence  (distinct delegate: %a and \
         %a)"
        Signature.Public_key_hash.pp_short
        delegate1
        Signature.Public_key_hash.pp_short
        delegate2)
    Data_encoding.(
      obj2
        (req "delegate1" Signature.Public_key_hash.encoding)
        (req "delegate2" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_double_endorsement_evidence {delegate1; delegate2} ->
          Some (delegate1, delegate2)
      | _ -> None)
    (fun (delegate1, delegate2) ->
      Inconsistent_double_endorsement_evidence {delegate1; delegate2}) ;
  register_error_kind
    `Branch
    ~id:"block.unrequired_double_endorsement_evidence"
    ~title:"Unrequired double endorsement evidence"
    ~description:"A double-endorsement evidence is unrequired"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A valid double-endorsement operation cannot  be applied: the \
         associated delegate  has previously been denounced in this cycle.")
    Data_encoding.empty
    (function Unrequired_double_endorsement_evidence -> Some () | _ -> None)
    (fun () -> Unrequired_double_endorsement_evidence) ;
  register_error_kind
    `Temporary
    ~id:"block.too_early_double_endorsement_evidence"
    ~title:"Too early double endorsement evidence"
    ~description:"A double-endorsement evidence is in the future"
    ~pp:(fun ppf (level, current) ->
      Format.fprintf
        ppf
        "A double-endorsement evidence is in the future  (current level: %a, \
         endorsement level: %a)"
        Raw_level.pp
        current
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "current" Raw_level.encoding))
    (function
      | Too_early_double_endorsement_evidence {level; current} ->
          Some (level, current)
      | _ -> None)
    (fun (level, current) ->
      Too_early_double_endorsement_evidence {level; current}) ;
  register_error_kind
    `Permanent
    ~id:"block.outdated_double_endorsement_evidence"
    ~title:"Outdated double endorsement evidence"
    ~description:"A double-endorsement evidence is outdated."
    ~pp:(fun ppf (level, last) ->
      Format.fprintf
        ppf
        "A double-endorsement evidence is outdated  (last acceptable level: \
         %a, endorsement level: %a)"
        Raw_level.pp
        last
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "last" Raw_level.encoding))
    (function
      | Outdated_double_endorsement_evidence {level; last} -> Some (level, last)
      | _ -> None)
    (fun (level, last) -> Outdated_double_endorsement_evidence {level; last}) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_double_baking_evidence"
    ~title:"Invalid double baking evidence"
    ~description:
      "A double-baking evidence is inconsistent  (two distinct level)"
    ~pp:(fun ppf (hash1, level1, hash2, level2) ->
      Format.fprintf
        ppf
        "Invalid double-baking evidence (hash: %a and %a, levels: %ld and %ld)"
        Block_hash.pp
        hash1
        Block_hash.pp
        hash2
        level1
        level2)
    Data_encoding.(
      obj4
        (req "hash1" Block_hash.encoding)
        (req "level1" int32)
        (req "hash2" Block_hash.encoding)
        (req "level2" int32))
    (function
      | Invalid_double_baking_evidence {hash1; level1; hash2; level2} ->
          Some (hash1, level1, hash2, level2)
      | _ -> None)
    (fun (hash1, level1, hash2, level2) ->
      Invalid_double_baking_evidence {hash1; level1; hash2; level2}) ;
  register_error_kind
    `Permanent
    ~id:"block.inconsistent_double_baking_evidence"
    ~title:"Inconsistent double baking evidence"
    ~description:
      "A double-baking evidence is inconsistent  (two distinct delegates)"
    ~pp:(fun ppf (delegate1, delegate2) ->
      Format.fprintf
        ppf
        "Inconsistent double-baking evidence  (distinct delegate: %a and %a)"
        Signature.Public_key_hash.pp_short
        delegate1
        Signature.Public_key_hash.pp_short
        delegate2)
    Data_encoding.(
      obj2
        (req "delegate1" Signature.Public_key_hash.encoding)
        (req "delegate2" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_double_baking_evidence {delegate1; delegate2} ->
          Some (delegate1, delegate2)
      | _ -> None)
    (fun (delegate1, delegate2) ->
      Inconsistent_double_baking_evidence {delegate1; delegate2}) ;
  register_error_kind
    `Branch
    ~id:"block.unrequired_double_baking_evidence"
    ~title:"Unrequired double baking evidence"
    ~description:"A double-baking evidence is unrequired"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A valid double-baking operation cannot  be applied: the associated \
         delegate  has previously been denounced in this cycle.")
    Data_encoding.empty
    (function Unrequired_double_baking_evidence -> Some () | _ -> None)
    (fun () -> Unrequired_double_baking_evidence) ;
  register_error_kind
    `Temporary
    ~id:"block.too_early_double_baking_evidence"
    ~title:"Too early double baking evidence"
    ~description:"A double-baking evidence is in the future"
    ~pp:(fun ppf (level, current) ->
      Format.fprintf
        ppf
        "A double-baking evidence is in the future  (current level: %a, baking \
         level: %a)"
        Raw_level.pp
        current
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "current" Raw_level.encoding))
    (function
      | Too_early_double_baking_evidence {level; current} ->
          Some (level, current)
      | _ -> None)
    (fun (level, current) -> Too_early_double_baking_evidence {level; current}) ;
  register_error_kind
    `Permanent
    ~id:"block.outdated_double_baking_evidence"
    ~title:"Outdated double baking evidence"
    ~description:"A double-baking evidence is outdated."
    ~pp:(fun ppf (level, last) ->
      Format.fprintf
        ppf
        "A double-baking evidence is outdated  (last acceptable level: %a, \
         baking level: %a)"
        Raw_level.pp
        last
        Raw_level.pp
        level)
    Data_encoding.(
      obj2 (req "level" Raw_level.encoding) (req "last" Raw_level.encoding))
    (function
      | Outdated_double_baking_evidence {level; last} -> Some (level, last)
      | _ -> None)
    (fun (level, last) -> Outdated_double_baking_evidence {level; last}) ;
  register_error_kind
    `Permanent
    ~id:"operation.invalid_activation"
    ~title:"Invalid activation"
    ~description:
      "The given key and secret do not correspond to any existing preallocated \
       contract"
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
      "Gas limit was not high enough to deserialize the transaction parameters \
       or origination script code or initial storage, making the operation \
       impossible to parse within the provided gas bounds."
    Data_encoding.empty
    (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
    (fun () -> Gas_quota_exceeded_init_deserialize) ;
  register_error_kind
    `Permanent
    ~id:"operation.inconsistent_sources"
    ~title:"Inconsistent sources in operation pack"
    ~description:
      "The operation pack includes operations from different sources."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The operation pack includes operations from different sources.")
    Data_encoding.empty
    (function Inconsistent_sources -> Some () | _ -> None)
    (fun () -> Inconsistent_sources) ;
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
    (fun () -> Failing_noop_error)

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
    (context
    * kind successful_manager_operation_result
    * packed_internal_operation list)
    tzresult
    Lwt.t =
 fun ctxt mode ~payer ~source ~chain_id ~internal operation ->
  let before_operation =
    (* This context is not used for backtracking. Only to compute
         gas consumption and originations for the operation result. *)
    ctxt
  in
  Contract.must_exist ctxt source >>=? fun () ->
  Gas.consume ctxt Michelson_v1_gas.Cost_of.manager_operation >>?= fun ctxt ->
  match operation with
  | Reveal _ ->
      return
        (* No-op: action already performed by `precheck_manager_contents`. *)
        ( ctxt,
          (Reveal_result
             {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt}
            : kind successful_manager_operation_result),
          [] )
  | Transaction {amount; parameters; destination; entrypoint} -> (
      Script.force_decode_in_context ctxt parameters
      (* [note]: for toplevel ops, cost is nil since the lazy value has
                 already been forced at precheck. Otherwise fail early if not
                 enough gas for complete deserialization cost *)
      >>?=
      fun (parameter, ctxt) ->
      Contract.spend ctxt source amount >>=? fun ctxt ->
      (match Contract.is_implicit destination with
      | None -> return (ctxt, [], false)
      | Some _ -> (
          Contract.allocated ctxt destination >>=? function
          | true -> return (ctxt, [], false)
          | false ->
              Lwt.return
                ( Fees.origination_burn ctxt >|? fun (ctxt, origination_burn) ->
                  ( ctxt,
                    [
                      Receipt.
                        ( Contract payer,
                          Debited origination_burn,
                          Block_application );
                    ],
                    true ) )))
      >>=? fun (ctxt, maybe_burn_balance_update, allocated_destination_contract)
        ->
      Contract.credit ctxt destination amount >>=? fun ctxt ->
      Contract.get_script ctxt destination >>=? fun (ctxt, script) ->
      match script with
      | None ->
          Lwt.return
            ( ( (match entrypoint with
                | "default" -> ok_unit
                | entrypoint ->
                    error (Script_tc_errors.No_such_entrypoint entrypoint))
              >>? fun () ->
                match Micheline.root parameter with
                | Prim (_, D_Unit, [], _) ->
                    (* Allow [Unit] parameter to non-scripted contracts. *)
                    ok ctxt
                | _ ->
                    error
                      (Script_interpreter.Bad_contract_parameter destination) )
            >|? fun ctxt ->
              let result =
                Transaction_result
                  {
                    storage = None;
                    lazy_storage_diff = None;
                    balance_updates =
                      Receipt.(
                        cleanup_balance_updates
                          [
                            (Contract source, Debited amount, Block_application);
                            ( Contract destination,
                              Credited amount,
                              Block_application );
                          ]
                        @ maybe_burn_balance_update);
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
            ~internal
          >>=? fun {ctxt; storage; lazy_storage_diff; operations} ->
          Contract.update_script_storage
            ctxt
            destination
            storage
            lazy_storage_diff
          >>=? fun ctxt ->
          Fees.record_paid_storage_space ctxt destination
          >>=? fun (ctxt, new_size, paid_storage_size_diff, fees) ->
          Contract.originated_from_current_nonce
            ~since:before_operation
            ~until:ctxt
          >|=? fun originated_contracts ->
          let result =
            Transaction_result
              {
                storage = Some storage;
                lazy_storage_diff;
                balance_updates =
                  Receipt.cleanup_balance_updates
                    [
                      (Contract payer, Debited fees, Block_application);
                      (Contract source, Debited amount, Block_application);
                      (Contract destination, Credited amount, Block_application);
                    ];
                originated_contracts;
                consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
                storage_size = new_size;
                paid_storage_size_diff;
                allocated_destination_contract;
              }
          in
          (ctxt, result, operations))
  | Origination {delegate; script; preorigination; credit} ->
      Script.force_decode_in_context ctxt script.storage
      (* see [note] *)
      >>?= fun (_unparsed_storage, ctxt) ->
      Script.force_decode_in_context ctxt script.code
      (* see [note] *)
      >>?= fun (unparsed_code, ctxt) ->
      Script_ir_translator.parse_script
        ctxt
        ~legacy:false
        ~allow_forged_in_storage:internal
        script
      >>=? fun (Ex_script parsed_script, ctxt) ->
      let views_result =
        Script_ir_translator.typecheck_views
          ctxt
          ~legacy:false
          parsed_script.storage_type
          parsed_script.views
      in
      trace
        (Script_tc_errors.Ill_typed_contract (unparsed_code, []))
        views_result
      >>=? fun ctxt ->
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
      Contract.spend ctxt source credit >>=? fun ctxt ->
      (match preorigination with
      | Some contract ->
          assert internal ;
          (* The preorigination field is only used to early return
                 the address of an originated contract in Michelson.
                 It cannot come from the outside. *)
          ok (ctxt, contract)
      | None -> Contract.fresh_contract_from_current_nonce ctxt)
      >>?= fun (ctxt, contract) ->
      Contract.originate
        ctxt
        contract
        ~delegate
        ~balance:credit
        ~script:(script, lazy_storage_diff)
      >>=? fun ctxt ->
      Fees.origination_burn ctxt >>?= fun (ctxt, origination_burn) ->
      Fees.record_paid_storage_space ctxt contract
      >|=? fun (ctxt, size, paid_storage_size_diff, fees) ->
      let result =
        Origination_result
          {
            lazy_storage_diff;
            balance_updates =
              Receipt.cleanup_balance_updates
                [
                  (Contract payer, Debited fees, Block_application);
                  (Contract payer, Debited origination_burn, Block_application);
                  (Contract source, Debited credit, Block_application);
                  (Contract contract, Credited credit, Block_application);
                ];
            originated_contracts = [contract];
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size = size;
            paid_storage_size_diff;
          }
      in
      (ctxt, result, [])
  | Delegation delegate ->
      Delegate.set ctxt source delegate >|=? fun ctxt ->
      ( ctxt,
        Delegation_result
          {consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt},
        [] )

type success_or_failure = Success of context | Failure

let apply_internal_manager_operations ctxt mode ~payer ~chain_id ops =
  let[@coq_struct "ctxt"] rec apply ctxt applied worklist =
    match worklist with
    | [] -> Lwt.return (Success ctxt, List.rev applied)
    | Internal_operation ({source; operation; nonce} as op) :: rest -> (
        (if internal_nonce_already_recorded ctxt nonce then
         fail (Internal_operation_replay (Internal_operation op))
        else
          let ctxt = record_internal_nonce ctxt nonce in
          apply_manager_operation_content
            ctxt
            mode
            ~source
            ~payer
            ~chain_id
            ~internal:true
            operation)
        >>= function
        | Error errors ->
            let result =
              Internal_operation_result
                (op, Failed (manager_kind op.operation, errors))
            in
            let skipped =
              List.rev_map
                (fun (Internal_operation op) ->
                  Internal_operation_result
                    (op, Skipped (manager_kind op.operation)))
                rest
            in
            Lwt.return (Failure, List.rev (skipped @ result :: applied))
        | Ok (ctxt, result, emitted) ->
            apply
              ctxt
              (Internal_operation_result (op, Applied result) :: applied)
              (emitted @ rest))
  in
  apply ctxt [] ops

let precheck_manager_contents (type kind) ctxt (op : kind Kind.manager contents)
    : context tzresult Lwt.t =
  let[@coq_match_with_default] (Manager_operation
                                 {
                                   source;
                                   fee;
                                   counter;
                                   operation;
                                   gas_limit;
                                   storage_limit;
                                 }) =
    op
  in
  Gas.consume_limit_in_block ctxt gas_limit >>?= fun ctxt ->
  let ctxt = Gas.set_limit ctxt gas_limit in
  Fees.check_storage_limit ctxt ~storage_limit >>?= fun () ->
  Contract.must_be_allocated ctxt (Contract.implicit_contract source)
  >>=? fun () ->
  Contract.check_counter_increment ctxt source counter >>=? fun () ->
  (match operation with
  | Reveal pk -> Contract.reveal_manager_key ctxt source pk
  | Transaction {parameters; _} ->
      Lwt.return
      @@ record_trace Gas_quota_exceeded_init_deserialize
      @@ (* Fail early if not enough gas for complete deserialization cost *)
      ( Script.force_decode_in_context ctxt parameters >|? fun (_arg, ctxt) ->
        ctxt )
  | Origination {script; _} ->
      Lwt.return
      @@ record_trace Gas_quota_exceeded_init_deserialize
      @@ (* Fail early if not enough gas for complete deserialization cost *)
      ( Script.force_decode_in_context ctxt script.code >>? fun (_code, ctxt) ->
        Script.force_decode_in_context ctxt script.storage
        >|? fun (_storage, ctxt) -> ctxt )
  | _ -> return ctxt)
  >>=? fun ctxt ->
  Contract.increment_counter ctxt source >>=? fun ctxt ->
  Contract.spend ctxt (Contract.implicit_contract source) fee >>=? fun ctxt ->
  Lwt.return (add_fees ctxt fee)

let apply_manager_contents (type kind) ctxt mode chain_id
    (op : kind Kind.manager contents) :
    (success_or_failure
    * kind manager_operation_result
    * packed_internal_operation_result list)
    Lwt.t =
  let[@coq_match_with_default] (Manager_operation
                                 {
                                   source;
                                   operation;
                                   gas_limit;
                                   storage_limit;
                                   _;
                                 }) =
    op
  in
  (* We do not expose the internal scaling to the users. Instead, we multiply
       the specified gas limit by the internal scaling. *)
  let ctxt = Gas.set_limit ctxt gas_limit in
  let ctxt = Fees.start_counting_storage_fees ctxt in
  let source = Contract.implicit_contract source in
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
          Fees.burn_storage_fees ctxt ~storage_limit ~payer:source >|= function
          | Ok ctxt ->
              ( Success ctxt,
                Applied operation_results,
                internal_operations_results )
          | Error errors ->
              ( Failure,
                Backtracked (operation_results, Some errors),
                internal_operations_results ))
      | (Failure, internal_operations_results) ->
          Lwt.return
            (Failure, Applied operation_results, internal_operations_results))
  | Error errors ->
      Lwt.return (Failure, Failed (manager_kind operation, errors), [])

let skipped_operation_result :
    type kind. kind manager_operation -> kind manager_operation_result =
  function
  | operation -> (
      match operation with
      | Reveal _ ->
          Applied
            (Reveal_result {consumed_gas = Gas.Arith.zero}
              : kind successful_manager_operation_result)
      | _ -> Skipped (manager_kind operation))

let rec mark_skipped :
    type kind.
    baker:Signature.Public_key_hash.t ->
    Level.t ->
    kind Kind.manager contents_list ->
    kind Kind.manager contents_result_list =
 fun ~baker level -> function[@coq_match_with_default]
  | Single (Manager_operation {source; fee; operation; _}) ->
      let source = Contract.implicit_contract source in
      Single_result
        (Manager_operation_result
           {
             balance_updates =
               Receipt.cleanup_balance_updates
                 [
                   (Contract source, Debited fee, Block_application);
                   (Fees (baker, level.cycle), Credited fee, Block_application);
                 ];
             operation_result = skipped_operation_result operation;
             internal_operation_results = [];
           })
  | Cons (Manager_operation {source; fee; operation; _}, rest) ->
      let source = Contract.implicit_contract source in
      Cons_result
        ( Manager_operation_result
            {
              balance_updates =
                Receipt.cleanup_balance_updates
                  [
                    (Contract source, Debited fee, Block_application);
                    (Fees (baker, level.cycle), Credited fee, Block_application);
                  ];
              operation_result = skipped_operation_result operation;
              internal_operation_results = [];
            },
          mark_skipped ~baker level rest )

let rec precheck_manager_contents_list :
    type kind.
    Alpha_context.t -> kind Kind.manager contents_list -> context tzresult Lwt.t
    =
 fun ctxt contents_list ->
  match[@coq_match_with_default] contents_list with
  | Single (Manager_operation _ as op) -> precheck_manager_contents ctxt op
  | Cons ((Manager_operation _ as op), rest) ->
      precheck_manager_contents ctxt op >>=? fun ctxt ->
      precheck_manager_contents_list ctxt rest

let check_manager_signature ctxt chain_id (op : _ Kind.manager contents_list)
    raw_operation =
  (* Currently, the [op] only contains one signature, so
     all operations are required to be from the same manager. This may
     change in the future, allowing several managers to group-sign a
     sequence of transactions. *)
  let check_same_manager (source, source_key) manager =
    match manager with
    | None ->
        (* Consistency already checked by
           [reveal_manager_key] in [precheck_manager_contents]. *)
        ok (source, source_key)
    | Some (manager, manager_key) ->
        if Signature.Public_key_hash.equal source manager then
          ok (source, Option.either manager_key source_key)
        else error Inconsistent_sources
  in
  let rec find_source :
      type kind.
      kind Kind.manager contents_list ->
      (Signature.public_key_hash * Signature.public_key option) option ->
      (Signature.public_key_hash * Signature.public_key option) tzresult =
   fun contents_list manager ->
    let source (type kind) = function[@coq_match_with_default]
      | (Manager_operation {source; operation = Reveal key; _} :
          kind Kind.manager contents) ->
          (source, Some key)
      | Manager_operation {source; _} -> (source, None)
    in
    match contents_list with
    | Single op -> check_same_manager (source op) manager
    | Cons (op, rest) ->
        check_same_manager (source op) manager >>? fun manager ->
        find_source rest (Some manager)
  in
  find_source op None >>?= fun (source, source_key) ->
  (match source_key with
  | Some key -> return key
  | None -> Contract.get_manager_key ctxt source)
  >>=? fun public_key ->
  Lwt.return (Operation.check_signature public_key chain_id raw_operation)

let rec apply_manager_contents_list_rec :
    type kind.
    Alpha_context.t ->
    Script_ir_translator.unparsing_mode ->
    public_key_hash ->
    Chain_id.t ->
    kind Kind.manager contents_list ->
    (success_or_failure * kind Kind.manager contents_result_list) Lwt.t =
 fun ctxt mode baker chain_id contents_list ->
  let level = Level.current ctxt in
  match[@coq_match_with_default] contents_list with
  | Single (Manager_operation {source; fee; _} as op) ->
      let source = Contract.implicit_contract source in
      apply_manager_contents ctxt mode chain_id op
      >|= fun (ctxt_result, operation_result, internal_operation_results) ->
      let result =
        Manager_operation_result
          {
            balance_updates =
              Receipt.cleanup_balance_updates
                [
                  (Contract source, Debited fee, Block_application);
                  (Fees (baker, level.cycle), Credited fee, Block_application);
                ];
            operation_result;
            internal_operation_results;
          }
      in
      (ctxt_result, Single_result result)
  | Cons ((Manager_operation {source; fee; _} as op), rest) -> (
      let source = Contract.implicit_contract source in
      apply_manager_contents ctxt mode chain_id op >>= function
      | (Failure, operation_result, internal_operation_results) ->
          let result =
            Manager_operation_result
              {
                balance_updates =
                  Receipt.cleanup_balance_updates
                    [
                      (Contract source, Debited fee, Block_application);
                      ( Fees (baker, level.cycle),
                        Credited fee,
                        Block_application );
                    ];
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
                    [
                      (Contract source, Debited fee, Block_application);
                      ( Fees (baker, level.cycle),
                        Credited fee,
                        Block_application );
                    ];
                operation_result;
                internal_operation_results;
              }
          in
          apply_manager_contents_list_rec ctxt mode baker chain_id rest
          >|= fun (ctxt_result, results) ->
          (ctxt_result, Cons_result (result, results)))

let mark_backtracked results =
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
  and mark_internal_operation_results (Internal_operation_result (kind, result))
      =
    Internal_operation_result (kind, mark_manager_operation_result result)
  and mark_manager_operation_result :
      type kind. kind manager_operation_result -> kind manager_operation_result
      = function
    | (Failed _ | Skipped _ | Backtracked _) as result -> result
    | Applied (Reveal_result _) as result -> result
    | Applied result -> Backtracked (result, None)
  in
  mark_contents_list results
  [@@coq_axiom_with_reason "non-top-level mutual recursion"]

let apply_manager_contents_list ctxt mode baker chain_id contents_list =
  apply_manager_contents_list_rec ctxt mode baker chain_id contents_list
  >>= fun (ctxt_result, results) ->
  match ctxt_result with
  | Failure -> Lwt.return (ctxt (* backtracked *), mark_backtracked results)
  | Success ctxt ->
      Lazy_storage.cleanup_temporaries ctxt >|= fun ctxt -> (ctxt, results)

let apply_contents_list (type kind) ctxt chain_id mode pred_block baker
    (operation : kind operation) (contents_list : kind contents_list) :
    (context * kind contents_result_list) tzresult Lwt.t =
  match[@coq_match_with_default] contents_list with
  | Single
      (Endorsement_with_slot
        {
          endorsement =
            {
              shell = {branch};
              protocol_data = {contents = Single (Endorsement {level}); _};
            } as unslotted;
          slot;
        }) ->
      if
        (match operation.protocol_data.signature with
        | None -> false
        | Some _ -> true)
        || not (Block_hash.equal operation.shell.branch branch)
      then fail Invalid_endorsement_wrapper
      else
        let operation = unslotted (* shadow the slot box *) in
        let block = operation.shell.branch in
        Baking.check_endorsement_rights ctxt chain_id operation ~slot
        >>=? fun (delegate, slots, used) ->
        error_unless
          (Block_hash.equal block pred_block)
          (Wrong_endorsement_predecessor (pred_block, block))
        >>?= fun () ->
        let current_level = (Level.current ctxt).level in
        error_unless
          Raw_level.(succ level = current_level)
          Invalid_endorsement_level
        >>?= fun () ->
        if used then fail (Duplicate_endorsement delegate)
        else
          let ctxt = record_endorsement ctxt delegate in
          let gap = List.length slots in
          Tez.(Constants.endorsement_security_deposit ctxt *? Int64.of_int gap)
          >>?= fun deposit ->
          Delegate.freeze_deposit ctxt delegate deposit >>=? fun ctxt ->
          Global.get_block_priority ctxt >>=? fun block_priority ->
          Baking.endorsing_reward ctxt ~block_priority gap >>?= fun reward ->
          Delegate.freeze_rewards ctxt delegate reward >|=? fun ctxt ->
          let level = Level.from_raw ctxt level in
          ( ctxt,
            Single_result
              (Endorsement_with_slot_result
                 (Endorsement_result
                    {
                      balance_updates =
                        Receipt.cleanup_balance_updates
                          [
                            ( Contract (Contract.implicit_contract delegate),
                              Debited deposit,
                              Block_application );
                            ( Deposits (delegate, level.cycle),
                              Credited deposit,
                              Block_application );
                            ( Rewards (delegate, level.cycle),
                              Credited reward,
                              Block_application );
                          ];
                      delegate;
                      slots;
                    })) )
  | Single (Endorsement _) -> fail Unwrapped_endorsement
  | Single (Seed_nonce_revelation {level; nonce}) ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun ctxt ->
      let seed_nonce_revelation_tip =
        Constants.seed_nonce_revelation_tip ctxt
      in
      Lwt.return
        ( add_rewards ctxt seed_nonce_revelation_tip >|? fun ctxt ->
          ( ctxt,
            Single_result
              (Seed_nonce_revelation_result
                 [
                   ( Rewards (baker, level.cycle),
                     Credited seed_nonce_revelation_tip,
                     Block_application );
                 ]) ) )
  | Single (Double_endorsement_evidence {op1; op2; slot}) -> (
      match (op1.protocol_data.contents, op2.protocol_data.contents) with
      | (Single (Endorsement e1), Single (Endorsement e2))
        when Raw_level.(e1.level = e2.level)
             && not (Block_hash.equal op1.shell.branch op2.shell.branch) ->
          let level = Level.from_raw ctxt e1.level in
          let oldest_level = Level.last_allowed_fork_level ctxt in
          fail_unless
            Level.(level < Level.current ctxt)
            (Too_early_double_endorsement_evidence
               {level = level.level; current = (Level.current ctxt).level})
          >>=? fun () ->
          fail_unless
            Raw_level.(oldest_level <= level.level)
            (Outdated_double_endorsement_evidence
               {level = level.level; last = oldest_level})
          >>=? fun () ->
          Baking.check_endorsement_rights ctxt chain_id op1 ~slot
          >>=? fun (delegate1, _, _) ->
          Baking.check_endorsement_rights ctxt chain_id op2 ~slot
          >>=? fun (delegate2, _, _) ->
          fail_unless
            (Signature.Public_key_hash.equal delegate1 delegate2)
            (Inconsistent_double_endorsement_evidence {delegate1; delegate2})
          >>=? fun () ->
          Delegate.has_frozen_balance ctxt delegate1 level.cycle
          >>=? fun valid ->
          fail_unless valid Unrequired_double_endorsement_evidence
          >>=? fun () ->
          Delegate.punish ctxt delegate1 level.cycle >>=? fun (ctxt, balance) ->
          Lwt.return Tez.(balance.deposit +? balance.fees) >>=? fun burned ->
          let reward =
            match Tez.(burned /? 2L) with Ok v -> v | Error _ -> Tez.zero
          in
          add_rewards ctxt reward >>?= fun ctxt ->
          let current_cycle = (Level.current ctxt).cycle in
          return
            ( ctxt,
              Single_result
                (Double_endorsement_evidence_result
                   (Receipt.cleanup_balance_updates
                      [
                        ( Deposits (delegate1, level.cycle),
                          Debited balance.deposit,
                          Block_application );
                        ( Fees (delegate1, level.cycle),
                          Debited balance.fees,
                          Block_application );
                        ( Rewards (delegate1, level.cycle),
                          Debited balance.rewards,
                          Block_application );
                        ( Rewards (baker, current_cycle),
                          Credited reward,
                          Block_application );
                      ])) )
      | (_, _) -> fail Invalid_double_endorsement_evidence)
  | Single (Double_baking_evidence {bh1; bh2}) ->
      let hash1 = Block_header.hash bh1 in
      let hash2 = Block_header.hash bh2 in
      fail_unless
        (Compare.Int32.(bh1.shell.level = bh2.shell.level)
        && not (Block_hash.equal hash1 hash2))
        (Invalid_double_baking_evidence
           {hash1; level1 = bh1.shell.level; hash2; level2 = bh2.shell.level})
      >>=? fun () ->
      Lwt.return (Raw_level.of_int32 bh1.shell.level) >>=? fun raw_level ->
      let oldest_level = Level.last_allowed_fork_level ctxt in
      fail_unless
        Raw_level.(raw_level < (Level.current ctxt).level)
        (Too_early_double_baking_evidence
           {level = raw_level; current = (Level.current ctxt).level})
      >>=? fun () ->
      fail_unless
        Raw_level.(oldest_level <= raw_level)
        (Outdated_double_baking_evidence
           {level = raw_level; last = oldest_level})
      >>=? fun () ->
      let level = Level.from_raw ctxt raw_level in
      Roll.baking_rights_owner
        ctxt
        level
        ~priority:bh1.protocol_data.contents.priority
      >>=? fun delegate1 ->
      Baking.check_signature bh1 chain_id delegate1 >>=? fun () ->
      Roll.baking_rights_owner
        ctxt
        level
        ~priority:bh2.protocol_data.contents.priority
      >>=? fun delegate2 ->
      Baking.check_signature bh2 chain_id delegate2 >>=? fun () ->
      fail_unless
        (Signature.Public_key.equal delegate1 delegate2)
        (Inconsistent_double_baking_evidence
           {
             delegate1 = Signature.Public_key.hash delegate1;
             delegate2 = Signature.Public_key.hash delegate2;
           })
      >>=? fun () ->
      let delegate = Signature.Public_key.hash delegate1 in
      Delegate.has_frozen_balance ctxt delegate level.cycle >>=? fun valid ->
      fail_unless valid Unrequired_double_baking_evidence >>=? fun () ->
      Delegate.punish ctxt delegate level.cycle >>=? fun (ctxt, balance) ->
      Tez.(balance.deposit +? balance.fees) >>?= fun burned ->
      let reward =
        match Tez.(burned /? 2L) with Ok v -> v | Error _ -> Tez.zero
      in
      Lwt.return
        ( add_rewards ctxt reward >|? fun ctxt ->
          let current_cycle = (Level.current ctxt).cycle in
          ( ctxt,
            Single_result
              (Double_baking_evidence_result
                 (Receipt.cleanup_balance_updates
                    [
                      ( Deposits (delegate, level.cycle),
                        Debited balance.deposit,
                        Block_application );
                      ( Fees (delegate, level.cycle),
                        Debited balance.fees,
                        Block_application );
                      ( Rewards (delegate, level.cycle),
                        Debited balance.rewards,
                        Block_application );
                      ( Rewards (baker, current_cycle),
                        Credited reward,
                        Block_application );
                    ])) ) )
  | Single (Activate_account {id = pkh; activation_code}) -> (
      let blinded_pkh =
        Blinded_public_key_hash.of_ed25519_pkh activation_code pkh
      in
      Commitment.find ctxt blinded_pkh >>=? function
      | None -> fail (Invalid_activation {pkh})
      | Some amount ->
          Commitment.remove_existing ctxt blinded_pkh >>=? fun ctxt ->
          let contract = Contract.implicit_contract (Signature.Ed25519 pkh) in
          Contract.(credit ctxt contract amount) >|=? fun ctxt ->
          ( ctxt,
            Single_result
              (Activate_account_result
                 [(Contract contract, Credited amount, Block_application)]) ))
  | Single (Proposals {source; period; proposals}) ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation >>?= fun () ->
      Voting_period.get_current ctxt >>=? fun {index = current_period; _} ->
      error_unless
        Compare.Int32.(current_period = period)
        (Wrong_voting_period (current_period, period))
      >>?= fun () ->
      Amendment.record_proposals ctxt source proposals >|=? fun ctxt ->
      (ctxt, Single_result Proposals_result)
  | Single (Ballot {source; period; proposal; ballot}) ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation >>?= fun () ->
      Voting_period.get_current ctxt >>=? fun {index = current_period; _} ->
      error_unless
        Compare.Int32.(current_period = period)
        (Wrong_voting_period (current_period, period))
      >>?= fun () ->
      Amendment.record_ballot ctxt source proposal ballot >|=? fun ctxt ->
      (ctxt, Single_result Ballot_result)
  | Single (Failing_noop _) ->
      (* Failing_noop _ always fails *)
      fail Failing_noop_error
  | Single (Manager_operation _) as op ->
      precheck_manager_contents_list ctxt op >>=? fun ctxt ->
      check_manager_signature ctxt chain_id op operation >>=? fun () ->
      apply_manager_contents_list ctxt mode baker chain_id op >|= ok
  | Cons (Manager_operation _, _) as op ->
      precheck_manager_contents_list ctxt op >>=? fun ctxt ->
      check_manager_signature ctxt chain_id op operation >>=? fun () ->
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

let may_start_new_cycle ctxt =
  match Level.dawn_of_a_new_cycle ctxt with
  | None -> return (ctxt, [], [])
  | Some last_cycle ->
      Seed.cycle_end ctxt last_cycle >>=? fun (ctxt, unrevealed) ->
      Roll.cycle_end ctxt last_cycle >>=? fun ctxt ->
      Delegate.cycle_end ctxt last_cycle unrevealed
      >>=? fun (ctxt, update_balances, deactivated) ->
      Bootstrap.cycle_end ctxt last_cycle >|=? fun ctxt ->
      (ctxt, update_balances, deactivated)

let endorsement_rights_of_pred_level ctxt =
  match Level.pred ctxt (Level.current ctxt) with
  | None -> assert false (* genesis *)
  | Some pred_level -> Baking.endorsement_rights ctxt pred_level

let apply_liquidity_baking_subsidy ctxt ~escape_vote =
  Liquidity_baking.on_subsidy_allowed
    ctxt
    ~escape_vote
    (fun ctxt liquidity_baking_cpmm_contract ->
      let ctxt =
        (* We set a gas limit of 1/20th the block limit, which is ~10x actual usage here in Granada. Gas consumed is reported in the Transaction receipt, but not counted towards the block limit. The gas limit is reset to unlimited at the end of this function.*)
        Gas.set_limit
          ctxt
          (Gas.Arith.integral_exn
             (Z.div
                (Gas.Arith.integral_to_z
                   (Constants.hard_gas_limit_per_block ctxt))
                (Z.of_int 20)))
      in
      let backtracking_ctxt = ctxt in
      (let liquidity_baking_subsidy = Constants.liquidity_baking_subsidy ctxt in
       (* credit liquidity baking subsidy to CPMM contract *)
       Contract.credit
         ctxt
         liquidity_baking_cpmm_contract
         liquidity_baking_subsidy
       >>=? fun ctxt ->
       Contract.get_script ctxt liquidity_baking_cpmm_contract
       >>=? fun (ctxt, script) ->
       match script with
       | None -> fail (Script_tc_errors.No_such_entrypoint "default")
       | Some script -> (
           let step_constants =
             let open Script_interpreter in
             (*
                  Using dummy values for source, payer, and chain_id since they are not used within the CPMM default entrypoint.
            *)
             {
               source = liquidity_baking_cpmm_contract;
               payer = liquidity_baking_cpmm_contract;
               self = liquidity_baking_cpmm_contract;
               amount = liquidity_baking_subsidy;
               chain_id = Chain_id.zero;
             }
           in
           let parameter =
             Micheline.strip_locations
               Michelson_v1_primitives.(Prim (0, D_Unit, [], []))
           in
           (*
                 Call CPPM default entrypoint with parameter Unit.
                 This is necessary for the CPMM's xtz_pool in storage to
                 increase since it cannot use BALANCE due to a transfer attack.

                 Mimicks a transaction.

                 There is no:
                 - storage burn (extra storage is free)
                 - fees (the operation is mandatory)
          *)
           Script_interpreter.execute
             ctxt
             Optimized
             step_constants
             ~script
             ~parameter
             ~entrypoint:"default"
             ~internal:false
           >>=? fun {ctxt; storage; lazy_storage_diff; operations} ->
           match operations with
           | _ :: _ ->
               (* No internal operations are expected here. Something bad may be happening. *)
               return (backtracking_ctxt, [])
           | [] ->
               (* update CPMM storage *)
               Contract.update_script_storage
                 ctxt
                 liquidity_baking_cpmm_contract
                 storage
                 lazy_storage_diff
               >>=? fun ctxt ->
               Fees.record_paid_storage_space_subsidy
                 ctxt
                 liquidity_baking_cpmm_contract
               >>=? fun (ctxt, new_size, paid_storage_size_diff) ->
               let balance_updates =
                 [
                   Receipt.
                     ( Contract liquidity_baking_cpmm_contract,
                       Credited liquidity_baking_subsidy,
                       Subsidy );
                 ]
               in
               let consumed_gas =
                 Gas.consumed ~since:backtracking_ctxt ~until:ctxt
               in
               let result =
                 Transaction_result
                   {
                     storage = Some storage;
                     lazy_storage_diff;
                     balance_updates;
                     (* At this point in application the origination nonce has not been initialized so it's not possible to originate new contracts. We've checked above that none were originated. *)
                     originated_contracts = [];
                     consumed_gas;
                     storage_size = new_size;
                     paid_storage_size_diff;
                     allocated_destination_contract = false;
                   }
               in
               let ctxt = Gas.set_unlimited ctxt in
               return (ctxt, [Successful_manager_result result])))
      >|= function
      | Ok (ctxt, results) -> Ok (ctxt, results)
      | Error _ ->
          (* Do not fail if something bad happens during CPMM contract call. *)
          let ctxt = Gas.set_unlimited backtracking_ctxt in
          Ok (ctxt, []))

let begin_full_construction ctxt pred_timestamp protocol_data =
  let priority = protocol_data.Block_header.priority in
  Global.set_block_priority ctxt priority >>=? fun ctxt ->
  Baking.check_timestamp ctxt ~priority pred_timestamp >>?= fun () ->
  let level = Level.current ctxt in
  Roll.baking_rights_owner ctxt level ~priority >>=? fun delegate_pk ->
  let ctxt = Fitness.increase ctxt in
  endorsement_rights_of_pred_level ctxt >>=? fun rights ->
  let ctxt = init_endorsements ctxt rights in
  let escape_vote = protocol_data.liquidity_baking_escape_vote in
  apply_liquidity_baking_subsidy ctxt ~escape_vote
  >|=? fun ( ctxt,
             liquidity_baking_operations_results,
             liquidity_baking_escape_ema ) ->
  ( ctxt,
    protocol_data,
    delegate_pk,
    liquidity_baking_operations_results,
    liquidity_baking_escape_ema )

let begin_partial_construction ctxt ~escape_vote =
  let ctxt = Fitness.increase ctxt in
  endorsement_rights_of_pred_level ctxt >>=? fun rights ->
  let ctxt = init_endorsements ctxt rights in
  apply_liquidity_baking_subsidy ctxt ~escape_vote

let begin_application ctxt chain_id block_header pred_timestamp =
  let priority = block_header.Block_header.protocol_data.contents.priority in
  Global.set_block_priority ctxt priority >>=? fun ctxt ->
  Baking.check_timestamp ctxt ~priority pred_timestamp >>?= fun () ->
  Baking.check_proof_of_work_stamp ctxt block_header >>?= fun () ->
  Baking.check_fitness_gap ctxt block_header >>?= fun () ->
  let current_level = Level.current ctxt in
  Roll.baking_rights_owner ctxt current_level ~priority >>=? fun delegate_pk ->
  Baking.check_signature block_header chain_id delegate_pk >>=? fun () ->
  let has_commitment =
    Option.is_some block_header.protocol_data.contents.seed_nonce_hash
  in
  error_unless
    Compare.Bool.(has_commitment = current_level.expected_commitment)
    (Invalid_commitment {expected = current_level.expected_commitment})
  >>?= fun () ->
  let ctxt = Fitness.increase ctxt in
  endorsement_rights_of_pred_level ctxt >>=? fun rights ->
  let ctxt = init_endorsements ctxt rights in
  let escape_vote =
    block_header.Block_header.protocol_data.contents
      .liquidity_baking_escape_vote
  in
  apply_liquidity_baking_subsidy ctxt ~escape_vote
  >|=? fun ( ctxt,
             liquidity_baking_operations_results,
             liquidity_baking_escape_ema ) ->
  ( ctxt,
    delegate_pk,
    liquidity_baking_operations_results,
    liquidity_baking_escape_ema )

let check_minimal_valid_time ctxt ~priority ~endorsing_power =
  let predecessor_timestamp = Timestamp.predecessor ctxt in
  Baking.minimal_valid_time
    (Constants.parametric ctxt)
    ~priority
    ~endorsing_power
    ~predecessor_timestamp
  >>? fun minimum ->
  let timestamp = Timestamp.current ctxt in
  error_unless
    Compare.Int64.(Time.to_seconds timestamp >= Time.to_seconds minimum)
    (Baking.Timestamp_too_early
       {
         minimal_time = minimum;
         provided_time = timestamp;
         priority;
         endorsing_power_opt = Some endorsing_power;
       })

let finalize_application ctxt protocol_data delegate migration_balance_updates
    liquidity_baking_escape_ema implicit_operations_results =
  let included_endorsements = included_endorsements ctxt in
  check_minimal_valid_time
    ctxt
    ~priority:protocol_data.Block_header.priority
    ~endorsing_power:included_endorsements
  >>?= fun () ->
  let deposit = Constants.block_security_deposit ctxt in
  add_deposit ctxt delegate deposit >>?= fun ctxt ->
  Baking.baking_reward
    ctxt
    ~block_priority:protocol_data.priority
    ~included_endorsements
  >>?= fun reward ->
  add_rewards ctxt reward >>?= fun ctxt ->
  Signature.Public_key_hash.Map.fold_es
    (fun delegate deposit ctxt -> Delegate.freeze_deposit ctxt delegate deposit)
    (get_deposits ctxt)
    ctxt
  >>=? fun ctxt ->
  (* end of level (from this point nothing should fail) *)
  let fees = Alpha_context.get_fees ctxt in
  Delegate.freeze_fees ctxt delegate fees >>=? fun ctxt ->
  let rewards = Alpha_context.get_rewards ctxt in
  Delegate.freeze_rewards ctxt delegate rewards >>=? fun ctxt ->
  (match protocol_data.Block_header.seed_nonce_hash with
  | None -> return ctxt
  | Some nonce_hash ->
      Nonce.record_hash ctxt {nonce_hash; delegate; rewards; fees})
  >>=? fun ctxt ->
  (* end of cycle *)
  (if Level.may_snapshot_rolls ctxt then Roll.snapshot_rolls ctxt
  else return ctxt)
  >>=? fun ctxt ->
  may_start_new_cycle ctxt >>=? fun (ctxt, balance_updates, deactivated) ->
  Amendment.may_start_new_voting_period ctxt >>=? fun ctxt ->
  let cycle = (Level.current ctxt).cycle in
  let balance_updates =
    Receipt.(
      cleanup_balance_updates
        (migration_balance_updates
        @ [
            ( Contract (Contract.implicit_contract delegate),
              Debited deposit,
              Block_application );
            (Deposits (delegate, cycle), Credited deposit, Block_application);
            (Rewards (delegate, cycle), Credited reward, Block_application);
          ]
        @ balance_updates))
  in
  let consumed_gas =
    Gas.Arith.sub
      (Gas.Arith.fp @@ Constants.hard_gas_limit_per_block ctxt)
      (Gas.block_level ctxt)
  in
  Voting_period.get_rpc_current_info ctxt >|=? fun voting_period_info ->
  let level_info = Level.current ctxt in
  let receipt =
    Apply_results.
      {
        baker = delegate;
        level_info;
        voting_period_info;
        nonce_hash = protocol_data.seed_nonce_hash;
        consumed_gas;
        deactivated;
        balance_updates;
        liquidity_baking_escape_ema;
        implicit_operations_results;
      }
  in
  (ctxt, receipt)
