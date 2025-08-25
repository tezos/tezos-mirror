(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Environment
open Environment.Error_monad
open Protocol
open Protocol.Alpha_context
open Services_registration_plugin

let custom_root =
  (RPC_path.(open_root / "context" / "contracts")
    : RPC_context.t RPC_path.context)

let big_map_root =
  (RPC_path.(open_root / "context" / "big_maps")
    : RPC_context.t RPC_path.context)

type info = {
  balance : Tez.t;
  delegate : public_key_hash option;
  counter : Manager_counter.t option;
  script : Script.t option;
  revealed : bool option;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun {balance; delegate; script; counter; revealed} ->
      (balance, delegate, script, counter, revealed))
    (fun (balance, delegate, script, counter, revealed) ->
      {balance; delegate; script; counter; revealed})
  @@ obj5
       (req "balance" Tez.encoding)
       (opt "delegate" Signature.Public_key_hash.encoding)
       (opt "script" Script.encoding)
       (opt "counter" Manager_counter.encoding_for_RPCs)
       (opt
          ~description:
            "field present for implicit account only: true means the manager \
             pk has been revealed"
          "revealed"
          bool)

let legacy = Script_ir_translator_config.make ~legacy:true ()

module S = struct
  open Data_encoding

  let balance =
    RPC_service.get_service
      ~description:
        "The spendable balance of a contract (in mutez), also known as liquid \
         balance. Corresponds to tez owned by the contract that are neither \
         staked, nor in unstaked requests, nor in frozen bonds. Identical to \
         the 'spendable' RPC."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance")

  let spendable =
    RPC_service.get_service
      ~description:
        "The spendable balance of a contract (in mutez), also known as liquid \
         balance. Corresponds to tez owned by the contract that are neither \
         staked, nor in unstaked requests, nor in frozen bonds. Identical to \
         the 'balance' RPC."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "spendable")

  let frozen_bonds =
    RPC_service.get_service
      ~description:"Access the frozen bonds of a contract."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "frozen_bonds")

  let balance_and_frozen_bonds =
    RPC_service.get_service
      ~description:
        "The sum (in mutez) of the spendable balance and frozen bonds of a \
         contract. Corresponds to the contract's full balance from which \
         staked funds and unstake requests have been excluded. Identical to \
         the 'spendable_and_frozen_bonds' RPC."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "balance_and_frozen_bonds")

  let spendable_and_frozen_bonds =
    RPC_service.get_service
      ~description:
        "The sum (in mutez) of the spendable balance and frozen bonds of a \
         contract. Corresponds to the contract's full balance from which \
         staked funds and unstake requests have been excluded. Identical to \
         the 'balance_and_frozen_bonds' RPC."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "spendable_and_frozen_bonds")

  let staked_balance =
    RPC_service.get_service
      ~description:
        "Access the staked balance of a contract (in mutez). Returns None if \
         the contract is originated, or neither delegated nor a delegate."
      ~query:RPC_query.empty
      ~output:(option Tez.encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "staked_balance")

  let staking_numerator =
    RPC_service.get_service
      ~description:
        "Returns an abstract representation of the contract's \
         total_delegated_stake."
      ~query:RPC_query.empty
      ~output:Staking_pseudotoken.For_RPC.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "staking_numerator")

  let unstaked_frozen_balance =
    RPC_service.get_service
      ~description:
        "Access the balance of a contract that was requested for an unstake \
         operation, but is still frozen for the duration of the slashing \
         period. Returns None if the contract is originated."
      ~query:RPC_query.empty
      ~output:(option Tez.encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "unstaked_frozen_balance")

  let unstaked_finalizable_balance =
    RPC_service.get_service
      ~description:
        "Access the balance of a contract that was requested for an unstake \
         operation, and is no longer frozen, which means it will appear in the \
         spendable balance of the contract after any \
         stake/unstake/finalize_unstake operation. Returns None if the \
         contract is originated."
      ~query:RPC_query.empty
      ~output:(option Tez.encoding)
      RPC_path.(
        custom_root /: Contract.rpc_arg / "unstaked_finalizable_balance")

  let unstake_requests =
    RPC_service.get_service
      ~description:
        "Access the unstake requests of the contract. The requests that appear \
         in the finalizable field can be finalized, which means that the \
         contract can transfer these (no longer frozen) funds to their \
         spendable balance with a [finalize_unstake] operation call. Returns \
         None if there is no unstake request pending."
      ~query:RPC_query.empty
      ~output:
        (option Unstake_requests.For_RPC.prepared_finalize_unstake_encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "unstake_requests")

  let full_balance =
    RPC_service.get_service
      ~description:
        "The full balance (in mutez) of the contract. Includes its spendable \
         balance, staked tez, unstake requests, and frozen bonds. Even if the \
         contract is a delegate, it does not include any staked or delegated \
         tez owned by external delegators."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "full_balance")

  let manager_key =
    RPC_service.get_service
      ~description:"Access the manager of an implicit contract."
      ~query:RPC_query.empty
      ~output:(option Signature.Public_key.encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "manager_key")

  let delegate =
    RPC_service.get_service
      ~description:"Access the delegate of a contract, if any."
      ~query:RPC_query.empty
      ~output:Signature.Public_key_hash.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "delegate")

  let counter =
    RPC_service.get_service
      ~description:"Access the counter of a contract, if any."
      ~query:RPC_query.empty
      ~output:Manager_counter.encoding_for_RPCs
      RPC_path.(custom_root /: Contract.rpc_arg / "counter")

  let script =
    RPC_service.get_service
      ~description:"Access the code and data of the contract."
      ~query:RPC_query.empty
      ~output:Script.encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "script")

  let storage =
    RPC_service.get_service
      ~description:"Access the data of the contract."
      ~query:RPC_query.empty
      ~output:Script.expr_encoding
      RPC_path.(custom_root /: Contract.rpc_arg / "storage")

  type normalize_types_query = {normalize_types : bool}

  let normalize_types_query : normalize_types_query RPC_query.t =
    let open RPC_query in
    query (fun normalize_types -> {normalize_types})
    |+ flag
         ~descr:
           "Whether types should be normalized (annotations removed, combs \
            flattened) or kept as they appeared in the original script."
         "normalize_types"
         (fun t -> t.normalize_types)
    |> seal

  let entrypoint_type =
    RPC_service.get_service
      ~description:"Return the type of the given entrypoint of the contract"
      ~query:normalize_types_query
      ~output:Script.expr_encoding
      RPC_path.(
        custom_root /: Contract.rpc_arg / "entrypoints" /: Entrypoint.rpc_arg)

  let list_entrypoints =
    RPC_service.get_service
      ~description:"Return the list of entrypoints of the contract"
      ~query:normalize_types_query
      ~output:
        (obj2
           (dft
              "unreachable"
              (Data_encoding.list
                 (obj1
                    (req
                       "path"
                       (Data_encoding.list
                          Michelson_v1_primitives.prim_encoding))))
              [])
           (req "entrypoints" (assoc Script.expr_encoding)))
      RPC_path.(custom_root /: Contract.rpc_arg / "entrypoints")

  let contract_big_map_get_opt =
    RPC_service.post_service
      ~description:
        "Access the value associated with a key in a big map of the contract \
         (deprecated)."
      ~query:RPC_query.empty
      ~input:
        (obj2
           (req "key" Script.expr_encoding)
           (req "type" Script.expr_encoding))
      ~output:(option Script.expr_encoding)
      RPC_path.(custom_root /: Contract.rpc_arg / "big_map_get")

  let big_map_get =
    RPC_service.get_service
      ~description:"Access the value associated with a key in a big map."
      ~query:RPC_query.empty
      ~output:Script.expr_encoding
      RPC_path.(big_map_root /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg)

  type big_map_get_all_query = {offset : int option; length : int option}

  let rpc_arg_uint : int RPC_arg.t =
    let open Result_syntax in
    let int_of_string s =
      let* i =
        int_of_string_opt s
        |> Option.to_result
             ~none:(Format.sprintf "Cannot parse integer value %s" s)
      in
      if Compare.Int.(i < 0) then
        Error (Format.sprintf "Negative integer: %d" i)
      else Ok i
    in
    RPC_arg.make
      ~name:"uint"
      ~descr:"A non-negative integer (greater than or equal to 0)."
      ~destruct:int_of_string
      ~construct:string_of_int
      ()

  let big_map_get_all_query : big_map_get_all_query RPC_query.t =
    let open RPC_query in
    query (fun offset length -> {offset; length})
    |+ opt_field
         ~descr:
           "Skip the first [offset] values. Useful in combination with \
            [length] for pagination."
         "offset"
         rpc_arg_uint
         (fun t -> t.offset)
    |+ opt_field
         ~descr:
           "Only retrieve [length] values. Useful in combination with [offset] \
            for pagination."
         "length"
         rpc_arg_uint
         (fun t -> t.length)
    |> seal

  let big_map_get_all =
    RPC_service.get_service
      ~description:
        "Get the (optionally paginated) list of values in a big map. Order of \
         values is unspecified, but is guaranteed to be consistent."
      ~query:big_map_get_all_query
      ~output:(list Script.expr_encoding)
      RPC_path.(big_map_root /: Big_map.Id.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:"Access the complete status of a contract."
      ~query:normalize_types_query
      ~output:info_encoding
      RPC_path.(custom_root /: Contract.rpc_arg)

  let list =
    RPC_service.get_service
      ~description:
        "All existing contracts (excluding empty implicit contracts)."
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      custom_root

  let estimated_own_pending_slashed_amount =
    RPC_service.get_service
      ~description:
        "Returns the estimated own pending slashed amount (in mutez) of a \
         given contract."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(
        custom_root /: Contract.rpc_arg / "estimated_own_pending_slashed_amount")

  module Sapling = struct
    (*
      Sapling: these RPCs are like Sapling RPCs (sapling_services.ml)
      specialized for contracts containing a single sapling state.
    *)

    let single_sapling_get_id ctxt contract_id =
      let open Lwt_result_syntax in
      let* ctxt, script = Contract.get_script ctxt contract_id in
      match script with
      | None -> return (None, ctxt)
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let*! tzresult =
            Script_ir_translator.parse_script
              ctxt
              ~elab_conf:legacy
              ~allow_forged_tickets_in_storage:true
              ~allow_forged_lazy_storage_id_in_storage:true
              script
          in
          let*? Ex_script (Script script), ctxt = tzresult in
          Lwt.return
          @@ Script_ir_translator.get_single_sapling_state
               ctxt
               script.storage_type
               script.storage

    let make_service
        Sapling_services.S.Args.{name; description; query; output; f} =
      let open Lwt_result_syntax in
      let name = "single_sapling_" ^ name in
      let path = RPC_path.(custom_root /: Contract.rpc_arg / name) in
      let service = RPC_service.get_service ~description ~query ~output path in
      ( service,
        fun ctxt contract_id q () ->
          match (contract_id : Contract.t) with
          | Implicit _ -> return_none
          | Originated contract_id ->
              let* sapling_id, ctxt = single_sapling_get_id ctxt contract_id in
              Option.map_es (fun sapling_id -> f ctxt sapling_id q) sapling_id
      )

    let get_diff = make_service Sapling_services.S.Args.get_diff

    let register () =
      let reg chunked (service, f) = opt_register1 ~chunked service f in
      reg false get_diff

    let mk_call1 (service, _f) ctxt block id q =
      RPC_context.make_call1 service ctxt block id q ()
  end
end

module Implem = struct
  let unstake_requests ctxt contract =
    let open Lwt_result_syntax in
    let open Unstake_requests.For_RPC in
    let* result =
      (* This function applies slashing to finalizable requests. *)
      prepare_finalize_unstake ctxt contract
    in
    match result with
    | None -> return_none
    | Some {finalizable; unfinalizable} ->
        let* unfinalizable =
          (* Apply slashing to unfinalizable requests too. *)
          apply_slash_to_unstaked_unfinalizable_stored_requests
            ctxt
            unfinalizable
        in
        return_some {finalizable; unfinalizable}
end

let register () =
  let open Lwt_result_syntax in
  register0 ~chunked:true S.list (fun ctxt () () ->
      let*! result = Contract.list ctxt in
      return result) ;
  let register_field_gen ~filter_contract ~wrap_result ~chunked s f =
    opt_register1 ~chunked s (fun ctxt contract () () ->
        filter_contract contract @@ fun filtered_contract ->
        let*! exists = Contract.exists ctxt contract in
        match exists with
        | true -> f ctxt filtered_contract |> wrap_result
        | false -> return_none)
  in
  let register_field_with_query_gen ~filter_contract ~wrap_result ~chunked s f =
    opt_register1 ~chunked s (fun ctxt contract query () ->
        filter_contract contract @@ fun filtered_contract ->
        let*! exists = Contract.exists ctxt contract in
        match exists with
        | true -> f ctxt filtered_contract query |> wrap_result
        | false -> return_none)
  in
  let register_field s =
    register_field_gen
      ~filter_contract:(fun c k -> k c)
      ~wrap_result:(fun res ->
        let+ value = res in
        Option.some value)
      s
  in
  let register_field_with_query s =
    register_field_with_query_gen
      ~filter_contract:(fun c k -> k c)
      ~wrap_result:(fun res ->
        let+ value = res in
        Option.some value)
      s
  in
  let register_opt_field s =
    register_field_gen
      ~filter_contract:(fun c k -> k c)
      ~wrap_result:(fun res -> res)
      s
  in
  let register_originated_opt_field s =
    register_field_gen
      ~filter_contract:(fun c k ->
        match (c : Contract.t) with
        | Implicit _ -> return_none
        | Originated c -> k c)
      ~wrap_result:(fun res -> res)
      s
  in
  let do_big_map_get ctxt id key =
    let open Script_ir_translator in
    let ctxt = Gas.set_unlimited ctxt in
    let* ctxt, types = Big_map.exists ctxt id in
    match types with
    | None -> return_none
    | Some (_, value_type) -> (
        let*? Ex_ty value_type, ctxt =
          parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
        in
        let* _ctxt, value = Big_map.get_opt ctxt id key in
        match value with
        | None -> return_none
        | Some value ->
            let* value, ctxt =
              parse_data
                ctxt
                ~elab_conf:legacy
                ~allow_forged_tickets:true
                ~allow_forged_lazy_storage_id:true
                value_type
                (Micheline.root value)
            in
            let+ value, _ctxt = unparse_data ctxt Readable value_type value in
            Some value)
  in
  let do_big_map_get_all ?offset ?length ctxt id =
    let open Script_ir_translator in
    let ctxt = Gas.set_unlimited ctxt in
    let* ctxt, types = Big_map.exists ctxt id in
    match types with
    | None -> raise Not_found
    | Some (_, value_type) ->
        let*? Ex_ty value_type, ctxt =
          parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
        in
        let* ctxt, key_values =
          Big_map.list_key_values ?offset ?length ctxt id
        in
        let+ _ctxt, rev_values =
          List.fold_left_s
            (fun acc (_key_hash, value) ->
              let*? ctxt, rev_values = acc in
              let* value, ctxt =
                parse_data
                  ctxt
                  ~elab_conf:legacy
                  ~allow_forged_tickets:true
                  ~allow_forged_lazy_storage_id:true
                  value_type
                  (Micheline.root value)
              in
              let+ value, ctxt = unparse_data ctxt Readable value_type value in
              (ctxt, value :: rev_values))
            (Ok (ctxt, []))
            key_values
        in
        List.rev rev_values
  in
  register_field ~chunked:false S.balance Contract.get_balance ;
  register_field ~chunked:false S.spendable Contract.get_balance ;
  register_field ~chunked:false S.frozen_bonds Contract.get_frozen_bonds ;
  register_field
    ~chunked:false
    S.balance_and_frozen_bonds
    Contract.get_balance_and_frozen_bonds ;
  register_field
    ~chunked:false
    S.spendable_and_frozen_bonds
    Contract.get_balance_and_frozen_bonds ;
  register_field
    ~chunked:false
    S.staked_balance
    Contract.For_RPC.get_staked_balance ;
  register_field ~chunked:false S.staking_numerator (fun ctxt delegator ->
      Staking_pseudotokens.For_RPC.staking_pseudotokens_balance ctxt ~delegator) ;
  register_field
    ~chunked:false
    S.unstaked_frozen_balance
    Contract.For_RPC.get_unstaked_frozen_balance ;
  register_field
    ~chunked:false
    S.unstaked_finalizable_balance
    Contract.For_RPC.get_unstaked_finalizable_balance ;
  register_field ~chunked:false S.full_balance Contract.For_RPC.get_full_balance ;
  register1 ~chunked:false S.unstake_requests (fun ctxt contract () () ->
      Implem.unstake_requests ctxt contract) ;
  opt_register1 ~chunked:false S.manager_key (fun ctxt contract () () ->
      match contract with
      | Originated _ -> return_none
      | Implicit mgr -> (
          let* is_revealed = Contract.is_manager_key_revealed ctxt mgr in
          match is_revealed with
          | false -> return_some None
          | true ->
              let+ key = Contract.get_manager_key ctxt mgr in
              Some (Some key))) ;
  register_opt_field ~chunked:false S.delegate Contract.Delegate.find ;
  opt_register1 ~chunked:false S.counter (fun ctxt contract () () ->
      match contract with
      | Originated _ -> return_none
      | Implicit mgr ->
          let+ counter = Contract.get_counter ctxt mgr in
          Some counter) ;
  register_originated_opt_field ~chunked:true S.script (fun c v ->
      let+ _, v = Contract.get_script c v in
      v) ;
  register_originated_opt_field ~chunked:true S.storage (fun ctxt contract ->
      let* ctxt, script = Contract.get_script ctxt contract in
      match script with
      | None -> return_none
      | Some script ->
          let ctxt = Gas.set_unlimited ctxt in
          let open Script_ir_translator in
          let* Ex_script (Script {storage; storage_type; _}), ctxt =
            parse_script
              ctxt
              ~elab_conf:legacy
              ~allow_forged_tickets_in_storage:true
              ~allow_forged_lazy_storage_id_in_storage:true
              script
          in
          let+ storage, _ctxt =
            unparse_data ctxt Readable storage_type storage
          in
          Some storage) ;
  opt_register2
    ~chunked:true
    S.entrypoint_type
    (fun ctxt v entrypoint {normalize_types} () ->
      match (v : Contract.t) with
      | Implicit _ -> return_none
      | Originated v -> (
          let* _, expr = Contract.get_script_code ctxt v in
          match expr with
          | None -> return_none
          | Some expr -> (
              let ctxt = Gas.set_unlimited ctxt in
              let legacy = true in
              let open Script_ir_translator in
              let*? expr, _ =
                Script.force_decode_in_context
                  ~consume_deserialization_gas:When_needed
                  ctxt
                  expr
              in
              let* {arg_type; _}, ctxt = parse_toplevel ctxt expr in
              let*? Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _ =
                parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
              in
              let*? r, ctxt =
                Gas_monad.run ctxt
                @@ Script_ir_translator.find_entrypoint
                     ~error_details:(Informative ())
                     arg_type
                     entrypoints
                     entrypoint
              in
              r |> function
              | Ok (Ex_ty_cstr {ty; original_type_expr; _}) ->
                  if normalize_types then
                    let*? ty_node, _ctxt =
                      Script_ir_unparser.unparse_ty ~loc:() ctxt ty
                    in
                    return_some (Micheline.strip_locations ty_node)
                  else
                    return_some (Micheline.strip_locations original_type_expr)
              | Error _ -> return_none))) ;
  opt_register1
    ~chunked:true
    S.list_entrypoints
    (fun ctxt v {normalize_types} () ->
      match (v : Contract.t) with
      | Implicit _ -> return_none
      | Originated v -> (
          let* _, expr = Contract.get_script_code ctxt v in
          match expr with
          | None -> return_none
          | Some expr ->
              let ctxt = Gas.set_unlimited ctxt in
              let legacy = true in
              let open Script_ir_translator in
              let*? expr, _ =
                Script.force_decode_in_context
                  ~consume_deserialization_gas:When_needed
                  ctxt
                  expr
              in
              let* {arg_type; _}, ctxt = parse_toplevel ctxt expr in
              Lwt.return
                (let open Result_syntax in
                 let* Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _
                     =
                   parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
                 in
                 let unreachable_entrypoint, map =
                   Script_ir_translator.list_entrypoints_uncarbonated
                     arg_type
                     entrypoints
                 in
                 let* entrypoint_types, _ctxt =
                   Entrypoint.Map.fold_e
                     (fun entry
                          (Script_typed_ir.Ex_ty ty, original_type_expr)
                          (acc, ctxt)
                        ->
                       let* ty_expr, ctxt =
                         if normalize_types then
                           let* ty_node, ctxt =
                             Script_ir_unparser.unparse_ty ~loc:() ctxt ty
                           in
                           return (Micheline.strip_locations ty_node, ctxt)
                         else
                           return
                             (Micheline.strip_locations original_type_expr, ctxt)
                       in
                       return
                         ((Entrypoint.to_string entry, ty_expr) :: acc, ctxt))
                     map
                     ([], ctxt)
                 in
                 return_some (unreachable_entrypoint, entrypoint_types)))) ;
  opt_register1
    ~chunked:true
    S.contract_big_map_get_opt
    (fun ctxt contract () (key, key_type) ->
      match (contract : Contract.t) with
      | Implicit _ -> return_none
      | Originated contract -> (
          let* ctxt, script = Contract.get_script ctxt contract in
          let key_type_node = Micheline.root key_type in
          let*? Ex_comparable_ty key_type, ctxt =
            Script_ir_translator.parse_comparable_ty ctxt key_type_node
          in
          let* key, ctxt =
            Script_ir_translator.parse_comparable_data
              ctxt
              key_type
              (Micheline.root key)
          in
          let* key, ctxt =
            Script_ir_translator.hash_comparable_data ctxt key_type key
          in
          match script with
          | None -> return_none
          | Some script -> (
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              let* Ex_script (Script script), ctxt =
                parse_script
                  ctxt
                  ~elab_conf:legacy
                  ~allow_forged_tickets_in_storage:true
                  ~allow_forged_lazy_storage_id_in_storage:true
                  script
              in
              let*? ids, _ctxt =
                Script_ir_translator.collect_lazy_storage
                  ctxt
                  script.storage_type
                  script.storage
              in
              match Script_ir_translator.list_of_big_map_ids ids with
              | [] | _ :: _ :: _ -> return_some None
              | [id] ->
                  let+ result = do_big_map_get ctxt id key in
                  Option.some result))) ;
  opt_register2 ~chunked:true S.big_map_get (fun ctxt id key () () ->
      do_big_map_get ctxt id key) ;
  register1 ~chunked:true S.big_map_get_all (fun ctxt id {offset; length} () ->
      do_big_map_get_all ?offset ?length ctxt id) ;
  register_field_with_query
    ~chunked:false
    S.info
    (fun ctxt contract {normalize_types} ->
      let* balance = Contract.get_balance ctxt contract in
      let* delegate = Contract.Delegate.find ctxt contract in
      match contract with
      | Implicit manager ->
          let* revealed = Contract.is_manager_key_revealed ctxt manager in
          let+ counter = Contract.get_counter ctxt manager in
          {
            balance;
            delegate;
            script = None;
            counter = Some counter;
            revealed = Some revealed;
          }
      | Originated contract -> (
          let* ctxt, script = Contract.get_script ctxt contract in
          match script with
          | None ->
              return
                {
                  balance;
                  delegate;
                  script = None;
                  counter = None;
                  revealed = None;
                }
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let+ script, _ctxt =
                Script_ir_translator.parse_and_unparse_script_unaccounted
                  ctxt
                  ~legacy:true
                  ~allow_forged_tickets_in_storage:true
                  ~allow_forged_lazy_storage_id_in_storage:true
                  Readable
                  ~normalize_types
                  script
              in
              {
                balance;
                delegate;
                script = Some script;
                counter = None;
                revealed = None;
              })) ;
  register1
    ~chunked:false
    S.estimated_own_pending_slashed_amount
    (fun ctxt contract () () ->
      Contract.For_RPC.get_estimated_own_pending_slashed_amount ctxt contract) ;

  S.Sapling.register ()

let list ctxt block = RPC_context.make_call0 S.list ctxt block () ()

let info ctxt block contract ~normalize_types =
  RPC_context.make_call1 S.info ctxt block contract {normalize_types} ()

let balance ctxt block contract =
  RPC_context.make_call1 S.balance ctxt block contract () ()

let frozen_bonds ctxt block contract =
  RPC_context.make_call1 S.frozen_bonds ctxt block contract () ()

let balance_and_frozen_bonds ctxt block contract =
  RPC_context.make_call1 S.balance_and_frozen_bonds ctxt block contract () ()

let staked_balance ctxt block contract =
  RPC_context.make_call1 S.staked_balance ctxt block contract () ()

let staking_numerator ctxt block contract =
  RPC_context.make_call1 S.staking_numerator ctxt block contract () ()

let unstaked_frozen_balance ctxt block contract =
  RPC_context.make_call1 S.unstaked_frozen_balance ctxt block contract () ()

let unstaked_finalizable_balance ctxt block contract =
  RPC_context.make_call1
    S.unstaked_finalizable_balance
    ctxt
    block
    contract
    ()
    ()

let unstake_requests ctxt block contract =
  RPC_context.make_call1 S.unstake_requests ctxt block contract () ()

let full_balance ctxt block contract =
  RPC_context.make_call1 S.full_balance ctxt block contract () ()

let manager_key ctxt block mgr =
  RPC_context.make_call1 S.manager_key ctxt block (Contract.Implicit mgr) () ()

let delegate ctxt block contract =
  RPC_context.make_call1 S.delegate ctxt block contract () ()

let delegate_opt ctxt block contract =
  RPC_context.make_opt_call1 S.delegate ctxt block contract () ()

let counter ctxt block mgr =
  RPC_context.make_call1 S.counter ctxt block (Contract.Implicit mgr) () ()

let script ctxt block contract =
  let contract = Contract.Originated contract in
  RPC_context.make_call1 S.script ctxt block contract () ()

let script_opt ctxt block contract =
  let contract = Contract.Originated contract in
  RPC_context.make_opt_call1 S.script ctxt block contract () ()

let storage ctxt block contract =
  let contract = Contract.Originated contract in
  RPC_context.make_call1 S.storage ctxt block contract () ()

let estimated_own_pending_slashed_amount ctxt block contract =
  let contract = Contract.Implicit contract in
  RPC_context.make_call1
    S.estimated_own_pending_slashed_amount
    ctxt
    block
    contract
    ()
    ()

let entrypoint_type ctxt block contract entrypoint ~normalize_types =
  RPC_context.make_call2
    S.entrypoint_type
    ctxt
    block
    (Contract.Originated contract)
    entrypoint
    {normalize_types}
    ()

let list_entrypoints ctxt block contract ~normalize_types =
  RPC_context.make_call1
    S.list_entrypoints
    ctxt
    block
    (Contract.Originated contract)
    {normalize_types}
    ()

let storage_opt ctxt block contract =
  let contract = Contract.Originated contract in
  RPC_context.make_opt_call1 S.storage ctxt block contract () ()

let big_map_get ctxt block id key =
  RPC_context.make_call2 S.big_map_get ctxt block id key () ()

let contract_big_map_get_opt ctxt block contract key =
  let contract = Contract.Originated contract in
  RPC_context.make_call1 S.contract_big_map_get_opt ctxt block contract () key

let single_sapling_get_diff ctxt block id ?offset_commitment ?offset_nullifier
    () =
  S.Sapling.(mk_call1 get_diff)
    ctxt
    block
    (Contract.Originated id)
    Sapling_services.{offset_commitment; offset_nullifier}
