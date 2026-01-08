(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base

module Request = struct
  type (_, _) t =
    | Apply_evm_events : {
        finalized_level : int32 option;
        events : Evm_events.t list;
      }
        -> (unit, tztrace) t
    | Apply_blueprint : {
        events : Evm_events.t list option;
        timestamp : Time.Protocol.t;
        chunks : Sequencer_blueprint.unsigned_chunked_blueprint;
        payload : Blueprint_types.payload tzresult Lwt.t;
        delayed_transactions : Evm_events.Delayed_transaction.t list;
      }
        -> (Ethereum_types.hash Seq.t, tztrace) t
    | Last_known_L1_level : (int32 option, tztrace) t
    | Patch_state : {
        commit : bool;
        key : string;
        patch : string option -> string option;
        block_number : Ethereum_types.quantity option;
      }
        -> (unit, tztrace) t
    | Wasm_pvm_version : (Tezos_scoru_wasm.Wasm_pvm_state.version, tztrace) t
    | Potential_observer_reorg : {
        evm_node_endpoint : Uri.t;
        blueprint_with_events : Blueprint_types.with_events;
      }
        -> (Ethereum_types.quantity option, tztrace) t
    | Finalized_levels : {
        l1_level : int32;
        start_l2_level : Ethereum_types.quantity;
        end_l2_level : Ethereum_types.quantity;
      }
        -> (unit, tztrace) t
    | Next_block_info : {
        timestamp : Time.Protocol.t;
        number : Ethereum_types.quantity;
      }
        -> (unit, tztrace) t
    | Execute_single_transaction : {
        tx : Broadcast.transaction;
        hash : Ethereum_types.hash;
      }
        -> (Transaction_receipt.t option, tztrace) t

  let name (type a b) (t : (a, b) t) =
    match t with
    | Apply_evm_events _ -> "Apply_evm_events"
    | Apply_blueprint _ -> "Apply_blueprint"
    | Last_known_L1_level -> "Last_known_l1_level"
    | Patch_state _ -> "Patch_state"
    | Wasm_pvm_version -> "Wasm_pvm_version"
    | Potential_observer_reorg _ -> "Potential_observer_reorg"
    | Finalized_levels _ -> "Finalized_levels"
    | Execute_single_transaction _ -> "Execute_single_transaction"
    | Next_block_info _ -> "Next_block_info"

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Apply_evm_events"
          (obj3
             (req "request" (constant "apply_evm_events"))
             (opt "finalized_level" int32)
             (req "events" (list Evm_events.encoding)))
          (function
            | View (Apply_evm_events {finalized_level; events}) ->
                Some ((), finalized_level, events)
            | _ -> None)
          (fun ((), finalized_level, events) ->
            View (Apply_evm_events {finalized_level; events}));
        case
          (Tag 1)
          ~title:"Apply_blueprint"
          (obj5
             (req "request" (constant "apply_blueprint"))
             (opt "events" (list Evm_events.encoding))
             (req "timestamp" Time.Protocol.encoding)
             (req
                "chunks"
                Sequencer_blueprint.unsigned_chunked_blueprint_encoding)
             (req
                "delayed_transactions"
                (list Evm_events.Delayed_transaction.encoding)))
          (function
            | View
                (Apply_blueprint
                   {
                     events;
                     timestamp;
                     chunks;
                     payload = _;
                     delayed_transactions;
                   }) ->
                Some ((), events, timestamp, chunks, delayed_transactions)
            | _ -> None)
          (fun _ -> assert false);
        case
          (Tag 2)
          ~title:"Last_known_L1_level"
          (obj1 (req "request" (constant "last_known_l1_level")))
          (function View Last_known_L1_level -> Some () | _ -> None)
          (fun () -> View Last_known_L1_level);
        case
          (Tag 4)
          ~title:"Patch_state"
          (obj4
             (req "request" (constant "patch_state"))
             (req "commit" bool)
             (req "key" string)
             (opt "block_number" Ethereum_types.quantity_encoding))
          (function
            | View (Patch_state {commit; key; patch = _; block_number}) ->
                Some ((), commit, key, block_number)
            | _ -> None)
          (fun ((), commit, key, block_number) ->
            (* This is dead code, the encoding is only used for logging (i.e.,
               encoding) *)
            View (Patch_state {commit; key; patch = Fun.id; block_number}));
        case
          (Tag 5)
          ~title:"Wasm_pvm_version"
          (obj1 (req "request" (constant "wasm_pvm_version")))
          (function View Wasm_pvm_version -> Some () | _ -> None)
          (fun () -> View Wasm_pvm_version);
        case
          (Tag 6)
          ~title:"Potential_observer_reorg"
          (obj3
             (req "request" (constant "potential_observer_reorg"))
             (req "evm_node_endpoint" string)
             (req "blueprint_with_events" Blueprint_types.with_events_encoding))
          (function
            | View
                (Potential_observer_reorg
                   {evm_node_endpoint; blueprint_with_events}) ->
                Some ((), Uri.to_string evm_node_endpoint, blueprint_with_events)
            | _ -> None)
          (fun ((), evm_node_endpoint, blueprint_with_events) ->
            View
              (Potential_observer_reorg
                 {
                   evm_node_endpoint = Uri.of_string evm_node_endpoint;
                   blueprint_with_events;
                 }));
        case
          (Tag 7)
          ~title:"Finalized_levels"
          (obj4
             (req "request" (constant "finalized_levels"))
             (req "l1_level" int32)
             (req "start_l2_level" Ethereum_types.quantity_encoding)
             (req "end_l2_level" Ethereum_types.quantity_encoding))
          (function
            | View (Finalized_levels {l1_level; start_l2_level; end_l2_level})
              ->
                Some ((), l1_level, start_l2_level, end_l2_level)
            | _ -> None)
          (fun ((), l1_level, start_l2_level, end_l2_level) ->
            View (Finalized_levels {l1_level; start_l2_level; end_l2_level}));
        case
          (Tag 8)
          ~title:"Execute_single_transaction"
          (obj3
             (req "request" (constant "execute_single_transaction"))
             (req "tx" Broadcast.transaction_encoding)
             (req "hash" Ethereum_types.hash_encoding))
          (function
            | View (Execute_single_transaction {tx; hash}) -> Some ((), tx, hash)
            | _ -> None)
          (fun ((), tx, hash) -> View (Execute_single_transaction {tx; hash}));
        case
          (Tag 9)
          ~title:"Next_block_info"
          (obj3
             (req "request" (constant "next_block_info"))
             (req "timestamp" Time.Protocol.encoding)
             (req "number" Ethereum_types.quantity_encoding))
          (function
            | View (Next_block_info {timestamp; number}) ->
                Some ((), timestamp, number)
            | _ -> None)
          (fun ((), timestamp, number) ->
            View (Next_block_info {timestamp; number}));
      ]

  let pp ppf view =
    Data_encoding.Json.pp ppf @@ Data_encoding.Json.construct encoding view
end
