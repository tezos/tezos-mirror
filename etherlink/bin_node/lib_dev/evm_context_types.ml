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
        payload : Blueprint_types.payload;
        delayed_transactions : Evm_events.Delayed_transaction.t list;
      }
        -> (Ethereum_types.hash Seq.t, tztrace) t
    | Last_known_L1_level : (int32 option, tztrace) t
    | Delayed_inbox_hashes : (Ethereum_types.hash list, tztrace) t
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
             (req "payload" Blueprint_types.payload_encoding)
             (req
                "delayed_transactions"
                (list Evm_events.Delayed_transaction.encoding)))
          (function
            | View
                (Apply_blueprint
                  {events; timestamp; payload; delayed_transactions}) ->
                Some ((), events, timestamp, payload, delayed_transactions)
            | _ -> None)
          (fun ((), events, timestamp, payload, delayed_transactions) ->
            View
              (Apply_blueprint
                 {events; timestamp; payload; delayed_transactions}));
        case
          (Tag 2)
          ~title:"Last_known_L1_level"
          (obj1 (req "request" (constant "last_known_l1_level")))
          (function View Last_known_L1_level -> Some () | _ -> None)
          (fun () -> View Last_known_L1_level);
        case
          (Tag 3)
          ~title:"Delayed_inbox_hashes"
          (obj1 (req "request" (constant "Delayed_inbox_hashes")))
          (function View Delayed_inbox_hashes -> Some () | _ -> None)
          (fun () -> View Delayed_inbox_hashes);
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
      ]

  let pp ppf view =
    Data_encoding.Json.pp ppf @@ Data_encoding.Json.construct encoding view
end
