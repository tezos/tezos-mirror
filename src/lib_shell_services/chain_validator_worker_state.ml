(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module Request = struct
  type view = Hash of Block_hash.t | PeerId of P2p_peer.Id.t

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Hash"
          (obj1 (req "hash" Block_hash.encoding))
          (function Hash h -> Some h | _ -> None)
          (fun h -> Hash h);
        case
          (Tag 1)
          ~title:"Peer_id"
          (obj1 (req "peer_id" P2p_peer.Id.encoding))
          (function PeerId pid -> Some pid | _ -> None)
          (fun pid -> PeerId pid);
      ]

  let pp ppf = function
    | Hash h -> Block_hash.pp ppf h
    | PeerId pid -> P2p_peer.Id.pp ppf pid
end

module Event = struct
  type update = Ignored_head | Branch_switch | Head_increment

  type synchronisation_status =
    | Synchronised of {is_chain_stuck : bool}
    | Not_synchronised

  type t =
    | Processed_block of {
        request : Request.view;
        request_status : Worker_types.request_status;
        update : update;
        fitness : Fitness.t;
        level : Int32.t;
        timestamp : Time.Protocol.t;
      }
    | Notify_branch of P2p_peer.Id.t
    | Notify_head of P2p_peer.Id.t
    | Connection of P2p_peer.Id.t
    | Disconnection of P2p_peer.Id.t
    | Could_not_switch_testchain of error trace
    | Bootstrapped
    | Sync_status of synchronisation_status
    | Bootstrap_active_peers of {active : int; needed : int}
    | Bootstrap_active_peers_heads_time of {
        min_head_time : Time.Protocol.t;
        max_head_time : Time.Protocol.t;
        most_recent_validation : Time.Protocol.t;
      }
    | Request_failure of
        Request.view * Worker_types.request_status * error trace

  type view = t

  let view t = t

  let level = function
    | Processed_block req -> (
        match req.update with
        | Ignored_head -> Internal_event.Info
        | Branch_switch | Head_increment -> Internal_event.Notice)
    | Could_not_switch_testchain _ -> Internal_event.Error
    | Notify_head _ -> Internal_event.Debug
    | Notify_branch _ -> Internal_event.Info
    | Connection _ | Disconnection _ -> Internal_event.Info
    | Bootstrapped -> Internal_event.Notice
    | Sync_status sync_status -> (
        match sync_status with
        | Synchronised {is_chain_stuck} ->
            if is_chain_stuck then Internal_event.Error
            else Internal_event.Notice
        | Not_synchronised -> Internal_event.Warning)
    | Bootstrap_active_peers _ -> Internal_event.Debug
    | Bootstrap_active_peers_heads_time _ -> Internal_event.Debug
    | Request_failure _ -> Internal_event.Notice

  let update_encoding =
    let open Data_encoding in
    def
      "chain_update"
      ~description:
        "If 'ignored', the new validated block is ignored since the current \
         head fitness is better. If 'branch', we have set our head to a new \
         validated block which is not the direct successor of the previous \
         head. If 'increment', the new validated head is the direct successor \
         of the previous head."
      (string_enum
         [
           ("ignored", Ignored_head);
           ("branch", Branch_switch);
           ("increment", Head_increment);
         ])

  let sync_status_encoding =
    let open Data_encoding in
    def
      "chain_status"
      ~description:
        "If 'unsynced', the node is not currently synchronized with of its \
         peers (it is probably still bootstrapping and its head is lagging \
         behind the chain's).\n\
         If 'synced', the node considers itself synchronized with its peers \
         and the current head timestamp is recent.\n\
         If 'stuck', the node considers itself synchronized with its peers but \
         the chain seems to be halted from its viewpoint."
      (string_enum
         [
           ("synced", Synchronised {is_chain_stuck = false});
           ("unsynced", Not_synchronised);
           ("stuck", Synchronised {is_chain_stuck = true});
         ])

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Processed_block"
          (obj1
             (req
                "processed_block"
                (obj6
                   (req "request" Request.encoding)
                   (req "status" Worker_types.request_status_encoding)
                   (req "outcome" update_encoding)
                   (req "fitness" Fitness.encoding)
                   (req "level" int32)
                   (req "timestamp" Time.Protocol.encoding))))
          (function
            | Processed_block
                {request; request_status; update; fitness; level; timestamp} ->
                Some (request, request_status, update, fitness, level, timestamp)
            | _ -> None)
          (fun (request, request_status, update, fitness, level, timestamp) ->
            Processed_block
              {request; request_status; update; fitness; level; timestamp});
        case
          (Tag 1)
          ~title:"Could_not_switch_testchain"
          (obj1 (req "could_not_switch_testchain" RPC_error.encoding))
          (function Could_not_switch_testchain err -> Some err | _ -> None)
          (fun err -> Could_not_switch_testchain err);
        case
          (Tag 2)
          ~title:"Bootstrapped"
          (obj1 (req "bootstrapped" unit))
          (function Bootstrapped -> Some () | _ -> None)
          (fun () -> Bootstrapped);
        case
          (Tag 3)
          ~title:"Sync_status"
          sync_status_encoding
          (function Sync_status sync_status -> Some sync_status | _ -> None)
          (fun sync_status -> Sync_status sync_status);
        case
          (Tag 4)
          ~title:"Bootstrap_active_peers"
          (obj1
             (req
                "bootstrap_active_peers"
                (obj2 (req "active" int31) (req "needed" int31))))
          (function
            | Bootstrap_active_peers {active; needed} -> Some (active, needed)
            | _ -> None)
          (fun (active, needed) -> Bootstrap_active_peers {active; needed});
        case
          (Tag 5)
          ~title:"Bootstrap_active_peers_heads_time"
          (obj1
             (req
                "bootstrap_active_peers_head_time"
                (obj3
                   (req "min_head_time" Time.Protocol.encoding)
                   (req "max_head_time" Time.Protocol.encoding)
                   (req "most_recent_validation" Time.Protocol.encoding))))
          (function
            | Bootstrap_active_peers_heads_time
                {min_head_time; max_head_time; most_recent_validation} ->
                Some (min_head_time, max_head_time, most_recent_validation)
            | _ -> None)
          (fun (min_head_time, max_head_time, most_recent_validation) ->
            Bootstrap_active_peers_heads_time
              {min_head_time; max_head_time; most_recent_validation});
        case
          (Tag 6)
          ~title:"notify_branch"
          (obj1
             (req "notify_branch" (obj1 (req "peer_id" P2p_peer.Id.encoding))))
          (function Notify_branch peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Notify_branch peer_id);
        case
          (Tag 7)
          ~title:"notify_head"
          (obj1 (req "notify_head" (obj1 (req "peer_id" P2p_peer.Id.encoding))))
          (function Notify_head peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Notify_head peer_id);
        case
          (Tag 8)
          ~title:"disconnection"
          (obj1
             (req
                "disconnection"
                (obj2
                   (req "kind" Data_encoding.string)
                   (req "peer_id" P2p_peer.Id.encoding))))
          (function
            | Disconnection peer_id -> Some ("disconnection", peer_id)
            | _ -> None)
          (fun (_, peer_id) -> Disconnection peer_id);
        case
          (Tag 9)
          ~title:"request_failure"
          (obj1
             (req
                "request_failure"
                (obj3
                   (req "failed_validation" Request.encoding)
                   (req "status" Worker_types.request_status_encoding)
                   (req "errors" RPC_error.encoding))))
          (function
            | Request_failure (r, s, err) -> Some (r, s, err) | _ -> None)
          (fun (r, s, err) -> Request_failure (r, s, err));
        case
          (Tag 10)
          ~title:"connection"
          (obj2
             (req "kind" Data_encoding.string)
             (req "peer_id" P2p_peer.Id.encoding))
          (function
            | Connection peer_id -> Some ("connection", peer_id) | _ -> None)
          (fun (_, peer_id) -> Connection peer_id);
      ]

  let sync_status_to_string = function
    | Synchronised {is_chain_stuck = false} -> "sync"
    | Not_synchronised -> "unsync"
    | Synchronised {is_chain_stuck = true} -> "stuck"

  let pp ppf = function
    | Processed_block req ->
        Format.fprintf ppf "@[<v 0>" ;
        (match req.update with
        | Ignored_head ->
            Format.fprintf
              ppf
              "Current head is better than %a (level %ld, timestamp %a, \
               fitness %a), we do not switch@,"
        | Branch_switch ->
            Format.fprintf
              ppf
              "Update current head to %a (level %ld, timestamp %a, fitness \
               %a), changing branch@,"
        | Head_increment ->
            Format.fprintf
              ppf
              "Update current head to %a (level %ld, timestamp %a, fitness \
               %a), same branch@,")
          Request.pp
          req.request
          req.level
          Time.Protocol.pp_hum
          req.timestamp
          Fitness.pp
          req.fitness ;
        Format.fprintf ppf "%a@]" Worker_types.pp_status req.request_status
    | Notify_branch peer_id ->
        Format.fprintf ppf "Notify branch from %a" P2p_peer.Id.pp peer_id
    | Notify_head peer_id ->
        Format.fprintf ppf "Notify head from %a" P2p_peer.Id.pp peer_id
    | Connection peer_id ->
        Format.fprintf ppf "Connection of %a" P2p_peer.Id.pp peer_id
    | Disconnection peer_id ->
        Format.fprintf ppf "Disconnection of %a" P2p_peer.Id.pp peer_id
    | Could_not_switch_testchain err ->
        Format.fprintf
          ppf
          "@[<v 0>Error while switching test chain:@ %a@]"
          (Format.pp_print_list Error_monad.pp)
          err
    | Bootstrapped -> Format.fprintf ppf "@[<v 0>Chain is bootstrapped@]"
    | Sync_status ss ->
        Format.fprintf ppf "@[<v 0>Sync_status: %s@]" (sync_status_to_string ss)
    | Bootstrap_active_peers {active; needed} ->
        Format.fprintf
          ppf
          "@[<v 0>Bootstrap peers: active %d needed %d@]"
          active
          needed
    | Bootstrap_active_peers_heads_time
        {min_head_time; max_head_time; most_recent_validation} ->
        Format.fprintf
          ppf
          "@[<v 0>Bootstrap peers: least recent head time stamp %a, \
           most_recent_head_timestamp %a, most recent validation %a@]"
          Time.Protocol.pp_hum
          min_head_time
          Time.Protocol.pp_hum
          max_head_time
          Time.Protocol.pp_hum
          most_recent_validation
    | Request_failure (req, {pushed; treated; completed}, errs) ->
        Format.fprintf
          ppf
          "@[<v 0>Chain validator request %a failed@,%a, %a@]"
          Request.pp
          req
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errs
end

module Distributed_db_state = struct
  type table_scheduler = {table_length : int; scheduler_length : int}

  type view = {
    p2p_readers_length : int;
    active_chains_length : int;
    operation_db : table_scheduler;
    operations_db : table_scheduler;
    block_header_db : table_scheduler;
    active_connections_length : int;
    active_peers_length : int;
  }

  let table_scheduler_encoding =
    let open Data_encoding in
    conv
      (fun {table_length; scheduler_length} -> (table_length, scheduler_length))
      (fun (table_length, scheduler_length) -> {table_length; scheduler_length})
      (obj2 (req "table_length" int31) (req "scheduler_length" int31))

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             p2p_readers_length;
             active_chains_length;
             operation_db;
             operations_db;
             block_header_db;
             active_connections_length;
             active_peers_length;
           } ->
        ( p2p_readers_length,
          active_chains_length,
          operation_db,
          operations_db,
          block_header_db,
          active_connections_length,
          active_peers_length ))
      (fun ( p2p_readers_length,
             active_chains_length,
             operation_db,
             operations_db,
             block_header_db,
             active_connections_length,
             active_peers_length ) ->
        {
          p2p_readers_length;
          active_chains_length;
          operation_db;
          operations_db;
          block_header_db;
          active_connections_length;
          active_peers_length;
        })
      (obj7
         (req "p2p_readers" int31)
         (req "active_chains" int31)
         (req "operation_db" table_scheduler_encoding)
         (req "operations_db" table_scheduler_encoding)
         (req "block_header_db" table_scheduler_encoding)
         (req "active_connections" int31)
         (req "active_peers" int31))
end
