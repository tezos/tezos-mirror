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

type synchronisation_status =
  | Synchronised of {is_chain_stuck : bool}
  | Not_synchronised

let sync_status_encoding =
  let open Data_encoding in
  def
    "chain_status"
    ~description:
      "If 'unsynced', the node is not currently synchronized with of its peers \
       (it is probably still bootstrapping and its head is lagging behind the \
       chain's).\n\
       If 'synced', the node considers itself synchronized with its peers and \
       the current head timestamp is recent.\n\
       If 'stuck', the node considers itself synchronized with its peers but \
       the chain seems to be halted from its viewpoint."
    (string_enum
       [
         ("synced", Synchronised {is_chain_stuck = false});
         ("unsynced", Not_synchronised);
         ("stuck", Synchronised {is_chain_stuck = true});
       ])

let sync_status_pp fmt = function
  | Synchronised {is_chain_stuck = true} -> Format.fprintf fmt "stuck"
  | Synchronised {is_chain_stuck = false} -> Format.fprintf fmt "synced"
  | Not_synchronised -> Format.fprintf fmt "unsynced"

type update = Ignored_head | Branch_switch | Head_increment

let update_encoding =
  let open Data_encoding in
  def
    "chain_update"
    ~description:
      "If 'ignored', the new validated block is ignored since the current head \
       fitness is better. If 'branch', we have set our head to a new validated \
       block which is not the direct successor of the previous head. If \
       'increment', the new validated head is the direct successor of the \
       previous head."
    (string_enum
       [
         ("ignored", Ignored_head);
         ("branch", Branch_switch);
         ("increment", Head_increment);
       ])

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
           }
         ->
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
             active_peers_length )
         ->
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
