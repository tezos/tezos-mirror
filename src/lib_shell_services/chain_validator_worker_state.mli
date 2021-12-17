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

module Request : sig
  type view = Hash of Block_hash.t | PeerId of P2p_peer.Id.t

  val encoding : view Data_encoding.encoding

  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type update = Ignored_head | Branch_switch | Head_increment

  val update_encoding : update Data_encoding.t

  type synchronisation_status =
    | Synchronised of {is_chain_stuck : bool}
    | Not_synchronised

  val sync_status_encoding : synchronisation_status Data_encoding.t

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

  val view : t -> view

  val level : t -> Internal_event.level

  val encoding : t Data_encoding.encoding

  val pp : Format.formatter -> t -> unit
end

module Distributed_db_state : sig
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

  val encoding : view Data_encoding.encoding
end
