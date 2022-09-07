(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_rpc
open Protocol
open Alpha_context

(* We distinguish RPC endpoints served by the rollup node into `global` and
   `local`. The difference between the two lies in whether the responses
   given by different rollup nodes in the same state (see below for an
   exact definition) must be the same (in the case of global endpoints)
   or can differ (in the case of local endpoints).

   More formally,  two rollup nodes are in the same quiescent state if they are
   subscribed to the same rollup address, and have processed the same set of
   heads from the layer1. We only consider quiescent states, that is those
   where rollup nodes are not actively processing a head received from layer1.

   Examples of global endpoints are `current_inbox` and
   `last_stored_commitment`, as the responses returned by these endpoints
   is expected to be consistent across rollup nodes in the same state.

   An example of local endpoint is `last_published_commitments`, as two rollup
   nodes in the same state may either publish or not publish a commitment,
   according to whether its inbox level is below the inbox level of the
   last cemented commitment at the time they tried to publish the commitment.
   See below for a more detailed explanation.
*)

let commitment_with_hash_and_level_encoding =
  Data_encoding.(
    obj3
      (req "commitment" Sc_rollup.Commitment.encoding)
      (req "hash" Sc_rollup.Commitment.Hash.encoding)
      (opt "published_at_level" Raw_level.encoding))

module Global = struct
  open RPC_path

  let prefix = root / "global"

  let sc_rollup_address () =
    RPC_service.get_service
      ~description:"Smart-contract rollup address"
      ~query:RPC_query.empty
      ~output:Sc_rollup.Address.encoding
      (prefix / "sc_rollup_address")

  let current_tezos_head () =
    RPC_service.get_service
      ~description:"Tezos head known to the smart-contract rollup node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Block_hash.encoding)
      (prefix / "tezos_head")

  let current_tezos_level () =
    RPC_service.get_service
      ~description:"Tezos level known to the smart-contract rollup node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Data_encoding.int32)
      (prefix / "tezos_level")

  let current_inbox () =
    RPC_service.get_service
      ~description:"Current inbox"
      ~query:RPC_query.empty
      ~output:Sc_rollup.Inbox.encoding
      (prefix / "inbox")

  let current_ticks () =
    RPC_service.get_service
      ~description:"Current number of ticks for current level"
      ~query:RPC_query.empty
      ~output:Data_encoding.z
      (prefix / "ticks")

  let current_total_ticks () =
    RPC_service.get_service
      ~description:"Current total number of ticks"
      ~query:RPC_query.empty
      ~output:Sc_rollup.Tick.encoding
      (prefix / "total_ticks")

  let current_num_messages () =
    RPC_service.get_service
      ~description:"Current number of messages"
      ~query:RPC_query.empty
      ~output:Data_encoding.z
      (prefix / "current_num_messages")

  let current_state_hash () =
    RPC_service.get_service
      ~description:"Current state hash"
      ~query:RPC_query.empty
      ~output:Sc_rollup.State_hash.encoding
      (prefix / "state_hash")

  let last_stored_commitment () =
    RPC_service.get_service
      ~description:"Last commitment computed by the node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option commitment_with_hash_and_level_encoding)
      (prefix / "last_stored_commitment")

  let current_status () =
    RPC_service.get_service
      ~description:"Current PVM status"
      ~query:RPC_query.empty
      ~output:Data_encoding.string
      (prefix / "status")

  let dal_slot_subscriptions () =
    RPC_service.get_service
      ~description:"Current data availability layer slot subscriptions"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Dal.Slot_index.encoding)
      (prefix / "dal" / "slot_subscriptions")

  let dal_slots () =
    RPC_service.get_service
      ~description:"Data availability slots for a given block hash"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Dal.Slot.encoding)
      (prefix / "dal" / "slots")

  let dal_confirmed_slots () =
    RPC_service.get_service
      ~description:"Data availability confirmed slots for a given block hash"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Dal.Slot.encoding)
      (prefix / "dal" / "confirmed_slots")
end

module Local = struct
  open RPC_path

  let prefix = root / "local"

  (* commitments are published only if their inbox level is above the last
     cemented commitment level inbox level. Because this information is
     fetched from the head of the tezos node to which the rollup node is
     connected, it is possible that two rollup nodes that have processed
     the same set of heads, but whose corresponding layer1 node has
     different information about the last cemented commitment, will
     decide to publish and not to publish a commitment, respectively.
     As a consequence, the results returned by the endpoint below
     in the rollup node will be different.
  *)
  let last_published_commitment () =
    RPC_service.get_service
      ~description:"Last commitment published by the node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option commitment_with_hash_and_level_encoding)
      (prefix / "last_published_commitment")

  type state_value_query = {key : string}

  let state_value_query : state_value_query RPC_query.t =
    let open RPC_query in
    query (fun key -> {key})
    |+ field "key" RPC_arg.string "" (fun t -> t.key)
    |> seal

  let current_state_value () =
    RPC_service.get_service
      ~description:"Current state value"
      ~query:state_value_query
      ~output:Data_encoding.bytes
      RPC_path.(open_root / "state")
end
