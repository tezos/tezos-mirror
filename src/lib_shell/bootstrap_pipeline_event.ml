(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

include Internal_event.Simple

let section = ["node"; "validator"; "bootstrap_pipeline"]

let fetching_step_from_peer =
  declare_4
    ~section
    ~name:"fetching_step_from_peer"
    ~msg:
      "fetching block {block} to {predecessor} ({count} headers) from peer \
       {peer_id}"
    ~level:Info
    ("block", Block_hash.encoding)
    ("predecessor", Block_hash.encoding)
    ("count", Data_encoding.int31)
    ("peer_id", P2p_peer.Id.encoding)

let still_fetching_large_step_from_peer =
  declare_3
    ~section
    ~name:"still_fetching_large_step_from_peer"
    ~msg:
      "fetched {fetched}/{length} headers from peer {peer_id}, and continuing"
    ~level:Notice
    ("fetched", Data_encoding.int31)
    ("length", Data_encoding.int31)
    ("peer_id", P2p_peer.Id.encoding)

let step_too_long =
  declare_1
    ~section
    ~name:"step_too_long"
    ~msg:"invalid step from peer {peer_id} (too long)"
    ~level:Info
    ("peer_id", P2p_peer.Id.encoding)

let step_too_short =
  declare_1
    ~section
    ~name:"step_too_short"
    ~msg:"invalid step from peer {peer_id} (too short)"
    ~level:Info
    ("peer_id", P2p_peer.Id.encoding)

let fetching_block_header_from_peer =
  declare_2
    ~section
    ~name:"fetching_block_header_from_peer"
    ~msg:"fetching header of {hash} from peer {peer_id}"
    ~level:Info
    ("hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let fetching_all_steps_from_peer =
  declare_1
    ~section
    ~name:"fetching_all_steps_from_peer"
    ~msg:"fetched all steps from peer {peer_id}"
    ~level:Info
    ("peer_id", P2p_peer.Id.encoding)

let header_request_timeout =
  declare_2
    ~section
    ~name:"header_request_timeout"
    ~msg:"request for header {block_hash} from peer {peer_id} timed out"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let locator_contains_future_block =
  declare_4
    ~section
    ~name:"locator_contains_future_block"
    ~msg:
      "block locator {block_hash} from peer {peer_id} contains future blocks"
    ~level:Notice
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)
    ("time", Time.System.encoding)
    ("block_time", Time.Protocol.encoding)

let locator_too_short =
  declare_0
    ~section
    ~name:"locator_too_short"
    ~msg:"received a locator that is too short"
    ~level:Info
    ()

let unexpected_error_while_fetching_headers =
  declare_0
    ~section
    ~name:"unexpected_error_while_fetching_headers"
    ~msg:"unexpected error while fetching headers"
    ~level:Error
    ()

let fetching_operations =
  declare_2
    ~section
    ~name:"fetching_operations"
    ~msg:"fetching operations of block {block_hash} from peer {peer_id}"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let fetched_operations =
  declare_2
    ~section
    ~name:"fetched_operations"
    ~msg:"fetched operations of block {block_hash} from peer {peer_id}"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let request_operations_timeout =
  declare_3
    ~section
    ~name:"request_operations_timeout"
    ~msg:
      "request for operations {block_hash}:{operations_index_tag} from peer \
       {peer_id} timed out"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("operations_index_tag", Data_encoding.int31)
    ("peer_id", P2p_peer.Id.encoding)

let requesting_validation =
  declare_2
    ~section
    ~name:"requesting_validation"
    ~msg:"requesting validation for block {block_hash} from peer {peer_id}"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let validated_block =
  declare_2
    ~section
    ~name:"validated_block"
    ~msg:"validated block {block_hash} from peer {peer_id}"
    ~level:Info
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)
