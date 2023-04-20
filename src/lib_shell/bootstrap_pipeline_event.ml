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

(* notice level events *)

let fetching_locator =
  declare_2
    ~section
    ~name:"fetching_locator"
    ~msg:"fetching branch of about {locator_length} blocks from peer {peer_id}"
    ~level:Notice
    ("locator_length", Data_encoding.int31)
    ("peer_id", P2p_peer.Id.encoding)

let still_fetching_large_step_from_peer =
  declare_3
    ~section
    ~name:"still_fetching_block_header_from_peer"
    ~msg:
      "still fetching headers from peer {peer_id}: \
       {block_fetched}/{step_length}"
    ~level:Notice
    ("peer_id", P2p_peer.Id.encoding)
    ("block_fetched", Data_encoding.int31)
    ("step_length", Data_encoding.int31)

(* info level events *)

let fetching_step_from_peer =
  declare_6
    ~section
    ~name:"fetching_step_from_peer"
    ~msg:
      "fetching step {step_number}/{number_of_steps} (step length \
       {step_length}) from {block} to {predecessor} from peer {peer_id}"
    ~level:Info
    ("step_number", Data_encoding.int31)
    ("number_of_steps", Data_encoding.int31)
    ("step_length", Data_encoding.int31)
    ("block", Block_hash.encoding)
    ("predecessor", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let fetching_block_header_from_peer =
  declare_4
    ~section
    ~name:"fetching_block_header_from_peer"
    ~msg:"fetched header {block} from {peer_id} {block_fetched}/{step_length}"
    ~level:Debug
    ("block", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)
    ("block_fetched", Data_encoding.int31)
    ("step_length", Data_encoding.int31)

(* debug level events *)

let fetching_all_steps_from_peer =
  declare_1
    ~section
    ~name:"fetching_all_steps_from_peer"
    ~msg:"fetched all steps from peer {peer_id}"
    ~level:Debug
    ("peer_id", P2p_peer.Id.encoding)

let fetching_operations =
  declare_2
    ~section
    ~name:"fetching_operations"
    ~msg:"fetching operations of block {block_hash} from peer {peer_id}"
    ~level:Debug
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let fetched_operations =
  declare_2
    ~section
    ~name:"fetched_operations"
    ~msg:"fetched operations of block {block_hash} from peer {peer_id}"
    ~level:Debug
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let requesting_validation =
  declare_2
    ~section
    ~name:"requesting_validation"
    ~msg:"requesting validation for block {block_hash} from peer {peer_id}"
    ~level:Debug
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let validated_block =
  declare_2
    ~section
    ~name:"validated_block"
    ~msg:"validated block {block_hash} from peer {peer_id}"
    ~level:Debug
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

(* warning level events *)

let request_operations_timeout =
  declare_3
    ~section
    ~name:"request_operations_timeout"
    ~msg:
      "request for operations {block_hash}:{operations_index_tag} from peer \
       {peer_id} timed out"
    ~level:Warning
    ("block_hash", Block_hash.encoding)
    ("operations_index_tag", Data_encoding.int31)
    ("peer_id", P2p_peer.Id.encoding)

let step_too_long =
  declare_1
    ~section
    ~name:"step_too_long"
    ~msg:"invalid step from peer {peer_id} (too long)"
    ~level:Warning
    ("peer_id", P2p_peer.Id.encoding)

let step_too_short =
  declare_1
    ~section
    ~name:"step_too_short"
    ~msg:"invalid step from peer {peer_id} (too short)"
    ~level:Warning
    ("peer_id", P2p_peer.Id.encoding)

let header_request_timeout =
  declare_2
    ~section
    ~name:"header_request_timeout"
    ~msg:"request for header {block_hash} from peer {peer_id} timed out"
    ~level:Warning
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)

let locator_contains_future_block =
  declare_4
    ~section
    ~name:"locator_contains_future_block"
    ~msg:"block locator {block_hash} from peer {peer_id} contains future blocks"
    ~level:Warning
    ("block_hash", Block_hash.encoding)
    ("peer_id", P2p_peer.Id.encoding)
    ("time", Time.System.encoding)
    ("block_time", Time.Protocol.encoding)

let locator_too_short =
  declare_0
    ~section
    ~name:"locator_too_short"
    ~msg:"received a locator that is too short"
    ~level:Warning
    ()

(* error level events *)

let unexpected_error_while_fetching_headers =
  declare_1
    ~section
    ~name:"unexpected_error_while_fetching_headers"
    ~msg:"unexpected error while fetching headers: {trace}"
    ~level:Error
    ~pp1:pp_print_top_error_of_trace
    ("trace", Error_monad.trace_encoding)
