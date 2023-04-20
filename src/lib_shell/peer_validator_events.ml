(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Request = Peer_validator_worker_state.Request
include Internal_event.Simple

let section = ["validator"; "peer"]

let validating_new_branch =
  declare_2
    ~section
    ~name:"validating_new_branch"
    ~msg:"validating new branch from peer {peer} (approx. {length} blocks)"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Format.pp_print_int
    ("peer", P2p_peer.Id.encoding)
    ("length", Data_encoding.int31)

let new_branch_validated =
  declare_2
    ~section
    ~name:"new_branch_validated"
    ~msg:"branch from {peer} validated with head {hash}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let fetching_operations_for_head =
  declare_2
    ~section
    ~name:"fetching_operations_for_head"
    ~msg:"fetching operaitons from {peer} for head {hash}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let requesting_new_head_validation =
  declare_2
    ~section
    ~name:"requesting_new_head_validation"
    ~msg:"requesting new head validation from {peer} for head {hash}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let new_head_validation_end =
  declare_2
    ~section
    ~name:"new_head_validation_end"
    ~msg:"new head validation from {peer} for head {hash} ended"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_head =
  declare_2
    ~section
    ~name:"ignoring_head"
    ~msg:"ignoring head {hash} from peer {peer} with non-increasing fitness"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_previously_validated_block =
  declare_2
    ~section
    ~name:"ignoring_previously_validated_block"
    ~msg:"ignoring previously validated head {hash} from {peer}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_invalid_block =
  declare_2
    ~section
    ~name:"ignoring_invalid_block"
    ~msg:"ignoring invalid block {hash} from {peer}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_valid_block =
  declare_2
    ~section
    ~name:"ignoring_prechecked_block"
    ~msg:"ignoring prechecked head {hash} from {peer}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_prechecked_invalid_block =
  declare_2
    ~section
    ~name:"ignoring_prechecked_invalid_block"
    ~msg:"ignoring invalid block {hash} from {peer}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let missing_new_head_predecessor =
  declare_2
    ~section
    ~name:"missing_new_head_predecessor"
    ~msg:"missing new head's predecessor {hash} from {peer}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_branch_with_invalid_locator =
  declare_2
    ~section
    ~name:"ignoring_branch_with_invalid_locator"
    ~msg:"ignoring branch with head {hash} from {peer} (invalid locator)"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let ignoring_branch_without_common_ancestor =
  declare_2
    ~section
    ~name:"ignoring_branch_without_common_ancestor"
    ~msg:"ignoring branch with head {hash} from {peer} (no common ancestor)"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let no_new_head_from_peer =
  declare_2
    ~section
    ~name:"no_new_head_from_peer"
    ~msg:"no new head from peer {peer} for {delay}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Time.System.Span.pp_hum
    ("peer", P2p_peer.Id.encoding)
    ("delay", Time.System.Span.encoding)

let processing_new_head =
  declare_2
    ~section
    ~name:"processing_new_head"
    ~msg:"processing new head fron peer {peer}: {hash}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let processing_new_branch =
  declare_2
    ~section
    ~name:"processing_new_branch"
    ~msg:"processing new branch fron peer {peer}: {hash}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Block_hash.pp
    ("peer", P2p_peer.Id.encoding)
    ("hash", Block_hash.encoding)

let terminating_worker =
  declare_2
    ~section
    ~name:"terminating_worker"
    ~msg:"terminating the validation worker for peer {peer} ({reason})"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Format.pp_print_string
    ("peer", P2p_peer.Id.encoding)
    ("reason", Data_encoding.string)

let request_completed =
  declare_2
    ~section
    ~name:"request_completed"
    ~msg:"request {view} completed ({status})"
    ~level:Debug
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status
    ("view", Request.encoding)
    ("status", Worker_types.request_status_encoding)

let request_error =
  declare_3
    ~section
    ~name:"request_error"
    ~msg:"request {view} failed ({status}): {error}"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status
    ~pp3:Error_monad.pp_print_trace
    ("view", Request.encoding)
    ("status", Worker_types.request_status_encoding)
    ("error", Error_monad.trace_encoding)

let peer_disconnection =
  declare_1
    ~section
    ~name:"peer_disconnection"
    ~msg:"peer {peer} disconnected"
    ~level:Notice
    ("peer", P2p_peer.Id.encoding)
    ~pp1:P2p_peer.Id.pp
