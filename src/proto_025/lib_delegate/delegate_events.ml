(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <marcin.pastudzki@tqtezos.com> *)
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

open Protocol

let level = Internal_event.Notice

(* Ignore the value in the output. *)
let pp_ignore fmt _ = Format.pp_print_string fmt ""

module Denunciator = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "denunciation"]

  let invalid_level_conversion =
    declare_1
      ~section
      ~level:Error
      ~name:"invalid_level_conversion"
      ~msg:"invalid level conversion: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let double_attestation_detected =
    declare_2
      ~alternative_color:Internal_event.Magenta
      ~section
      ~level
      ~name:"double_attestation_detected"
      ~msg:"double attestation detected"
      ("existing_attestation", Operation_hash.encoding)
      ("new_attestation", Operation_hash.encoding)

  let double_attestation_denounced =
    declare_2
      ~alternative_color:Internal_event.Blue
      ~section
      ~level
      ~name:"double_attestation_denounced"
      ~msg:"double attestation evidence injected: {hash}"
      ("hash", Operation_hash.encoding)
      ~pp2:pp_ignore
      ("bytes", Data_encoding.bytes)

  let attestation_conflict_ignored =
    declare_2
      ~section
      ~level:Debug
      ~name:"attestation_conflict_ignored"
      ~msg:
        "{existing_attestation} is conflicting with {new_attestation} but no \
         misbehavior was found."
      ("existing_attestation", Operation_hash.encoding)
      ("new_attestation", Operation_hash.encoding)

  let double_preattestation_detected =
    declare_2
      ~alternative_color:Internal_event.Magenta
      ~section
      ~level
      ~name:"double_preattestation_detected"
      ~msg:"double preattestation detected"
      ("existing_preattestation", Operation_hash.encoding)
      ("new_preattestation", Operation_hash.encoding)

  let double_preattestation_denounced =
    declare_2
      ~alternative_color:Internal_event.Blue
      ~section
      ~level
      ~name:"double_preattestation_denounced"
      ~msg:"double preattestation evidence injected: {hash}"
      ("hash", Operation_hash.encoding)
      ~pp2:pp_ignore
      ("bytes", Data_encoding.bytes)

  let preattestation_conflict_ignored =
    declare_2
      ~section
      ~level:Debug
      ~name:"preattestation_conflict_ignored"
      ~msg:
        "{existing_preattestation} is conflicting with {new_preattestation} \
         but no misbehavior was found."
      ("existing_preattestation", Operation_hash.encoding)
      ("new_preattestation", Operation_hash.encoding)

  let double_consensus_already_denounced =
    declare_1
      ~section
      ~level:Debug
      ~name:"double_consensus_already_denounced"
      ~msg:"double consensus operation already denounced in {hash}"
      ("hash", Operation_hash.encoding)

  let consensus_operation_too_old =
    declare_1
      ~section
      ~level:Debug
      ~name:"consensus_operation_too_old"
      ~msg:"operation {hash} is too old to be handled"
      ("hash", Operation_hash.encoding)

  let consensus_operation_too_far_in_future =
    declare_1
      ~section
      ~level:Debug
      ~name:"consensus_operation_too_far_in_future"
      ~msg:"operation {hash} too far in the future"
      ("hash", Operation_hash.encoding)

  let inconsistent_attestation =
    declare_1
      ~section
      ~level:Error
      ~name:"inconsistent_attestation"
      ~msg:"inconsistent attestation found {hash}"
      ("hash", Operation_hash.encoding)

  let unexpected_pruned_block =
    declare_1
      ~section
      ~level:Error
      ~name:"unexpected_pruned_block"
      ~msg:"unexpected pruned block: {hash}"
      ("hash", Block_hash.encoding)

  let double_baking_but_not =
    declare_0
      ~section
      ~level:Debug
      ~name:"double_baking_but_not"
      ~msg:"double baking detected but block hashes are equivalent; skipping"
      ()

  let double_baking_detected =
    declare_0
      ~alternative_color:Internal_event.Magenta
      ~section
      ~level
      ~name:"double_baking_detected"
      ~msg:"double baking detected"
      ()

  let double_baking_denounced =
    declare_2
      ~alternative_color:Internal_event.Blue
      ~section
      ~level
      ~name:"double_baking_denounced"
      ~msg:"double baking evidence injected {hash}"
      ("hash", Operation_hash.encoding)
      ~pp2:pp_ignore
      ("bytes", Data_encoding.bytes)

  let protocol_change_detected =
    declare_0
      ~section
      ~level:Error
      ~name:"protocol_change_detected"
      ~msg:"protocol changing detected; skipping the block"
      ()

  let accuser_saw_block =
    declare_2
      ~section
      ~level:Debug
      ~name:"accuser_saw_block"
      ~msg:"block level: {level}"
      ("level", Alpha_context.Raw_level.encoding)
      ("hash", Block_hash.encoding)

  let fetch_operations_error =
    declare_1
      ~section
      ~level:Error
      ~name:"fetch_operations_error"
      ~msg:"error while fetching operations of block {hash}"
      ("hash", Block_hash.encoding)
      ~pp1:Block_hash.pp

  let accuser_processed_block =
    declare_1
      ~section
      ~level
      ~name:"accuser_processed_block"
      ~msg:"block {hash} registered"
      ("hash", Block_hash.encoding)

  let accuser_block_error =
    declare_2
      ~section
      ~level:Error
      ~name:"accuser_block_error"
      ~msg:"error while processing block {hash} {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("hash", Block_hash.encoding)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))
end

module Baking_scheduling = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "baking-scheduling"]

  let cannot_fetch_event =
    declare_1
      ~section
      ~level:Info
      ~name:"cannot_fetch_event"
      ~msg:"{worker}: can't fetch the current event; waiting for new event"
      ("worker", Data_encoding.string)

  let daemon_error =
    declare_2
      ~section
      ~level:Error
      ~name:"daemon_error"
      ~msg:"{worker}: error while baking: {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("worker", Data_encoding.string)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let daemon_setup =
    declare_1
      ~section
      ~level:Info
      ~name:"daemon_setup"
      ~msg:"setting up before the {worker} can start"
      ("worker", Data_encoding.string)

  let daemon_connection_lost =
    declare_1
      ~section
      ~level:Error
      ~name:"daemon_connection_lost"
      ~msg:"connection to node lost, {worker} exiting"
      ("worker", Data_encoding.string)

  let daemon_wakeup =
    declare_1
      ~section
      ~level:Debug
      ~name:"daemon_wakeup"
      ~msg:"waking up for {worker}"
      ("worker", Data_encoding.string)

  let daemon_start =
    declare_1
      ~section
      ~level:Info
      ~name:"daemon_start"
      ~msg:"starting {worker} daemon"
      ("worker", Data_encoding.string)
end
