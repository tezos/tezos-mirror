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

module Revelation = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "reveal"]

  let no_nonce_reveal =
    declare_1
      ~section
      ~level
      ~name:"no_nonce_reveal"
      ~msg:"nothing to reveal for block {block}"
      ("block", Block_hash.encoding)

  let reveal_nonce =
    declare_5
      ~section
      ~level
      ~name:"reveal_nonce"
      ~msg:
        "revealing nonce {nonce} from level {level} for chain {chain}, block \
         {block} with operation {operation}"
      ("nonce", Alpha_context.Nonce.encoding)
      ("level", Alpha_context.Raw_level.encoding)
      ("chain", Data_encoding.string)
      ("block", Data_encoding.string)
      ("operation", Operation_hash.encoding)
end

module Nonces = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "nonces"]

  let cannot_retrieve_block_header =
    declare_2
      ~section
      ~level:Warning
      ~name:"cannot_retrieve_block_header"
      ~msg:"cannot retrieve block {block} header associated to nonce: {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("block", Data_encoding.string)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let cannot_retrieve_head_level =
    declare_0
      ~section
      ~level:Error
      ~name:"cannot_retrieve_head_level"
      ~msg:"cannot fetch chain's head level; aborting nonce filtering"
      ()

  let too_many_orphans =
    declare_1
      ~section
      ~level:Warning
      ~name:"too_many_orphans"
      ~msg:
        "found too many nonces associated to blocks unknown by the node in \
         '$TEZOS_CLIENT/{filename}'; after checking that these blocks were \
         never included in the chain (e.g. via a block explorer), consider \
         using `tezos-client filter orphan nonces` to clear them"
      ("filename", Data_encoding.string)

  let found_nonce =
    declare_2
      ~section
      ~level
      ~name:"found_nonce"
      ~msg:"found nonce to reveal for {hash} (level: {level})"
      ("hash", Block_hash.encoding)
      ("level", Alpha_context.Raw_level.encoding)

  let bad_nonce =
    declare_1
      ~section
      ~level:Error
      ~name:"bad_nonce"
      ~msg:"incoherent nonce for level {level}"
      ("level", Alpha_context.Raw_level.encoding)
end

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

  let double_endorsement_detected =
    declare_2
      ~section
      ~level
      ~name:"double_endorsement_detected"
      ~msg:"double endorsement detected"
      ("existing_endorsement", Operation_hash.encoding)
      ("new_endorsement", Operation_hash.encoding)

  let double_endorsement_denounced =
    declare_2
      ~section
      ~level
      ~name:"double_endorsement_denounced"
      ~msg:"double endorsement evidence injected: {hash}"
      ("hash", Operation_hash.encoding)
      ~pp2:pp_ignore
      ("bytes", Data_encoding.bytes)

  let double_preendorsement_detected =
    declare_2
      ~section
      ~level
      ~name:"double_preendorsement_detected"
      ~msg:"double preendorsement detected"
      ("existing_preendorsement", Operation_hash.encoding)
      ("new_preendorsement", Operation_hash.encoding)

  let double_preendorsement_denounced =
    declare_2
      ~section
      ~level
      ~name:"double_preendorsement_denounced"
      ~msg:"double preendorsement evidence injected: {hash}"
      ("hash", Operation_hash.encoding)
      ~pp2:pp_ignore
      ("bytes", Data_encoding.bytes)

  let inconsistent_endorsement =
    declare_1
      ~section
      ~level:Error
      ~name:"inconsistent_endorsement"
      ~msg:"inconsistent endorsement found {hash}"
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
      ~section
      ~level
      ~name:"double_baking_detected"
      ~msg:"double baking detected"
      ()

  let double_baking_denounced =
    declare_2
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
    declare_2
      ~section
      ~level:Error
      ~name:"fetch_operations_error"
      ~msg:"error while fetching operations in block {hash} {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("hash", Block_hash.encoding)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

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

module Baking_forge = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "baking_forge"]

  let double_bake_near_miss =
    declare_1
      ~section
      ~level:Error
      ~name:"double_bake_near_miss"
      ~msg:"level {level}: previously baked"
      ("level", Alpha_context.Raw_level.encoding)

  let inject_baked_block =
    declare_3
      ~section
      ~level:Info
      ~name:"inject_baked_block"
      ~msg:"Client_baking_forge.inject_block: inject {hash}"
      ("hash", Block_hash.encoding)
      ~pp2:pp_ignore
      ("header", Data_encoding.bytes)
      ~pp3:Format.(pp_print_list @@ pp_print_list @@ Operation.pp)
      ( "operations",
        Data_encoding.(list @@ list @@ dynamic_size Operation.encoding) )

  let baking_local_validation_start =
    declare_1
      ~section
      ~level:Debug
      ~name:"baking_local_validation_start"
      ~msg:"starting client-side validation after {hash}"
      ("hash", Block_hash.encoding)

  let context_fetch_error =
    declare_1
      ~section
      ~level:Error
      ~name:"context_fetch_error"
      ~msg:"error while fetching current context: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let reopen_context =
    declare_0
      ~section
      ~level
      ~name:"reopen_context"
      ~msg:"retrying to open the context"
      ()

  let baking_rejected_invalid_operation =
    declare_2
      ~section
      ~level:Debug
      ~name:"baking_rejected_invalid_operation"
      ~msg:"client-side validation: filtered invalid operation {hash} {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("hash", Operation_hash.encoding)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let shell_prevalidation_notice =
    declare_0
      ~section
      ~level
      ~name:"shell_prevalidation_notice"
      ~msg:"building a block using shell validation"
      ()

  let shell_prevalidation_new_protocol =
    declare_0
      ~section
      ~level
      ~name:"shell_prevalidation_new_protocol"
      ~msg:"new protocol detected: using shell validation"
      ()

  let found_valid_operations =
    declare_4
      ~section
      ~level
      ~name:"found_valid_operations"
      ~msg:
        "found {valid_count} valid operations ({refused_count} refused) for \
         timestamp {timestamp} (fitness {fitness})"
      ~pp4:Fitness.pp
      ("valid_count", Data_encoding.int31)
      ("refused_count", Data_encoding.int31)
      ("timestamp", Time.System.encoding)
      ("fitness", Fitness.encoding)

  let block_conversion_failed =
    declare_1
      ~section
      ~level:Error
      ~name:"block_conversion_failed"
      ~msg:"error on raw_level conversion: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let block_injection_failed =
    declare_2
      ~section
      ~level:Error
      ~name:"block_injection_failed"
      ~msg:
        "error while injecting block; included operations: {operations}; \
         errors: {errors}"
      ~pp1:(Format.pp_print_list Operation.pp)
      ~pp2:pp_print_top_error_of_trace
      ("operations", Data_encoding.(list @@ dynamic_size Operation.encoding))
      ("errors", Error_monad.trace_encoding)

  let built_invalid_block_error =
    declare_1
      ~section
      ~level:Error
      ~name:"built_invalid_block_error"
      ~msg:
        "shell-side validation: error while prevalidating operations: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let try_baking =
    declare_4
      ~section
      ~level:Debug
      ~name:"try_baking"
      ~msg:
        "try baking after {hash} (slot {priority}) for {client} ({timestamp})"
      ("hash", Block_hash.encoding)
      ("priority", Data_encoding.int31)
      ("client", Data_encoding.string)
      ("timestamp", Time.System.encoding)

  let new_head_received =
    declare_0
      ~section
      ~level
      ~name:"new_head_received"
      ~msg:
        "received a new head while waiting for operations; aborting this block"
      ()

  let client_side_validation_error =
    declare_1
      ~section
      ~level:Error
      ~name:"client_side_validation_error"
      ~msg:
        "client-side validation: error while filtering invalid operations: \
         {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let waiting_before_injection =
    declare_2
      ~section
      ~level
      ~name:"waiting_before_injection"
      ~msg:
        "[{current_timestamp}] not ready to inject yet, waiting until \
         {valid_timestamp}"
      ("current_timestamp", Time.System.encoding)
      ("valid_timestamp", Time.System.encoding)

  let try_forging =
    declare_4
      ~section
      ~level:Debug
      ~name:"try_forging"
      ~msg:
        "try forging locally the block header for {hash} (slot {priority}) for \
         {client} ({timestamp})"
      ("hash", Block_hash.encoding)
      ("priority", Data_encoding.int31)
      ("client", Data_encoding.string)
      ("timestamp", Time.System.encoding)

  let start_injecting_block =
    declare_5
      ~section
      ~level:Info
      ~name:"start_injecting_block"
      ~msg:
        "injecting block (priority {priority}, fitness {fitness}) for {client} \
         after {predecessor}"
      ~pp2:Fitness.pp
      ("priority", Data_encoding.int31)
      ("fitness", Fitness.encoding)
      ("client", Data_encoding.string)
      ("predecessor", Block_hash.encoding)
      ("baker", Client_keys.Public_key_hash.encoding)

  let injected_block =
    declare_7
      ~section
      ~level
      ~name:"injected_block"
      ~msg:
        "injected block {block_hash} for {client} after {predecessor} (level \
         {level}, priority {priority}, fitness {fitness}, operations \
         {operations})"
      ~pp6:Fitness.pp
      ~pp7:Format.(pp_print_list Operation.pp)
      ("block_hash", Block_hash.encoding)
      ("client", Data_encoding.string)
      ("predecessor", Block_hash.encoding)
      ("level", Alpha_context.Raw_level.encoding)
      ("priority", Data_encoding.int31)
      ("fitness", Fitness.encoding)
      ("operations", Data_encoding.(list @@ dynamic_size Operation.encoding))

  let baking_slot_fetch_errors =
    declare_1
      ~section
      ~level:Error
      ~name:"baking_slot_fetch_errors"
      ~msg:"error while fetching baking possibilities: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let no_slot_found =
    declare_2
      ~section
      ~level
      ~name:"no_slot_found"
      ~msg:"no slot found at level {level} (max_priority = {priority})"
      ("level", Alpha_context.Raw_level.encoding)
      ("priority", Data_encoding.int31)

  let have_baking_slot =
    declare_6
      ~section
      ~level
      ~name:"have_baking_slot"
      ~msg:
        "new baking slot found (level {level}, priority {priority}) at \
         {timestamp} for {client} after {predecessor}"
      ("level", Alpha_context.Raw_level.encoding)
      ("priority", Data_encoding.int31)
      ("timestamp", Time.System.encoding)
      ("client", Data_encoding.string)
      ("predecessor", Block_hash.encoding)
      ("baker", Client_keys.Public_key_hash.encoding)

  let read_nonce_fail =
    declare_1
      ~section
      ~level:Error
      ~name:"read_nonce_fail"
      ~msg:"cannot read nonces: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let nonce_retrieval_fail =
    declare_1
      ~section
      ~level:Error
      ~name:"nonce_retrieval_fail"
      ~msg:"cannot retrieve unrevealed nonces: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let nonce_injection_fail =
    declare_1
      ~section
      ~level:Error
      ~name:"nonce_injection_fail"
      ~msg:"cannot inject nonces: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let retrying_on_error =
    declare_1
      ~section
      ~level:Error
      ~name:"retrying_on_error"
      ~msg:"retrying after baking error {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let endorsement_received =
    declare_2
      ~section
      ~level:Info
      ~name:"endorsement_received"
      ~msg:"received endorsement for slot {slot} (power: {power})"
      ~pp1:Format.pp_print_int
      ("slot", Data_encoding.int31)
      ~pp2:Format.pp_print_int
      ("power", Data_encoding.int31)

  let expected_validity_time =
    declare_2
      ~section
      ~name:"expected_validity_time"
      ~level:Info
      ~msg:"expected validity time: {time} (endorsing power: {power})"
      ~pp1:Alpha_context.Timestamp.pp
      ("time", Alpha_context.Timestamp.encoding)
      ~pp2:Format.pp_print_int
      ("power", Data_encoding.int31)

  let reading_per_block =
    declare_1
      ~section
      ~name:"reading_per_block"
      ~level:Notice
      ~msg:"reading per block vote file path: {path}"
      ("path", Data_encoding.string)

  let per_block_vote_file_notice =
    declare_1
      ~section
      ~name:"per_block_vote_file_notice"
      ~level:Notice
      ~msg:"per block vote file {event}"
      ("event", Data_encoding.string)

  let reading_liquidity_baking =
    declare_0
      ~section
      ~name:"reading_liquidity_baking"
      ~level:Notice
      ~msg:"reading liquidity baking escape vote"
      ()

  let liquidity_baking_escape_vote =
    declare_1
      ~section
      ~name:"liquidity_baking_escape_vote"
      ~level:Notice
      ~msg:"liquidity baking escape vote = {value}"
      ("value", Data_encoding.bool)

  let per_block_vote_file_fail =
    declare_1
      ~section
      ~name:"per_block_vote_file_error"
      ~level:Notice
      ~msg:"Error reading the block vote file: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let liquidity_baking_escape =
    declare_0
      ~section
      ~name:"liquidity_baking_continue"
      ~level:Notice
      ~msg:"Will vote to escape Liquidity Baking"
      ()

  let liquidity_baking_continue =
    declare_0
      ~section
      ~name:"liquidity_baking_escape"
      ~level:Notice
      ~msg:"Will vote to continue Liquidity Baking"
      ()
end

module Endorsement = struct
  include Internal_event.Simple

  let section = [Protocol.name; "delegate"; "endorsement"]

  let double_endorsement_near_miss =
    declare_1
      ~section
      ~level:Error
      ~name:"double_endorsement_near_miss"
      ~msg:"level {level}: previously endorsed"
      ("level", Alpha_context.Raw_level.encoding)

  let injected_endorsement =
    declare_5
      ~section
      ~level
      ~name:"injected_endorsement"
      ~msg:
        "injected endorsement for block '{block_hash}' (level {level}, \
         contract {client}) '{op_hash}'"
      ("block_hash", Block_hash.encoding)
      ("level", Alpha_context.Raw_level.encoding)
      ("client", Data_encoding.string)
      ("op_hash", Operation_hash.encoding)
      ("baker", Client_keys.Public_key_hash.encoding)

  let endorsing =
    declare_3
      ~section
      ~level:Debug
      ~name:"endorsing"
      ~msg:"endorsing {block} for {client} (level {level})!"
      ("block", Block_hash.encoding)
      ("client", Data_encoding.string)
      ("level", Alpha_context.Raw_level.encoding)

  let check_endorsement_ok =
    declare_2
      ~section
      ~level:Debug
      ~name:"check_endorsement_ok"
      ~msg:"checking if allowed to endorse block {block} for {client}"
      ("block", Block_hash.encoding)
      ("client", Data_encoding.string)

  let endorsement_no_slots_found =
    declare_2
      ~section
      ~level:Debug
      ~name:"endorsement_no_slots_found"
      ~msg:"no slot found for {block}/{client}"
      ("block", Block_hash.encoding)
      ("client", Data_encoding.string)

  let endorsement_slots_found =
    declare_3
      ~section
      ~level:Debug
      ~name:"endorsement_slots_found"
      ~msg:"found slots for {block}/{client} ({slots})"
      ~pp3:
        Format.(
          fun fmt ->
            fprintf
              fmt
              "[%a]"
              (pp_print_list
                 ~pp_sep:(fun f () -> pp_print_string f "; ")
                 Format.pp_print_int))
      ("block", Block_hash.encoding)
      ("client", Data_encoding.string)
      ("slots", Data_encoding.list Data_encoding.int31)

  let previously_endorsed =
    declare_1
      ~section
      ~level:Debug
      ~name:"previously_endorsed"
      ~msg:"level {level} (or higher) previously endorsed: do not endorse"
      ("level", Alpha_context.Raw_level.encoding)

  let endorsement_stale_block =
    declare_1
      ~section
      ~level:Info
      ~name:"endorsement_stale_block"
      ~msg:"ignore block {block}: forged too far the past"
      ("block", Block_hash.encoding)

  let endorsement_got_block =
    declare_1
      ~section
      ~level:Info
      ~name:"endorsement_got_block"
      ~msg:"received new block {block}"
      ("block", Block_hash.encoding)

  let wait_before_injecting =
    declare_2
      ~section
      ~level:Info
      ~name:"wait_before_injecting"
      ~msg:"waiting until {timestamp} ({timespan}) to inject endorsements"
      ("timestamp", Time.System.encoding)
      ("timespan", Time.System.Span.encoding)

  let error_while_endorsing =
    declare_2
      ~section
      ~level:Error
      ~name:"error_while_endorsing"
      ~msg:"error while injecting endorsement for baker {baker}: {errors}"
      ~pp2:pp_print_top_error_of_trace
      ("baker", Client_keys.Public_key_hash.encoding)
      ("errors", Error_monad.(TzTrace.encoding error_encoding))
end
