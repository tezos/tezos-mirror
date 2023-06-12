(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Store_types
include Internal_event.Simple

let section = ["node"; "store"]

(* Info *)
let set_head =
  declare_1
    ~section
    ~level:Info
    ~name:"set_head"
    ~msg:"{block} set as new head"
    ~pp1:pp_block_descriptor
    ("block", block_descriptor_encoding)

let set_checkpoint =
  declare_1
    ~section
    ~level:Info
    ~name:"set_checkpoint"
    ~msg:"checkpoint updated to {new_checkpoint}"
    ~pp1:pp_block_descriptor
    ("new_checkpoint", block_descriptor_encoding)

let set_target =
  declare_1
    ~section
    ~level:Debug
    ~name:"set_target"
    ~msg:"the target was updated to {new_target}"
    ~pp1:pp_block_descriptor
    ("new_target", block_descriptor_encoding)

let set_savepoint =
  declare_1
    ~section
    ~level:Info
    ~name:"set_savepoint"
    ~msg:"the savepoint was updated to {new_savepoint}"
    ~pp1:pp_block_descriptor
    ("new_savepoint", block_descriptor_encoding)

let set_caboose =
  declare_1
    ~section
    ~level:Info
    ~name:"set_caboose"
    ~msg:"the caboose was updated to {new_caboose}"
    ~pp1:pp_block_descriptor
    ("new_caboose", block_descriptor_encoding)

let store_block =
  declare_1
    ~section
    ~level:Info
    ~name:"store_block"
    ~msg:"block {block} was stored"
    ~pp1:pp_block_descriptor
    ("block", block_descriptor_encoding)

let store_validated_block =
  declare_1
    ~section
    ~level:Info
    ~name:"store_validated_block"
    ~msg:"validated block {block} was stored"
    ~pp1:pp_block_descriptor
    ("block", block_descriptor_encoding)

let start_updating_floating_stores =
  declare_0
    ~section
    ~level:Info
    ~name:"start_updating_floating_stores"
    ~msg:"updating floating stores"
    ()

let start_cementing_blocks =
  declare_0
    ~section
    ~level:Info
    ~name:"start_cementing_blocks"
    ~msg:"cementing blocks"
    ()

let start_cementing_blocks_metadata =
  declare_0
    ~section
    ~level:Info
    ~name:"start_cementing_blocks_metadata"
    ~msg:"cementing blocks metadata"
    ()

let start_merging_thread =
  declare_0
    ~section
    ~level:Info
    ~name:"start_merging_thread"
    ~msg:"running merging thread"
    ()

let end_merging_thread =
  declare_0
    ~section
    ~level:Info
    ~name:"end_merging_thread"
    ~msg:"merging thread ended"
    ()

let start_store_garbage_collection =
  declare_0
    ~section
    ~level:Info
    ~name:"start_store_garbage_collection"
    ~msg:"garbage-collecting the cemented store"
    ()

let start_merge_finalizer =
  declare_0
    ~section
    ~level:Info
    ~name:"start_merge_finalizer"
    ~msg:"triggering merge finalizer"
    ()

let start_retreiving_predecessors =
  declare_0
    ~section
    ~level:Info
    ~name:"start_retreiving_predecessors"
    ~msg:"retrieving predecessors from floating store"
    ()

let start_retreiving_cycles =
  declare_0
    ~section
    ~level:Info
    ~name:"start_retreiving_cycles"
    ~msg:"retrieving cycles from floating store"
    ()

let store_is_consistent =
  declare_0
    ~section
    ~level:Debug
    ~name:"store_is_consistent"
    ~msg:"the store is consistent"
    ()

let metadata_read_error =
  declare_1
    ~section
    ~level:Debug
    ~name:"error_while_reading_cemented_metadata"
    ~msg:"unexpected error while reading cemented metadata: {exc}"
    ~pp1:Format.pp_print_string
    ("exc", Data_encoding.string)

(* Notice *)
let fork_testchain =
  declare_4
    ~section
    ~level:Notice
    ~name:"fork_testchain"
    ~msg:
      "the test chain {chain_id} for protocol {protocol_hash} with genesis \
       block hash {genesis_hash} was initialized from {fork_block} and is now \
       registered in the store"
    ~pp1:Chain_id.pp
    ("chain_id", Chain_id.encoding)
    ~pp2:Protocol_hash.pp
    ("protocol_hash", Protocol_hash.encoding)
    ~pp3:Block_hash.pp
    ("genesis_hash", Block_hash.encoding)
    ~pp4:pp_block_descriptor
    ("fork_block", block_descriptor_encoding)

let pp_int32 fmt i = Format.fprintf fmt "%ld" i

let start_merging_stores =
  declare_1
    ~section
    ~level:Notice
    ~name:"start_merging_stores"
    ~msg:"merging store up to block level {lafl}"
    ~pp1:pp_int32
    ("lafl", Data_encoding.int32)

let end_merging_stores =
  declare_1
    ~section
    ~level:Notice
    ~name:"end_merging_stores"
    ~msg:"store was successfully merged in {time}"
    ~pp1:Time.System.Span.pp_hum
    ("time", Time.System.Span.encoding)

let start_context_gc =
  declare_1
    ~section
    ~level:Info
    ~name:"start_context_gc"
    ~msg:"removing old contexts below block {block}"
    ~pp1:pp_block_descriptor
    ("block", block_descriptor_encoding)

let start_context_split =
  declare_1
    ~section
    ~level:Info
    ~name:"start_context_split"
    ~msg:"splitting context into a new chunk at level {level}"
    ~pp1:pp_int32
    ("level", Data_encoding.int32)

let context_gc_is_not_allowed =
  declare_0
    ~section
    ~level:Warning
    ~name:"gc_is_not_allowed"
    ~msg:
      "garbage collection is not fully enabled on this data directory: context \
       cannot be garbage collected. Please read the documentation or import a \
       snapshot to enable it"
    ()

let try_waiting_for_merge_termination =
  declare_0
    ~section
    ~level:Notice
    ~name:"try_waiting_for_merge_termination"
    ~msg:"try waiting for the store's merge completion"
    ()

let switch_history_mode =
  declare_2
    ~section
    ~level:Notice
    ~name:"switch_history_mode"
    ~msg:"history mode successfully switched from {old} to {new}"
    ~pp1:History_mode.pp
    ("old", History_mode.encoding)
    ~pp2:History_mode.pp
    ("new", History_mode.encoding)

let inconsistent_store =
  declare_1
    ~section
    ~level:Notice
    ~name:"inconsistent_store"
    ~msg:"the store is in an inconsistent state: {errs}"
    ~pp1:(fun ppf -> Format.fprintf ppf "%a" Error_monad.pp_print_trace)
    ("errs", Error_monad.trace_encoding)

let fix_store =
  declare_0
    ~section
    ~level:Notice
    ~name:"fix_store"
    ~msg:"attempting to restore the store's consistency..."
    ()

let fix_floating_stores =
  declare_0
    ~section
    ~level:Notice
    ~name:"fix_floating_stores"
    ~msg:"the consistency of the floating stores was restored"
    ()

let fix_head =
  declare_2
    ~section
    ~level:Notice
    ~name:"fix_head"
    ~msg:
      "updating head (previously {prev}) with the fittest block present in the \
       store: {new}"
    ~pp1:
      (Format.pp_print_option
         ~none:(fun fmt () -> Format.fprintf fmt "missing")
         (fun fmt -> Format.fprintf fmt "%a" pp_block_descriptor))
    ("prev", Data_encoding.option block_descriptor_encoding)
    ~pp2:pp_block_descriptor
    ("new", block_descriptor_encoding)

let fix_cementing_highwatermark =
  let pp_cemented_highwatermark =
    Format.pp_print_option
      ~none:(fun fmt () -> Format.fprintf fmt "None")
      (fun fmt -> Format.fprintf fmt "%ld")
  in
  declare_2
    ~section
    ~level:Notice
    ~name:"fix_cementing_highwatermark"
    ~msg:"updating cementing highwatermark (previously {prev}) with: {new}"
    ~pp1:pp_cemented_highwatermark
    ("prev", Data_encoding.(option int32))
    ~pp2:pp_cemented_highwatermark
    ("new", Data_encoding.(option int32))

let fix_checkpoint =
  declare_2
    ~section
    ~level:Notice
    ~name:"fix_checkpoint"
    ~msg:"updating checkpoint (previously {prev}) with: {new}"
    ~pp1:
      (Format.pp_print_option
         ~none:(fun fmt () -> Format.fprintf fmt "missing")
         (fun fmt -> Format.fprintf fmt "%a" pp_block_descriptor))
    ("prev", Data_encoding.option block_descriptor_encoding)
    ~pp2:pp_block_descriptor
    ("new", block_descriptor_encoding)

let fix_savepoint =
  declare_2
    ~section
    ~level:Notice
    ~name:"fix_savepoint"
    ~msg:
      "updating savepoint (previously {prev}) with the lowest block with \
       metadata found in the store: {new}"
    ~pp1:
      (Format.pp_print_option
         ~none:(fun fmt () -> Format.fprintf fmt "missing")
         (fun fmt -> Format.fprintf fmt "%a" pp_block_descriptor))
    ("prev", Data_encoding.option block_descriptor_encoding)
    ~pp2:pp_block_descriptor
    ("new", block_descriptor_encoding)

let fix_caboose =
  declare_2
    ~section
    ~level:Notice
    ~name:"fix_caboose"
    ~msg:
      "updating caboose (previously {prev}) with the lowest block found in the \
       store: {new}"
    ~pp1:
      (Format.pp_print_option
         ~none:(fun fmt () -> Format.fprintf fmt "missing")
         (fun fmt -> Format.fprintf fmt "%a" pp_block_descriptor))
    ("prev", Data_encoding.option block_descriptor_encoding)
    ~pp2:pp_block_descriptor
    ("new", block_descriptor_encoding)

let store_was_fixed =
  declare_0
    ~section
    ~level:Notice
    ~name:"store_was_fixed"
    ~msg:"the store was successfully fixed!"
    ()

let recover_merge =
  declare_0
    ~section
    ~level:Notice
    ~name:"recovering_merge"
    ~msg:"recovering from an interrupted store merge"
    ()

let restore_protocols_table =
  declare_0
    ~section
    ~level:Notice
    ~name:"restore_protocols_table"
    ~msg:"restoring protocols table"
    ()

let restore_protocol_activation =
  declare_2
    ~section
    ~level:Notice
    ~name:"restore_protocol_activation"
    ~msg:"protocol {protocol_level} ({protocol_hash}) was successfully restored"
    ("protocol_level", Data_encoding.int31)
    ~pp2:Protocol_hash.pp
    ("protocol_hash", Protocol_hash.encoding)

let update_protocol_table =
  declare_4
    ~section
    ~level:Notice
    ~name:"update_protocol_table"
    ~msg:
      "the protocol table was updated: protocol {proto_hash} (level \
       {proto_level}) was activated on block {block_hash} (level \
       {block_level})"
    ("proto_hash", Protocol_hash.encoding)
    ~pp1:Protocol_hash.pp_short
    ("proto_level", Data_encoding.int31)
    ("block_hash", Block_hash.encoding)
    ~pp3:Block_hash.pp
    ("block_level", Data_encoding.int32)
    ~pp4:pp_int32

let restore_history_mode =
  declare_1
    ~section
    ~level:Notice
    ~name:"restore_history_mode"
    ~msg:
      "history mode was successfully restored to {history_mode}, based on the \
       configuration file or command line argument"
    ("history_mode", History_mode.encoding)
    ~pp1:History_mode.pp

let restore_inferred_history_mode =
  declare_1
    ~section
    ~level:Notice
    ~name:"restore_inferred_history_mode"
    ~msg:
      "history mode was successfully restored to {history_mode}. Warning: this \
       history mode may differ from the one preceding the restore procedure \
       and you may need to restart the node to explicitly force the history \
       mode switch"
    ("history_mode", History_mode.encoding)
    ~pp1:History_mode.pp

(* Warning *)
let warning_missing_metadata =
  declare_2
    ~level:Warning
    ~section
    ~name:"missing_metadata"
    ~msg:
      "the storage is missing some metadata for cycle \
       {start_level}-{end_level}. Please consider restoring a consistent \
       storage"
    ("start_level", Data_encoding.int32)
    ("end_level", Data_encoding.int32)

(* Error *)
let merge_error =
  declare_3
    ~section
    ~level:Error
    ~name:"merge_error"
    ~msg:"merge from {start} to {end} failed: {message}"
    ~pp1:pp_int32
    ("start", Data_encoding.int32)
    ~pp2:pp_int32
    ("end", Data_encoding.int32)
    ~pp3:Format.pp_print_string
    ("message", Data_encoding.string)

let notify_merge_error =
  declare_1
    ~section
    ~level:Error
    ~name:"notify_merge_error"
    ~msg:
      "store merge has failed, restart the node to restore the consistency: \
       {errs}"
    ~pp1:(fun ppf -> Format.fprintf ppf "%a" Error_monad.pp_print_trace)
    ("errs", Error_monad.trace_encoding)

let upgrade_store_failed =
  declare_0
    ~section
    ~level:Error
    ~name:"upgrade_store_failed"
    ~msg:"store upgrade failed, cleaning up temporary files"
    ()

let upgrade_store_started =
  declare_0
    ~section
    ~level:Notice
    ~name:"upgrade_store_started"
    ~msg:"upgrading the store"
    ()
