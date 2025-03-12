(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["evm_context"]

let ready =
  declare_0
    ~section
    ~name:"evm_context_is_ready"
    ~msg:"EVM context worker is ready"
    ~level:Info
    ()

let shutdown =
  declare_0
    ~section
    ~name:"evm_context_shutdown"
    ~msg:"EVM context worker is shutting down"
    ~level:Info
    ()

let reconstruct_replace_mainnet_kernel =
  declare_0
    ~section
    ~name:"reconstruct_replace_mainnet_kernel"
    ~msg:"replacing initial mainnet kernel"
    ~level:Info
    ()

let gc_split =
  declare_2
    ~section
    ~name:"evm_context_gc_split"
    ~msg:"splitting Irmin context at level {level} ({timestamp})"
    ~level:Info
    ~pp1:Ethereum_types.pp_quantity
    ~pp2:Time.Protocol.pp_hum
    ("level", Ethereum_types.quantity_encoding)
    ("timestamp", Time.Protocol.encoding)

let gc_started =
  declare_2
    ~section
    ~name:"evm_context_gc_started"
    ~msg:
      "garbage collection started for level {gc_level} at head level \
       {head_level}"
    ~level:Info
    ~pp1:Ethereum_types.pp_quantity
    ~pp2:Ethereum_types.pp_quantity
    ("gc_level", Ethereum_types.quantity_encoding)
    ("head_level", Ethereum_types.quantity_encoding)

let gc_finished =
  declare_3
    ~section
    ~name:"evm_context_gc_finished"
    ~msg:
      "garbage collection finished for level {gc_level} at head level \
       {head_level} (took {duration})"
    ~level:Info
    ~pp1:Ethereum_types.pp_quantity
    ~pp2:Ethereum_types.pp_quantity
    ~pp3:Ptime.Span.pp
    ("gc_level", Ethereum_types.quantity_encoding)
    ("head_level", Ethereum_types.quantity_encoding)
    ( "duration",
      Data_encoding.(
        conv
          Ptime.Span.to_float_s
          (fun s ->
            Ptime.Span.of_float_s s |> Option.value ~default:Ptime.Span.zero)
          float) )

let gc_waiter_failed =
  declare_1
    ~section
    ~name:"evm_context_gc_waiter_failed"
    ~level:Warning
    ~msg:"garbage collector waiter failed with an exception: {exn}"
    ("exn", Data_encoding.string)
    ~pp1:Format.pp_print_string

let unexpected_l1_block =
  declare_2
    ~section
    ~name:"evm_context_unexpected_l1_block"
    ~level:Warning
    ~msg:
      "received L1 block of level {provided_level} to process, but was \
       expected level {expected_level}"
    ("expected_level", Data_encoding.int32)
    ("provided_level", Data_encoding.int32)

let processed_l1_level =
  declare_2
    ~section
    ~name:"evm_context_processed_l1_level"
    ~level:Info
    ~msg:
      "processed L1 level {level}. Last finalized blueprint is: \
       {finalized_blueprint}"
    ("level", Data_encoding.int32)
    ("finalized_blueprint", Data_encoding.n)

let reset_impossible_missing_finalized_state =
  declare_0
    ~section
    ~name:"evm_context_reset_impossible_missing_finalized_state"
    ~level:Warning
    ~msg:"cannot found finalized state we must exit on divergence"
    ()

let missing_state =
  declare_1
    ~section
    ~name:"evm_context_missing_state"
    ~level:Warning
    ~msg:"cannot find state at level {level}"
    ~pp1:Ethereum_types.pp_quantity
    ("level", Ethereum_types.quantity_encoding)

let reset_at_level =
  declare_1
    ~section
    ~name:"evm_context_reset_at_level"
    ~level:Notice
    ~msg:"resetting to finalized block {level}"
    ~pp1:Ethereum_types.pp_quantity
    ("level", Ethereum_types.quantity_encoding)

let worker_request_failed =
  declare_2
    ~section
    ~name:"evm_context_request_failed"
    ~msg:"request {view} failed: {errors}"
    ~level:Error
    ("view", Evm_context_types.Request.encoding)
    ~pp1:Evm_context_types.Request.pp
    ("errors", Events.trace_encoding)
    ~pp2:Error_monad.pp_print_trace

let observer_potential_reorg =
  declare_1
    ~section
    ~name:"evm_context_observer_potential_reorg"
    ~level:Warning
    ~msg:"potential reorganization happening at level {level}"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let observer_reorg_old_blueprint =
  declare_1
    ~section
    ~name:"evm_context_observer_reorg_old_blueprint"
    ~level:Warning
    ~msg:"EVM endpoint provided an old known blueprint (level {level})"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let observer_reorg_cannot_decode_blueprint =
  declare_1
    ~section
    ~name:"evm_context_observer_reorg_cannot_decode_blueprint"
    ~level:Warning
    ~msg:"EVM endpoint provided a blueprint (level {level}) we cannot decode"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let observer_reorg_cannot_find_divergence =
  declare_2
    ~section
    ~name:"evm_context_observer_reorg_cannot_find_divergence"
    ~level:Warning
    ~msg:
      "potential blueprint of reorg is at level {level1}, cannot find block \
       {level2} locally"
    ~pp1:Z.pp_print
    ~pp2:Z.pp_print
    ("level1", Data_encoding.n)
    ("level2", Data_encoding.n)

let observer_reorg_cannot_find_state =
  declare_1
    ~section
    ~name:"evm_context_observer_reorg_cannot_find_state"
    ~level:Warning
    ~msg:"reorganization needs state at level {level} but checkpoint is missing"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let get_block_failed =
  declare_2
    ~section
    ~name:"evm_context_get_block_failed"
    ~level:Error
    ~msg:"get block by number failed for level {level}{trace}"
    ~pp1:Ethereum_types.pp_quantity
    ~pp2:pp_print_trace
    ("level", Ethereum_types.quantity_encoding)
    ("trace", Events.trace_encoding)

let start_history_mode =
  declare_1
    ~section
    ~name:"evm_context_start_history_mode"
    ~level:Notice
    ~msg:"running with history mode {mode}"
    ("mode", Configuration.history_mode_encoding)

let switching_history_mode =
  declare_2
    ~section
    ~name:"evm_context_switch_history_mode"
    ~level:Warning
    ~msg:"switching history mode from {from} to {to_}"
    ("from", Configuration.history_mode_encoding)
    ("to_", Configuration.history_mode_encoding)

let ready () = emit ready ()

let shutdown () = emit shutdown ()

let reconstruct_replace_mainnet_kernel () =
  emit reconstruct_replace_mainnet_kernel ()

let gc_split level timestamp = emit gc_split (level, timestamp)

let gc_started ~gc_level ~head_level = emit gc_started (gc_level, head_level)

let gc_finished ~gc_level ~head_level duration =
  emit gc_finished (gc_level, head_level, duration)

let gc_waiter_failed exn =
  emit__dont_wait__use_with_care gc_waiter_failed (Printexc.to_string exn)

let unexpected_l1_block ~expected_level ~provided_level =
  emit unexpected_l1_block (expected_level, provided_level)

let processed_l1_level (level, finalized_blueprint) =
  emit processed_l1_level (level, finalized_blueprint)

let reset_impossible_missing_finalized_state =
  emit reset_impossible_missing_finalized_state

let missing_state level = emit missing_state level

let reset_at_level level = emit reset_at_level level

let worker_request_failed request_view errs =
  emit worker_request_failed (request_view, errs)

let observer_potential_reorg Ethereum_types.(Qty level) =
  emit observer_potential_reorg level

let observer_reorg_old_blueprint Ethereum_types.(Qty level) =
  emit observer_reorg_old_blueprint level

let observer_reorg_cannot_decode_blueprint Ethereum_types.(Qty level) =
  emit observer_reorg_cannot_decode_blueprint level

let observer_reorg_cannot_find_divergence Ethereum_types.(Qty level) =
  emit observer_reorg_cannot_find_divergence (level, Z.pred level)

let observer_reorg_cannot_find_state Ethereum_types.(Qty level) =
  emit observer_reorg_cannot_find_state level

let get_block_failed n err = emit get_block_failed (n, err)

let start_history_mode history_mode = emit start_history_mode history_mode

let switching_history_mode ~from ~to_ = emit switching_history_mode (from, to_)
