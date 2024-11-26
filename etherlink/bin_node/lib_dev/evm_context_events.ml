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
    ~msg:"EVM Context worker is ready"
    ~level:Info
    ()

let shutdown =
  declare_0
    ~section
    ~name:"evm_context_shutdown"
    ~msg:"EVM Context worker is shutting down"
    ~level:Info
    ()

let reconstruct_replace_mainnet_kernel =
  declare_0
    ~section
    ~name:"reconstruct_replace_mainnet_kernel"
    ~msg:"Replacing initial mainnet kernel"
    ~level:Info
    ()

let gc_split =
  declare_2
    ~section
    ~name:"evm_context_gc_split"
    ~msg:"Splitting Irmin context at level {level} ({timestamp})"
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
      "Garbage collection started for level {gc_level} at head level \
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
      "Garbage collection finished for level {gc_level} at head level \
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
    ~msg:"[Warning] Garbage collector waiter failed with an exception:"
    ("exn", Data_encoding.string)

let unexpected_l1_block =
  declare_2
    ~section
    ~name:"evm_context_unexpected_l1_block"
    ~level:Warning
    ~msg:
      "[Warning] Apply EVM events got a block for level {provided_level} but \
       is older than expected level {expected_level}."
    ("expected_level", Data_encoding.int32)
    ("provided_level", Data_encoding.int32)

let processed_l1_level =
  declare_1
    ~section
    ~name:"evm_context_processed_l1_level"
    ~level:Info
    ~msg:"Processed L1 level {level}"
    ("level", Data_encoding.int32)

let reset_impossible_missing_finalized_state =
  declare_0
    ~section
    ~name:"evm_context_reset_impossible_missing_finalized_state"
    ~level:Warning
    ~msg:"[Warning] Cannot found finalized state we must exit on divergence"
    ()

let reset_incoherent_finalized_state =
  declare_2
    ~section
    ~name:"evm_context_reset_incoherent_finalized_state"
    ~level:Warning
    ~msg:
      "[Warning] Finalized state was expected on level \
       {expected_finalized_number} but found {finalized_number}"
    ~pp1:Ethereum_types.pp_quantity
    ~pp2:Ethereum_types.pp_quantity
    ("expected_finalized_number", Ethereum_types.quantity_encoding)
    ("finalized_number", Ethereum_types.quantity_encoding)

let reset_at_level =
  declare_1
    ~section
    ~name:"evm_context_reset_at_level"
    ~level:Notice
    ~msg:"[Warning] Resetting to finalized block {level}"
    ~pp1:Ethereum_types.pp_quantity
    ("level", Ethereum_types.quantity_encoding)

let worker_request_failed =
  declare_2
    ~section
    ~name:"evm_context_request_failed"
    ~msg:"[Warning]: Request {view} failed: {errors}"
    ~level:Error
    ("view", Evm_context_types.Request.encoding)
    ~pp1:Evm_context_types.Request.pp
    ("errors", Error_monad.trace_encoding)
    ~pp2:Error_monad.pp_print_trace

let observer_potential_reorg =
  declare_1
    ~section
    ~name:"evm_context_observer_potential_reorg"
    ~level:Warning
    ~msg:"[Warning] Potential reorganization happening at level {level}"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let observer_reorg_old_blueprint =
  declare_1
    ~section
    ~name:"evm_context_observer_reorg_old_blueprint"
    ~level:Warning
    ~msg:
      "[Warning] EVM Endpoint provided an old known blueprint (level {level})"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

let observer_reorg_cannot_decode_blueprint =
  declare_1
    ~section
    ~name:"evm_context_observer_reorg_cannot_decode_blueprint"
    ~level:Warning
    ~msg:
      "[Warning] EVM Endpoint provided a blueprint (level {level}) we cannot \
       decode"
    ~pp1:Z.pp_print
    ("level", Data_encoding.n)

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

let processed_l1_level level = emit processed_l1_level level

let reset_impossible_missing_finalized_state =
  emit reset_impossible_missing_finalized_state

let reset_incoherent_finalized_state ~expected_finalized_number
    ~finalized_number =
  emit
    reset_incoherent_finalized_state
    (expected_finalized_number, finalized_number)

let reset_at_level level = emit reset_at_level level

let worker_request_failed request_view errs =
  emit worker_request_failed (request_view, errs)

let observer_potential_reorg Ethereum_types.(Qty level) =
  emit observer_potential_reorg level

let observer_reorg_old_blueprint Ethereum_types.(Qty level) =
  emit observer_reorg_old_blueprint level

let observer_reorg_cannot_decode_blueprint Ethereum_types.(Qty level) =
  emit observer_reorg_cannot_decode_blueprint level
