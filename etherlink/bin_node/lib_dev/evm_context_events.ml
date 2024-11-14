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
