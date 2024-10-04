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

let ready () = emit ready ()

let shutdown () = emit shutdown ()

let reconstruct_replace_mainnet_kernel () =
  emit reconstruct_replace_mainnet_kernel ()

let gc_split level timestamp = emit gc_split (level, timestamp)
