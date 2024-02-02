(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["evm_node"; "prod"]

let received_upgrade =
  declare_1
    ~section
    ~name:"received_upgrade"
    ~msg:"Received an upgrade payload: {payload}"
    ~level:Notice
    ("payload", Data_encoding.string)

let ignored_kernel_arg =
  declare_0
    ~section
    ~name:"ignored_kernel_arg"
    ~msg:
      "Ignored the kernel command-line argument since the EVM state was \
       already initialized"
    ~level:Warning
    ()

let received_upgrade payload = emit received_upgrade payload

let ignored_kernel_arg () = emit ignored_kernel_arg ()
