(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["tracer"]

let read_line =
  declare_2
    ~section
    ~name:"call_tracer_read_line"
    ~msg:"Call tracer read a line {line} of tx {tx}"
    ~level:Debug
    ("line", Data_encoding.n)
    ("tx", Data_encoding.string)

let tracer_input =
  declare_1
    ~section
    ~name:"tracer_input"
    ~msg:"Tracer with input {inputs} "
    ~level:Debug
    ("inputs", Data_encoding.string)

let read_line = emit read_line

let tracer_input config = emit tracer_input config
