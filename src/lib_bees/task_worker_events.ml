(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["bees"; "task_worker"]

let shutdown_enter =
  declare_0
    ~section
    ~name:"task_worker_shutdown_enter"
    ~msg:"task worker shutdown: enter"
    ~level:Debug
    ()

let shutdown_exit =
  declare_0
    ~section
    ~name:"task_worker_shutdown_exit"
    ~msg:"task worker shutdown: exit"
    ~level:Debug
    ()

let worker_launch_failed =
  declare_2
    ~section
    ~name:"task_worker_launch_failed"
    ~msg:"worker launch failed with {error} (domains={domains})"
    ~level:Error
    ("error", Data_encoding.string)
    ("domains", Data_encoding.int31)

let worker_retrying =
  declare_1
    ~section
    ~name:"task_worker_retrying"
    ~msg:"retrying with {domains} domains"
    ~level:Info
    ("domains", Data_encoding.int31)

let worker_launch_degraded =
  declare_2
    ~section
    ~name:"task_worker_launch_degraded"
    ~msg:
      "worker launch succeeded with reduced capacity: requested {requested} \
       domains, running with {actual} domains"
    ~level:Warning
    ("requested", Data_encoding.int31)
    ("actual", Data_encoding.int31)

let task_launch_failed =
  declare_1
    ~section
    ~name:"task_worker_enqueue_failed"
    ~msg:"failed to enqueue task: {error}"
    ~level:Error
    ("error", Data_encoding.string)

let promise_double_resolution_detected =
  declare_2
    ~section
    ~name:"task_worker_promise_double_resolution"
    ~msg:
      "promise double resolution while launching worker: {error}\n{backtrace}"
    ~level:Error
    ("error", Data_encoding.string)
    ("backtrace", Data_encoding.string)
