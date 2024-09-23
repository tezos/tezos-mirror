(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Simple = struct
  include Internal_event.Simple

  let section = ["etherlink_governance_observer"]

  let starting_observer =
    declare_0
      ~section
      ~level:Notice
      ~name:"starting_etherlink_governance_observer"
      ~msg:"Starting Etherlink's governance observer"
      ()

  let starting_metrics_server =
    declare_0
      ~section
      ~level:Notice
      ~name:"starting_metrics_server"
      ~msg:"Starting Prometheus' metrics server"
      ()

  let monitor_head_restart =
    declare_0
      ~section
      ~level:Notice
      ~name:"monitor_head_restart"
      ~msg:"Head monitoring process exited, trying to restart it"
      ()

  let contract_metrics =
    declare_1
      ~section
      ~level:Debug
      ~name:"contract_metrics"
      ~pp1:Format.pp_print_string
      ~msg:"Contract metric(s) was/were set: {metrics}"
      ("metrics", Data_encoding.string)

  let storage_state_error =
    declare_1
      ~section
      ~level:Error
      ~name:"storage_state_error"
      ~pp1:Format.pp_print_string
      ~msg:"Error while processing storage state: {error_msg}"
      ("error_msg", Data_encoding.string)

  let contract_operations_error =
    declare_1
      ~section
      ~level:Error
      ~name:"contract_operations_error"
      ~pp1:Format.pp_print_string
      ~msg:"Error while processing contract operations: {error_msg}"
      ("error_msg", Data_encoding.string)
end

let starting_observer = Simple.(emit starting_observer)

let starting_metrics_server = Simple.(emit starting_metrics_server)

let monitor_head_restart = Simple.(emit monitor_head_restart)

let contract_metrics metrics = Simple.(emit contract_metrics metrics)

let storage_state_error error_msg = Simple.(emit storage_state_error error_msg)

let contract_operations_error error_msg =
  Simple.(emit contract_operations_error error_msg)
