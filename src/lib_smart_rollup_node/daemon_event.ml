(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module Simple = struct
  include Internal_event.Simple

  let section = ["smart_rollup_node"; "daemon"]

  let head_processing =
    declare_2
      ~section
      ~name:"smart_rollup_node_daemon_process_head"
      ~msg:"Processing head {hash} at level {level}"
      ~pp1:Block_hash.pp_short
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let new_head_processed =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_new_head_processed"
      ~msg:"Processed head {hash} at level {level} in {process_time}"
      ~level:Notice
      ~pp1:Block_hash.pp_short
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("process_time", Time.System.Span.encoding)
      ~pp3:Ptime.Span.pp

  let new_head_degraded =
    declare_2
      ~section
      ~name:"smart_rollup_node_daemon_new_head_degraded"
      ~msg:"[DEGRADED MODE] Seen head {hash} at level {level}"
      ~level:Error
      ~pp1:Block_hash.pp_short
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let reorg =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_reorg"
      ~msg:"Reorg of {number} blocks from level {from} to level {to}"
      ~level:Notice
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let catch_up =
    declare_1
      ~section
      ~name:"smart_rollup_node_daemon_catch_up"
      ~msg:"Catching up on {levels} blocks"
      ~level:Notice
      ("levels", Data_encoding.int32)

  let processing_heads_iteration =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_processing_heads"
      ~msg:
        "A new iteration of process_heads has been triggered: processing \
         {number} heads from level {from} to level {to}"
      ~level:Debug
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let new_heads_processed =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_new_heads_processed"
      ~msg:"Processed {number} heads for levels {from} to {to}"
      ~level:Info
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let new_heads_side_process_finished =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_loop_process_finished"
      ~msg:"Main loop processed {number} heads up to level {to}"
      ~level:Info
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let included_successful_operation =
    declare_1
      ~section
      ~name:"smart_rollup_node_daemon_included_successful_operation"
      ~msg:"Operation {operation} was included as successful"
      ~level:Debug
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp

  let included_failed_operation =
    declare_3
      ~section
      ~name:"smart_rollup_node_daemon_included_failed_operation"
      ~msg:
        "[Warning]: Operation {operation} was included as {status} with error \
         {error}"
      ~level:Warning
      ("operation", L1_operation.encoding)
      ( "status",
        Data_encoding.(
          string_enum
            [
              ("failed", `Failed);
              ("backtracked", `Backtracked);
              ("skipped", `Skipped);
            ]) )
      ("error", Data_encoding.option Error_monad.trace_encoding)
      ~pp1:L1_operation.pp
      ~pp3:(fun ppf -> function
        | None -> Format.pp_print_string ppf "none"
        | Some e -> Error_monad.pp_print_trace ppf e)

  let migration =
    declare_5
      ~name:"smart_rollup_node_daemon_protocol_migration"
      ~msg:
        "{catching_up} from {old_protocol} ({old_protocol_level}) to \
         {new_protocol} ({new_protocol_level}) "
      ~level:Notice
      ("catching_up", Data_encoding.bool)
      ("old_protocol", Protocol_hash.encoding)
      ("old_protocol_level", Data_encoding.int31)
      ("new_protocol", Protocol_hash.encoding)
      ("new_protocol_level", Data_encoding.int31)
      ~pp1:(fun ppf catching_up ->
        Format.pp_print_string
          ppf
          (if catching_up then "Catching up on migration" else "Migration"))

  let switched_protocol =
    declare_3
      ~name:"smart_rollup_node_daemon_switched_protocol"
      ~msg:"Switched to {protocol} ({proto_level}) with constants {constants} "
      ~level:Notice
      ("protocol", Protocol_hash.encoding)
      ("proto_level", Data_encoding.int31)
      ("constants", Rollup_constants.encoding)
      ~pp3:(fun fmt c ->
        Data_encoding.Json.pp
          fmt
          (Data_encoding.Json.construct Rollup_constants.encoding c))

  let error =
    declare_1
      ~section
      ~name:"smart_rollup_node_daemon_error"
      ~msg:"[Fatal daemon error]: {error}"
      ~level:Fatal
      ("error", trace_encoding)
      ~pp1:pp_print_trace

  let degraded_mode =
    declare_0
      ~section
      ~name:"smart_rollup_node_daemon_degraded_mode"
      ~msg:
        "[Daemon error]: entering degraded mode - only playing refutation game \
         to defend commitments and publishing pending commitments"
      ~level:Error
      ()

  let refutation_loop_retry =
    declare_1
      ~section
      ~name:"smart_rollup_node_daemon_refutation_loop_retry"
      ~msg:"[Refutation daemon error]: restarting refutation daemon in {delay}."
      ~level:Warning
      ("delay", Time.System.Span.encoding)
      ~pp1:Ptime.Span.pp

  let main_loop_retry =
    declare_1
      ~section
      ~name:"smart_rollup_node_daemon_main_loop_retry"
      ~msg:"Restarting main rollup node loop in {delay}."
      ~level:Warning
      ("delay", Time.System.Span.encoding)
      ~pp1:Ptime.Span.pp

  let exit_bailout_mode =
    declare_0
      ~section
      ~name:"smart_rollup_node_daemon_exit_bailout_mode"
      ~msg:
        "Exit bailout mode - stakes have been recovered, and the node is \
         exiting safely now"
      ~level:Notice
      ()
end

let head_processing hash level = Simple.(emit head_processing (hash, level))

let new_head_processed hash level process_time =
  Simple.(emit new_head_processed (hash, level, process_time))

let new_head_degraded hash level = Simple.(emit new_head_degraded (hash, level))

let new_heads_iteration event = function
  | oldest :: rest ->
      let newest =
        match List.rev rest with [] -> oldest | newest :: _ -> newest
      in
      let number =
        Int32.sub newest.Layer1.level oldest.Layer1.level
        |> Int32.succ |> Int32.to_int
      in
      Simple.emit event (number, oldest.level, newest.level)
  | [] -> Lwt.return_unit

let processing_heads_iteration =
  new_heads_iteration Simple.processing_heads_iteration

let reorg = function
  | [] -> Lwt.return_unit
  | blocks -> new_heads_iteration Simple.reorg blocks

let new_heads_processed = new_heads_iteration Simple.new_heads_processed

let new_heads_side_process_finished =
  new_heads_iteration Simple.new_heads_side_process_finished

let catch_up levels = Simple.(emit catch_up) levels

let included_operation ?errors status operation =
  match status with
  | `Applied -> Simple.(emit included_successful_operation) operation
  | `Failed | `Backtracked | `Skipped ->
      Simple.(emit included_failed_operation) (operation, status, errors)

let migration ~catching_up (old_protocol, old_protocol_level)
    (new_protocol, new_protocol_level) =
  Simple.(emit migration)
    ( catching_up,
      old_protocol,
      old_protocol_level,
      new_protocol,
      new_protocol_level )

let switched_protocol protocol proto_level constants =
  Simple.(emit switched_protocol) (protocol, proto_level, constants)

let error e = Simple.(emit error) e

let degraded_mode () = Simple.(emit degraded_mode) ()

let refutation_loop_retry d =
  Simple.(emit refutation_loop_retry) (Time.System.Span.of_seconds_exn d)

let main_loop_retry d =
  Simple.(emit main_loop_retry) (Time.System.Span.of_seconds_exn d)

let exit_bailout_mode () = Simple.(emit exit_bailout_mode) ()
