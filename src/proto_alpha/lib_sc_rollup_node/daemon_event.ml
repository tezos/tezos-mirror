(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

  let section = [Protocol.name; "sc_rollup_node"; "daemon"]

  let head_processing =
    declare_2
      ~section
      ~name:"sc_rollup_daemon_process_head"
      ~msg:"Processing head {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let new_head_processed =
    declare_2
      ~section
      ~name:"sc_rollup_node_layer_1_new_head_processed"
      ~msg:"Finished processing layer 1 head {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let processing_heads_iteration =
    declare_3
      ~section
      ~name:"sc_rollup_daemon_processing_heads"
      ~msg:
        "A new iteration of process_heads has been triggered: processing \
         {number} heads from level {from} to level {to}"
      ~level:Notice
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let new_heads_processed =
    declare_3
      ~section
      ~name:"sc_rollup_node_layer_1_new_heads_processed"
      ~msg:
        "Finished processing {number} layer 1 heads for levels {from} to {to}"
      ~level:Notice
      ("number", Data_encoding.int31)
      ("from", Data_encoding.int32)
      ("to", Data_encoding.int32)

  let included_successful_operation =
    declare_1
      ~section
      ~name:"sc_rollup_daemon_included_successful_operation"
      ~msg:"Operation {operation} was included as successful"
      ~level:Debug
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp

  let included_failed_operation =
    declare_3
      ~section
      ~name:"sc_rollup_daemon_included_failed_operation"
      ~msg:"Operation {operation} was included as {status} with error {error}"
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
      ("error", Data_encoding.option Environment.Error_monad.trace_encoding)
      ~pp1:L1_operation.pp
      ~pp3:
        (fun ppf -> function
          | None -> Format.pp_print_string ppf "none"
          | Some e -> Environment.Error_monad.pp_trace ppf e)

  let error =
    declare_1
      ~section
      ~name:"sc_rollup_daemon_error"
      ~msg:"Fatal daemon error: {error}"
      ~level:Fatal
      ("error", trace_encoding)
      ~pp1:pp_print_trace

  let degraded_mode =
    declare_0
      ~section
      ~name:"sc_rollup_daemon_degraded_mode"
      ~msg:
        "Entering degraded mode: only playing refutation game to defend \
         commitments."
      ~level:Error
      ()
end

let head_processing hash level = Simple.(emit head_processing (hash, level))

let new_head_processed hash level =
  Simple.(emit new_head_processed (hash, level))

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

let new_heads_processed = new_heads_iteration Simple.new_heads_processed

let included_operation (type kind)
    (operation : kind Protocol.Alpha_context.manager_operation)
    (result : kind Protocol.Apply_results.manager_operation_result) =
  match L1_operation.of_manager_operation operation with
  | None -> Lwt.return_unit
  | Some operation -> (
      match result with
      | Applied _ -> Simple.(emit included_successful_operation) operation
      | result ->
          let status, errors =
            match result with
            | Applied _ -> assert false
            | Failed (_, e) -> (`Failed, Some e)
            | Backtracked (_, e) -> (`Backtracked, e)
            | Skipped _ -> (`Skipped, None)
          in
          Simple.(emit included_failed_operation) (operation, status, errors))

let error e = Simple.(emit error) e

let degraded_mode () = Simple.(emit degraded_mode) ()
