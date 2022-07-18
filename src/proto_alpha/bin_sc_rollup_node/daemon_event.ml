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

  let section = ["sc_rollup_node"; "daemon"]

  let head_processing =
    declare_4
      ~section
      ~name:"sc_rollup_daemon_process_head"
      ~msg:
        "Processing: head {hash} at level {level}: finalized? {finalized}, \
         partially processed? {seen_before}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("finalized", Data_encoding.bool)
      ("seen_before", Data_encoding.bool)

  let not_finalized_head =
    declare_2
      ~section
      ~name:"sc_rollup_daemon_not_finalized"
      ~msg:
        "The following head has only be partially processed - commitments have \
         not been computed: Head {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let processing_heads_iteration =
    declare_2
      ~section
      ~name:"sc_rollup_daemon_processing_heads"
      ~msg:
        "A new iteration of process_heads has been triggered: processing heads \
         from level {from} to level {to}"
      ~level:Notice
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

  let finalized_successful_operation =
    declare_1
      ~section
      ~name:"sc_rollup_daemon_finalized_successful_operation"
      ~msg:"Operation {operation} was finalized"
      ~level:Debug
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp
end

let head_processing hash level finalized seen_before =
  Simple.(emit head_processing (hash, level, finalized, seen_before))

let not_finalized_head hash level =
  Simple.(emit not_finalized_head (hash, level))

let processing_heads_iteration old_heads new_heads =
  let maybe_level = Option.map (fun (Layer1.Head {level; _}) -> level) in
  let from_level =
    match maybe_level @@ List.hd old_heads with
    | None -> maybe_level @@ List.hd new_heads
    | Some level -> Some level
  in
  let to_level =
    match maybe_level @@ List.last_opt new_heads with
    | None -> maybe_level @@ List.hd old_heads
    | Some level -> Some level
  in
  match (from_level, to_level) with
  | Some from_level, Some to_level ->
      Simple.(emit processing_heads_iteration (from_level, to_level))
  | _ -> Lwt.return_unit

let included_operation (type kind) ~finalized
    (operation : kind Protocol.Alpha_context.manager_operation)
    (result : kind Protocol.Apply_results.manager_operation_result) =
  let operation = L1_operation.make operation in
  match result with
  | Applied _ when finalized ->
      Simple.(emit finalized_successful_operation) operation
  | _ when finalized ->
      (* No events for finalized non successful operations  *)
      Lwt.return_unit
  | Applied _ -> Simple.(emit included_successful_operation) operation
  | result ->
      let status, errors =
        match result with
        | Applied _ -> assert false
        | Failed (_, e) -> (`Failed, Some e)
        | Backtracked (_, e) -> (`Backtracked, e)
        | Skipped _ -> (`Skipped, None)
      in
      Simple.(emit included_failed_operation) (operation, status, errors)
