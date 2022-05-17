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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2880
   Add corresponding .mli file. *)

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
