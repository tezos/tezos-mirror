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

open Publisher_worker_types

module Simple = struct
  include Internal_event.Simple

  let section = ["smart_rollup_node"; "commitment"]

  let starting =
    declare_0
      ~section
      ~name:"smart_rollup_node_commitment_publisher_starting"
      ~msg:"Starting commitment publisher for the smart rollup node"
      ~level:Info
      ()

  let stopping =
    declare_0
      ~section
      ~name:"smart_rollup_node_commitment_publisher_stopping"
      ~msg:"Stopping commitment publisher for the smart rollup node"
      ~level:Info
      ()

  let last_cemented_commitment_updated =
    declare_2
      ~section
      ~name:"smart_rollup_node_commitment_lcc_updated"
      ~msg:
        "Last cemented commitment was updated to hash {hash} at inbox level \
         {level}"
      ~level:Debug
      ("hash", Commitment.Hash.encoding)
      ("level", Data_encoding.int32)

  let last_published_commitment_updated =
    declare_2
      ~section
      ~name:"smart_rollup_node_commitment_lpc_updated"
      ~msg:
        "Last published commitment was updated to hash {hash} at inbox level \
         {level}"
      ~level:Debug
      ("hash", Commitment.Hash.encoding)
      ("level", Data_encoding.int32)

  let compute_commitment =
    declare_1
      ~section
      ~name:"smart_rollup_node_commitment_compute"
      ~msg:"Computing and storing new commitment for level {level}"
      ~level:Info
      ("level", Data_encoding.int32)

  let new_commitment =
    declare_2
      ~section
      ~name:"smart_rollup_node_new_commitment"
      ~msg:"New commitment {hash} for inbox level {level}"
      ~level:Notice
      ("hash", Commitment.Hash.encoding)
      ("level", Data_encoding.int32)

  let publish_commitment =
    declare_2
      ~section
      ~name:"smart_rollup_node_commitment_publish_commitment"
      ~msg:"Publishing commitment {hash} for inbox level {level}"
      ~level:Info
      ("hash", Commitment.Hash.encoding)
      ("level", Data_encoding.int32)

  let recover_bond =
    declare_1
      ~section
      ~name:"smart_rollup_node_recover_bond"
      ~msg:"Recover bond for {staker}"
      ~level:Info
      ("staker", Signature.Public_key_hash.encoding)

  let publish_execute_whitelist_update =
    declare_3
      ~section
      ~name:"smart_rollup_node_publish_execute_whitelist_update"
      ~msg:
        "Publishing execute whitelist update for cemented commitment {hash}, \
         outbox level {outbox_level} and index {message_index}"
      ~level:Info
      ("hash", Commitment.Hash.encoding)
      ("outbox_level", Data_encoding.int32)
      ("message_index", Data_encoding.int31)

  module Publisher = struct
    let section = section @ ["publisher"]

    let request_failed =
      declare_3
        ~section
        ~name:"request_failed"
        ~msg:"[Warning] Request {view} failed ({worker_status}): {errors}"
        ~level:Warning
        ("view", Request.encoding)
        ~pp1:Request.pp
        ("worker_status", Worker_types.request_status_encoding)
        ~pp2:Worker_types.pp_status
        ("errors", Error_monad.trace_encoding)
        ~pp3:Error_monad.pp_print_trace

    let request_completed =
      declare_2
        ~section
        ~name:"request_completed"
        ~msg:"{view} {worker_status}"
        ~level:Debug
        ("view", Request.encoding)
        ("worker_status", Worker_types.request_status_encoding)
        ~pp1:Request.pp
        ~pp2:Worker_types.pp_status
  end
end

let starting = Simple.(emit starting)

let stopping = Simple.(emit stopping)

let section = Simple.section

let last_cemented_commitment_updated head level =
  Simple.(emit last_cemented_commitment_updated (head, level))

let last_published_commitment_updated head level =
  Simple.(emit last_published_commitment_updated (head, level))

let compute_commitment level = Simple.(emit compute_commitment level)

let new_commitment head level = Simple.(emit new_commitment (head, level))

let publish_commitment head level =
  Simple.(emit publish_commitment (head, level))

let recover_bond staker = Simple.(emit recover_bond staker)

let publish_execute_whitelist_update hash level index =
  Simple.(emit publish_execute_whitelist_update (hash, level, index))

module Publisher = struct
  let request_failed view worker_status errors =
    Simple.(emit Publisher.request_failed (view, worker_status, errors))

  let request_completed view worker_status =
    Simple.(emit Publisher.request_completed (view, worker_status))
end
