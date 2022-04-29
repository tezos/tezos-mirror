(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Injector_worker_types

module Make (Rollup : Injector_sigs.PARAMETERS) = struct
  module Tags = Injector_tags.Make (Rollup.Tag)
  include Internal_event.Simple

  let section = Rollup.events_section

  let declare_1 ~name ~msg ~level ?pp1 enc1 =
    declare_3
      ~section
      ~name
      ~msg:("[{signer}: {tags}] " ^ msg)
      ~level
      ("signer", Signature.Public_key_hash.encoding)
      ("tags", Tags.encoding)
      enc1
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp2:Tags.pp
      ?pp3:pp1

  let declare_2 ~name ~msg ~level ?pp1 ?pp2 enc1 enc2 =
    declare_4
      ~section
      ~name
      ~msg:("[{signer}: {tags}] " ^ msg)
      ~level
      ("signer", Signature.Public_key_hash.encoding)
      ("tags", Tags.encoding)
      enc1
      enc2
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp2:Tags.pp
      ?pp3:pp1
      ?pp4:pp2

  let declare_3 ~name ~msg ~level ?pp1 ?pp2 ?pp3 enc1 enc2 enc3 =
    declare_5
      ~section
      ~name
      ~msg:("[{signer}: {tags}] " ^ msg)
      ~level
      ("signer", Signature.Public_key_hash.encoding)
      ("tags", Tags.encoding)
      enc1
      enc2
      enc3
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp2:Tags.pp
      ?pp3:pp1
      ?pp4:pp2
      ?pp5:pp3

  let request_failed =
    declare_3
      ~name:"request_failed"
      ~msg:"request {view} failed ({worker_status}): {errors}"
      ~level:Warning
      ("view", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let request_completed_notice =
    declare_2
      ~name:"request_completed_notice"
      ~msg:"{view} {worker_status}"
      ~level:Notice
      ("view", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status

  let request_completed_debug =
    declare_2
      ~name:"request_completed_debug"
      ~msg:"{view} {worker_status}"
      ~level:Debug
      ("view", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status

  let new_tezos_head =
    declare_1
      ~name:"new_tezos_head"
      ~msg:"processing new Tezos head {head}"
      ~level:Debug
      ("head", Block_hash.encoding)

  let injecting_pending =
    declare_1
      ~name:"injecting_pending"
      ~msg:"Injecting {count} pending operations"
      ~level:Notice
      ("count", Data_encoding.int31)

  let pp_operations_list ppf operations =
    Format.fprintf
      ppf
      "@[%a@]"
      (Format.pp_print_list L1_operation.pp)
      operations

  let pp_operations_hash_list ppf operations =
    Format.fprintf
      ppf
      "@[%a@]"
      (Format.pp_print_list L1_operation.Hash.pp)
      operations

  let injecting_operations =
    declare_1
      ~name:"injecting_operations"
      ~msg:"Injecting operations: {operations}"
      ~level:Notice
      ("operations", Data_encoding.list L1_operation.encoding)
      ~pp1:pp_operations_list

  let simulating_operations =
    declare_2
      ~name:"simulating_operations"
      ~msg:"Simulating operations (force = {force}): {operations}"
      ~level:Debug
      ("operations", Data_encoding.list L1_operation.encoding)
      ("force", Data_encoding.bool)
      ~pp1:pp_operations_list

  let dropping_operation =
    declare_2
      ~name:"dropping_operation"
      ~msg:"Dropping operation {operation} failing with {error}"
      ~level:Notice
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp
      ("error", Environment.Error_monad.trace_encoding)
      ~pp2:Environment.Error_monad.pp_trace

  let injected =
    declare_1
      ~name:"injected"
      ~msg:"Injected in {oph}"
      ~level:Notice
      ("oph", Operation_hash.encoding)

  let add_pending =
    declare_1
      ~name:"add_pending"
      ~msg:"Add {operation} to pending"
      ~level:Notice
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp

  let included =
    declare_3
      ~name:"included"
      ~msg:"Included operations of {block} at level {level}: {operations}"
      ~level:Notice
      ("block", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp3:pp_operations_hash_list

  let revert_operations =
    declare_1
      ~name:"revert_operations"
      ~msg:"Reverting operations: {operations}"
      ~level:Notice
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp1:pp_operations_hash_list

  let confirmed_level =
    declare_1
      ~name:"confirmed_level"
      ~msg:"Confirmed Tezos level {level}"
      ~level:Notice
      ("level", Data_encoding.int32)

  let confirmed_operations =
    declare_2
      ~name:"confirmed_operations"
      ~msg:"Confirmed operations of level {level}: {operations}"
      ~level:Notice
      ("level", Data_encoding.int32)
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp2:pp_operations_hash_list
end
