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
open Injector_sigs

module Make
    (Parameters : PARAMETERS)
    (Tags : module type of Injector_tags.Make (Parameters.Tag))
    (Operation : PARAM_OPERATION)
    (Inj_operation : INJECTOR_OPERATION with type operation = Operation.t)
    (Request : module type of Request (Inj_operation)) =
    struct
      include Internal_event.Simple

      let section = Parameters.events_section @ ["injector"]

      let monitoring_error =
        declare_1
          ~section
          ~name:"monitoring_error"
          ~msg:"error (ignored) in monitoring: {error}"
          ~level:Warning
          ("error", trace_encoding)

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

      let cannot_compute_reorg =
        declare_1
          ~name:"cannot_compute_reorg"
          ~msg:"Cannot compute reorg for new block {head}"
          ~level:Warning
          ("head", Block_hash.encoding)

      let injecting_pending =
        declare_1
          ~name:"injecting_pending"
          ~msg:"injecting {count} pending operations"
          ~level:Notice
          ("count", Data_encoding.int31)

      let pp_operations_list ppf operations =
        Format.fprintf
          ppf
          "@[%a@]"
          (Format.pp_print_list Operation.pp)
          operations

      let pp_operations_hash_list ppf operations =
        Format.fprintf
          ppf
          "@[%a@]"
          (Format.pp_print_list Inj_operation.Hash.pp)
          operations

      let number_of_operations_in_queue =
        declare_1
          ~name:"number_of_operations_in_queue"
          ~msg:
            "injector's queue: there is currently {number_of_operations} \
             operations waiting to be injected"
          ~level:Info
          ("number_of_operations", Data_encoding.int31)

      let considered_operations_info =
        declare_1
          ~name:"considered_operations_info"
          ~msg:
            "injector's queue: the following operations are being considered \
             for injection {operations}"
          ~level:Debug
          ("operations", Data_encoding.list Operation.encoding)
          ~pp1:pp_operations_list

      let dropped_operations =
        declare_1
          ~name:"dropped_operations"
          ~msg:
            "dropping operations: the following operations are dropped \
             {operations}"
          ~level:Debug
          ("operations", Data_encoding.list Operation.encoding)
          ~pp1:pp_operations_list

      let simulating_operations =
        declare_2
          ~name:"simulating_operations"
          ~msg:"simulating operations (force = {force}): {operations}"
          ~level:Debug
          ("operations", Data_encoding.list Operation.encoding)
          ("force", Data_encoding.bool)
          ~pp1:pp_operations_list

      let discard_error_operation =
        declare_3
          ~name:"discard_error_operation"
          ~msg:
            "discarding operation {operation} failing {count} times with \
             {error}"
          ~level:Notice
          ("operation", Operation.encoding)
          ~pp1:Operation.pp
          ("count", Data_encoding.int31)
          ("error", Data_encoding.option Error_monad.trace_encoding)
          ~pp3:(fun ppf -> Option.iter (Error_monad.pp_print_trace ppf))

      let injected =
        declare_2
          ~name:"injected"
          ~msg:"injected {nb} operations in {oph}"
          ~level:Notice
          ("nb", Data_encoding.int31)
          ("oph", Operation_hash.encoding)

      let add_pending =
        declare_1
          ~name:"add_pending"
          ~msg:"add {operation} to pending"
          ~level:Notice
          ("operation", Operation.encoding)
          ~pp1:Operation.pp

      let retry_operation =
        declare_1
          ~name:"retry_operation"
          ~msg:"retry {operation}"
          ~level:Notice
          ("operation", Operation.encoding)
          ~pp1:Operation.pp

      let included =
        declare_3
          ~name:"included"
          ~msg:"included operations of {block} at level {level}: {operations}"
          ~level:Notice
          ("block", Block_hash.encoding)
          ("level", Data_encoding.int32)
          ("operations", Data_encoding.list Inj_operation.Hash.encoding)
          ~pp3:pp_operations_hash_list

      let revert_operations =
        declare_1
          ~name:"revert_operations"
          ~msg:"reverting operations: {operations}"
          ~level:Notice
          ("operations", Data_encoding.list Inj_operation.Hash.encoding)
          ~pp1:pp_operations_hash_list

      let confirmed_level =
        declare_1
          ~name:"confirmed_level"
          ~msg:"confirmed Tezos level {level}"
          ~level:Notice
          ("level", Data_encoding.int32)

      let loaded_from_disk =
        declare_2
          ~name:"loaded_from_disk"
          ~msg:"loaded {nb} elements in {kind} from disk"
          ~level:Notice
          ("nb", Data_encoding.int31)
          ("kind", Data_encoding.string)

      let corrupted_operation_on_disk =
        declare_2
          ~name:"corrupted_operation_on_disk"
          ~msg:"ignoring unreadable file {file} on disk: {error}"
          ~level:Warning
          ("file", Data_encoding.string)
          ("error", Error_monad.trace_encoding)
          ~pp1:Format.pp_print_string
          ~pp2:Error_monad.pp_print_trace

      let inject_wait =
        declare_1
          ~name:"inject_wait"
          ~msg:"waiting {delay} seconds to trigger injection"
          ~level:Notice
          ("delay", Data_encoding.float)

      let never_included =
        declare_2
          ~name:"never_included"
          ~msg:"{operation} was never included in a block after {ttl} blocks"
          ~level:Warning
          ("operation", Operation.encoding)
          ("ttl", Data_encoding.int31)
          ~pp1:Operation.pp
    end
