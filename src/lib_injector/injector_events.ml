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
    (Tags :
      module type of Injector_tags.Make (Parameters.Tag))
        (Operation : PARAM_OPERATION)
        (Inj_operation : INJECTOR_OPERATION with type operation = Operation.t)
        (Request : module type of Request (Parameters.Tag) (Inj_operation)) =
    struct
  include Internal_event.Simple

  let section = Parameters.events_section @ ["injector"]

  let monitoring_error =
    declare_1
      ~section
      ~name:"monitoring_error"
      ~msg:"[Warning] (ignored) in monitoring: {error}"
      ~level:Warning
      ("error", trace_encoding)
      ~pp1:pp_print_trace

  let metrics_error =
    declare_1
      ~section
      ~name:"metrics_error"
      ~level:Error
      ~msg:"[Error] Failed to complete metrics operation due to error {error}"
      ("error", Data_encoding.string)

  let metrics_error error =
    Internal_event.Simple.(emit__dont_wait__use_with_care metrics_error) error

  let pp_key_alias =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt "; ")
        pp_print_string)

  let declare_1 ~name ~msg ~level ?pp1 enc1 =
    declare_3
      ~section
      ~name
      ~msg:("[{signers}] " ^ msg)
      ~level
      ("signers", Data_encoding.(list string))
      ("tags", Tags.encoding)
      enc1
      ~pp1:pp_key_alias
      ~pp2:Tags.pp
      ?pp3:pp1

  let declare_2 ~name ~msg ~level ?pp1 ?pp2 enc1 enc2 =
    declare_4
      ~section
      ~name
      ~msg:("[{signers}] " ^ msg)
      ~level
      ("signers", Data_encoding.(list string))
      ("tags", Tags.encoding)
      enc1
      enc2
      ~pp1:pp_key_alias
      ~pp2:Tags.pp
      ?pp3:pp1
      ?pp4:pp2

  let declare_3 ~name ~msg ~level ?pp1 ?pp2 ?pp3 enc1 enc2 enc3 =
    declare_5
      ~section
      ~name
      ~msg:("[{signers}] " ^ msg)
      ~level
      ("signers", Data_encoding.(list string))
      ("tags", Tags.encoding)
      enc1
      enc2
      enc3
      ~pp1:pp_key_alias
      ~pp2:Tags.pp
      ?pp3:pp1
      ?pp4:pp2
      ?pp5:pp3

  let request_failed =
    declare_3
      ~name:"request_failed"
      ~msg:"[Warning] Request {view} failed ({worker_status}): {errors}"
      ~level:Warning
      ("view", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

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
      ~msg:"Processing new Tezos head {head}"
      ~level:Debug
      ("head", Block_hash.encoding)

  let cannot_compute_reorg =
    declare_1
      ~name:"cannot_compute_reorg"
      ~msg:"[Warning] Cannot compute reorg for new block {head}"
      ~level:Warning
      ("head", Block_hash.encoding)

  let injecting_pending =
    declare_1
      ~name:"injecting_pending"
      ~msg:"Injecting {count} pending operations"
      ~level:Debug
      ("count", Data_encoding.int31)

  let pp_operations_list ~numbered ppf operations =
    Format.fprintf ppf "@[<v>" ;
    List.iteri
      (fun i op ->
        if i <> 0 then Format.fprintf ppf "@," ;
        if numbered then Format.fprintf ppf "%d. " (i + 1)
        else Format.pp_print_string ppf "- " ;
        Operation.pp ppf op)
      operations ;
    Format.fprintf ppf "@]"

  let pp_operations_hash_list ppf operations =
    Format.fprintf ppf "@[%a@]" (Format.pp_print_list Id.pp) operations

  let number_of_operations_in_queue =
    declare_1
      ~name:"number_of_operations_in_queue"
      ~msg:
        "Injector's queue: there is currently {number_of_operations} \
         operations waiting to be injected"
      ~level:Info
      ("number_of_operations", Data_encoding.int31)

  let considered_operations_info =
    declare_1
      ~name:"considered_operations_info"
      ~msg:
        "Injector's queue: the following operations are being considered for \
         injection {operations}"
      ~level:Debug
      ("operations", Data_encoding.list Operation.encoding)
      ~pp1:(pp_operations_list ~numbered:true)

  let dropped_operation =
    declare_2
      ~name:"dropped_operation"
      ~msg:"Dropping operation {operation} with error {error}"
      ~level:Info
      ("operation", Operation.encoding)
      ("error", Data_encoding.option Error_monad.trace_encoding)
      ~pp1:Operation.pp
      ~pp2:(fun ppf -> Option.iter (Error_monad.pp_print_trace ppf))

  let simulating_operations =
    declare_2
      ~name:"simulating_operations"
      ~msg:"Simulating operations (force = {force}): {operations}"
      ~level:Info
      ("operations", Data_encoding.list Operation.encoding)
      ("force", Data_encoding.bool)
      ~pp1:(pp_operations_list ~numbered:true)

  let discard_error_operation =
    declare_3
      ~name:"discard_error_operation"
      ~msg:"Discarding operation {operation} failing {count} times with {error}"
      ~level:Error
      ("operation", Operation.encoding)
      ~pp1:Operation.pp
      ("count", Data_encoding.int31)
      ("error", Data_encoding.option Error_monad.trace_encoding)
      ~pp3:(fun ppf -> Option.iter (Error_monad.pp_print_trace ppf))

  let error_simulation_operation =
    declare_3
      ~name:"error_simulation_operation"
      ~msg:"Simulation for {operation} failing {count} times with {error}"
      ~level:Debug
      ("operation", Operation.encoding)
      ~pp1:Operation.pp
      ("count", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let injected =
    declare_2
      ~name:"injected"
      ~msg:"Injected {nb} operations in {oph}"
      ~level:Info
      ("nb", Data_encoding.int31)
      ("oph", Operation_hash.encoding)

  let injected_ops =
    declare_2
      ~name:"injected_ops"
      ~msg:"Injected operations in {oph}: {operations}"
      ~level:Notice
      ("oph", Operation_hash.encoding)
      ("operations", Data_encoding.list Operation.encoding)
      ~pp2:(pp_operations_list ~numbered:true)

  let total_injected_ops =
    declare_1
      ~name:"total_injected_ops"
      ~msg:"Total {nb} operations injected in the last round"
      ~level:Info
      ("nb", Data_encoding.int31)

  let add_pending =
    declare_1
      ~name:"add_pending"
      ~msg:"Add {operation} to pending"
      ~level:Info
      ("operation", Operation.encoding)
      ~pp1:Operation.pp

  let retry_operation =
    declare_1
      ~name:"retry_operation"
      ~msg:"Retry {operation}"
      ~level:Debug
      ("operation", Operation.encoding)
      ~pp1:Operation.pp

  let included =
    declare_3
      ~name:"included"
      ~msg:"Included operations of {block} at level {level}: {operations}"
      ~level:Info
      ("block", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("operations", Data_encoding.list Id.encoding)
      ~pp3:pp_operations_hash_list

  let revert_operations =
    declare_1
      ~name:"revert_operations"
      ~msg:"Reverting operations: {operations}"
      ~level:Info
      ("operations", Data_encoding.list Id.encoding)
      ~pp1:pp_operations_hash_list

  let confirmed_level =
    declare_1
      ~name:"confirmed_level"
      ~msg:"Confirmed Tezos level {level}"
      ~level:Debug
      ("level", Data_encoding.int32)

  let loaded_from_disk =
    declare_2
      ~name:"loaded_from_disk"
      ~msg:"Loaded {nb} elements in {kind} from disk"
      ~level:Debug
      ("nb", Data_encoding.int31)
      ("kind", Data_encoding.string)

  let corrupted_operation_on_disk =
    declare_2
      ~name:"corrupted_operation_on_disk"
      ~msg:"Ignoring unreadable file {file} on disk: {error}"
      ~level:Info
      ("file", Data_encoding.string)
      ("error", Error_monad.trace_encoding)
      ~pp1:Format.pp_print_string
      ~pp2:Error_monad.pp_print_trace

  let inject_wait =
    declare_1
      ~name:"inject_wait"
      ~msg:"Waiting {delay} seconds to trigger injection"
      ~level:Debug
      ("delay", Data_encoding.float)

  let never_included =
    declare_2
      ~name:"never_included"
      ~msg:
        "[Warning] {operation} was never included in a block after {ttl} blocks"
      ~level:Warning
      ("operation", Operation.encoding)
      ("ttl", Data_encoding.int31)
      ~pp1:Operation.pp
end
