(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol_client_context
open Protocol
open Alpha_context

type error += Failed_to_checkout_context

type error += Invalid_context

let wrap_error_lwt x = x >>= fun x -> Lwt.return @@ Environment.wrap_tzresult x

let ( >>=?? ) x k = wrap_error_lwt x >>=? k

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.failed_to_checkout_context"
    ~title:"Failed to checkout context"
    ~description:"The given context hash does not exists in the context."
    ~pp:(fun ppf () -> Format.fprintf ppf "Failed to checkout the context")
    Data_encoding.unit
    (function Failed_to_checkout_context -> Some () | _ -> None)
    (fun () -> Failed_to_checkout_context) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.invalid_context"
    ~title:"Invalid context"
    ~description:"Occurs when the context is inconsistent."
    ~pp:(fun ppf () -> Format.fprintf ppf "The given context is invalid.")
    Data_encoding.unit
    (function Invalid_context -> Some () | _ -> None)
    (fun () -> Invalid_context)

type incremental = {
  predecessor : Baking_state.block_info;
  context : Tezos_protocol_environment.Context.t;
  state : Protocol.validation_state;
  rev_operations : Operation.packed list;
  header : Tezos_base.Block_header.shell_header;
}

let load_context ~context_path =
  protect (fun () ->
      Context.init ~readonly:true context_path >>= fun index ->
      return (Abstract_context_index.abstract index))

let check_context_consistency (abstract_index : Abstract_context_index.t)
    context_hash =
  (* Hypothesis : the version key exists *)
  let version_key = ["version"] in
  abstract_index.checkout_fun context_hash >>= function
  | None -> fail Failed_to_checkout_context
  | Some context -> (
      Context_ops.mem context version_key >>= function
      | true -> return_unit
      | false -> fail Invalid_context)

let begin_construction ~timestamp ?protocol_data
    (abstract_index : Abstract_context_index.t) predecessor chain_id =
  let {Baking_state.shell = pred_shell; hash = pred_hash; _} = predecessor in
  abstract_index.checkout_fun pred_shell.context >>= function
  | None -> fail Failed_to_checkout_context
  | Some context ->
      let header : Tezos_base.Block_header.shell_header =
        Tezos_base.Block_header.
          {
            predecessor = pred_hash;
            proto_level = pred_shell.proto_level;
            validation_passes = 0;
            fitness = pred_shell.fitness;
            timestamp;
            level = pred_shell.level;
            context = Context_hash.zero (* fake context hash *);
            operations_hash = Operation_list_list_hash.zero (* fake op hash *);
          }
      in
      Lifted_protocol.begin_construction
        ~chain_id
        ~predecessor_context:context
        ~predecessor_timestamp:pred_shell.timestamp
        ~predecessor_fitness:pred_shell.fitness
        ~predecessor_level:pred_shell.level
        ~predecessor:pred_hash
        ?protocol_data
        ~timestamp
        ~cache:`Lazy
        ()
      >>=? fun state ->
      return {predecessor; context; state; rev_operations = []; header}

let add_operation st (op : Operation.packed) =
  Protocol.apply_operation st.state op >>=?? fun (state, receipt) ->
  return ({st with state; rev_operations = op :: st.rev_operations}, receipt)

let finalize_construction inc =
  Protocol.finalize_block inc.state (Some inc.header) >>=?? return
