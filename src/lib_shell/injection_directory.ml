(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let read_chain_id validator chain =
  let open Lwt_syntax in
  let distributed_db = Validator.distributed_db validator in
  let store = Distributed_db.store distributed_db in
  match chain with
  | None -> Lwt.return_none
  | Some chain ->
      let* v = Chain_directory.get_chain_id store chain in
      Lwt.return_some v

let inject_block validator ?force ?chain bytes operations =
  let open Lwt_result_syntax in
  let*! chain_id = read_chain_id validator chain in
  let* hash, block =
    Validator.validate_block validator ?force ?chain_id bytes operations
  in
  return
    ( hash,
      let* _ = block in
      return_unit )

let inject_operation validator ~force ?chain bytes =
  let open Lwt_result_syntax in
  let*! chain_id = read_chain_id validator chain in
  match Data_encoding.Binary.of_bytes_opt Operation.encoding bytes with
  | None -> failwith "Can't parse the operation"
  | Some op ->
      let t =
        (Validator.inject_operation
           validator
           ~force
           ?chain_id
           op
         [@profiler.wrap_f
           {driver_ids = [Opentelemetry]}
             (Opentelemetry_profiler.trace_operation
                (`Operation op)
                "inject_operation")])
      in

      let hash = Operation.hash op in
      return (hash, t)

let inject_operations validator ~force ?chain bytes_list =
  let open Lwt_result_syntax in
  let* rev_hashes, rev_promises =
    List.fold_left_es
      (fun (hashes, promises) bytes ->
        let* hash, promise = inject_operation validator ~force ?chain bytes in
        return (hash :: hashes, promise :: promises))
      ([], [])
      bytes_list
  in
  let hashes = List.rev rev_hashes in
  let promises = List.rev rev_promises in
  let fold_errors (has_failed, result) promise_result oph =
    match promise_result with
    | Ok () ->
        ( has_failed,
          Injection_services.Injection_operation_succeed_case oph :: result )
    | Error trace ->
        (* The list will be reversed. *)
        ( true,
          List.rev_append trace
          @@ (Injection_services.Injection_operation_error_case oph :: result)
        )
  in
  let join_results l =
    let has_failed, result =
      WithExceptions.Result.get_ok ~loc:__LOC__
      @@ List.fold_left2
           ~when_different_lengths:()
           fold_errors
           (false, [])
           l
           hashes
    in
    if not has_failed then Ok ()
    else
      Result_syntax.fail
        (Injection_services.Injection_operations_error :: List.rev result)
  in
  let t = Lwt.map join_results (Lwt.all promises) in
  return (hashes, t)

let inject_protocol store proto =
  let open Lwt_result_syntax in
  let proto_bytes = Data_encoding.Binary.to_bytes_exn Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    let*! b = Updater.compile hash proto in
    match b with
    | false -> failwith "Compilation failed (%a)" Protocol_hash.pp_short hash
    | true -> (
        let*! o = Store.Protocol.store store hash proto in
        match o with
        | None ->
            failwith
              "Previously registered protocol (%a)"
              Protocol_hash.pp_short
              hash
        | Some _ -> return_unit)
  in
  Lwt.return (hash, validation)

let build_rpc_directory validator =
  let open Lwt_result_syntax in
  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.store distributed_db in
  let dir : unit Tezos_rpc.Directory.t ref = ref Tezos_rpc.Directory.empty in
  let register0 s f =
    dir := Tezos_rpc.Directory.register !dir s (fun () p q -> f p q)
  in
  let inject_operation ~force q contents =
    let* hash, wait =
      inject_operation validator ~force ?chain:q#chain contents
    in
    let* () = if q#async then return_unit else wait in
    return hash
  in
  let inject_operations q contents =
    let* hashes, wait =
      inject_operations validator ~force:q#force ?chain:q#chain contents
    in
    let* () = if q#async then return_unit else wait in
    return hashes
  in
  register0 Injection_services.S.block (fun q (raw, operations) ->
      let* hash, wait =
        inject_block validator ?chain:q#chain ~force:q#force raw operations
      in
      let* () = if q#async then return_unit else wait in
      return hash) ;
  register0 Injection_services.S.operation (inject_operation ~force:false) ;
  register0
    Injection_services.S.private_operation
    (inject_operation ~force:true) ;
  register0 Injection_services.S.private_operations inject_operations ;
  register0 Injection_services.S.protocol (fun q protocol ->
      let*! hash, wait = inject_protocol state protocol in
      let* () = if q#async then return_unit else wait in
      return hash) ;
  !dir
