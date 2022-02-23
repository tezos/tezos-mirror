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
  let distributed_db = Validator.distributed_db validator in
  let store = Distributed_db.store distributed_db in
  match chain with
  | None -> Lwt.return_none
  | Some chain -> Chain_directory.get_chain_id store chain >>= Lwt.return_some

let inject_block validator ?force ?chain bytes operations =
  read_chain_id validator chain >>= fun chain_id ->
  Validator.validate_block validator ?force ?chain_id bytes operations
  >>=? fun (hash, block) -> return (hash, block >>=? fun _ -> return_unit)

let inject_operation validator ~force ?chain bytes =
  read_chain_id validator chain >>= fun chain_id ->
  let t =
    match Data_encoding.Binary.of_bytes_opt Operation.encoding bytes with
    | None -> failwith "Can't parse the operation"
    | Some op -> Validator.inject_operation validator ~force ?chain_id op
  in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_protocol store proto =
  let proto_bytes = Data_encoding.Binary.to_bytes_exn Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    Updater.compile hash proto >>= function
    | false -> failwith "Compilation failed (%a)" Protocol_hash.pp_short hash
    | true -> (
        Store.Protocol.store store hash proto >>= function
        | None ->
            failwith
              "Previously registered protocol (%a)"
              Protocol_hash.pp_short
              hash
        | Some _ -> return_unit)
  in
  Lwt.return (hash, validation)

let build_rpc_directory validator =
  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.store distributed_db in
  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q)
  in
  let inject_operation ~force q contents =
    inject_operation validator ~force ?chain:q#chain contents
    >>= fun (hash, wait) ->
    (if q#async then return_unit else wait) >>=? fun () -> return hash
  in
  register0 Injection_services.S.block (fun q (raw, operations) ->
      inject_block validator ?chain:q#chain ~force:q#force raw operations
      >>=? fun (hash, wait) ->
      (if q#async then return_unit else wait) >>=? fun () -> return hash) ;
  register0 Injection_services.S.operation (inject_operation ~force:false) ;
  register0
    Injection_services.S.private_operation
    (inject_operation ~force:true) ;
  register0 Injection_services.S.protocol (fun q protocol ->
      inject_protocol state protocol >>= fun (hash, wait) ->
      (if q#async then return_unit else wait) >>=? fun () -> return hash) ;
  !dir
