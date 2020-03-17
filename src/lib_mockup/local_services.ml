(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Directory = Resto_directory.Make (RPC_encoding)
module Service = Resto.MakeService (RPC_encoding)
open Tezos_shell_services

type error += Injection_not_possible

type error += Cannot_parse_op

let () =
  register_error_kind
    `Temporary
    ~id:"local_services.Injection_not_possible"
    ~title:"Injection_not_possible"
    ~description:"Injection not possible"
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Injection not possible in mockup mode without on-disk mockup context.")
    Data_encoding.unit
    (function Injection_not_possible -> Some () | _ -> None)
    (fun () -> Canceled)

let () =
  register_error_kind
    `Temporary
    ~id:"local_services.Cannot_parse_op"
    ~title:"Cannot_parse_op"
    ~description:"Cannot parse operation"
    ~pp:(fun ppf () -> Format.pp_print_string ppf "Cannot parse operation.")
    Data_encoding.unit
    (function Injection_not_possible -> Some () | _ -> None)
    (fun () -> Canceled)

(* Since we bypass the node but still use the RPC mechanism for procedure
   calls, we have to register some RPCs ourselves. *)

let rec print_path : type pr p. (pr, p) Resto.Internal.path -> string list =
 fun path ->
  match path with
  | Root ->
      []
  | Static (path, s) ->
      s :: print_path path
  | Dynamic (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path
  | DynamicTail (path, arg) ->
      Printf.sprintf "<%s>" arg.descr.name :: print_path path

let print_service : type p q i o. (_, _, p, q, i, o, _) Service.t -> string =
 fun serv ->
  let iserv = Service.Internal.to_service serv in
  String.concat "/" (List.rev (print_path iserv.path))

(* We need to construct a dummy p2p to build the associated
   rpc directory. *)
let init_fake_p2p =
  let open Tezos_p2p in
  let peer_meta_config =
    {
      P2p_params.peer_meta_encoding = Tezos_p2p_services.Peer_metadata.encoding;
      peer_meta_initial = Tezos_p2p_services.Peer_metadata.empty;
      score = (fun _ -> 0.0);
    }
  in
  let message_config : unit P2p_params.message_config =
    {
      encoding = [];
      chain_name = Distributed_db_version.Name.of_string "TEZOS_CLIENT_MOCKUP";
      (* The following cannot be empty. *)
      distributed_db_versions = [Distributed_db_version.zero];
    }
  in
  fun () ->
    P2p.faked_network
      message_config
      peer_meta_config
      Tezos_p2p_services.Connection_metadata.
        {disable_mempool = true; private_node = true}

(* Create dummy RPC directory for the p2p *)
let p2p () =
  let fake_p2p = init_fake_p2p () in
  Tezos_p2p.P2p_directory.build_rpc_directory fake_p2p

let chain () =
  Directory.prefix
    Tezos_shell_services.Chain_services.path
    (Directory.register
       Directory.empty
       Tezos_shell_services.Chain_services.S.chain_id
       (fun _ () () -> RPC_answer.return Chain_id.zero))

let protocols protocol_hash =
  let path =
    let open Tezos_rpc.RPC_path in
    prefix Block_services.chain_path Block_services.path
  in
  let service =
    Tezos_rpc.RPC_service.prefix path Block_services.Empty.S.protocols
  in
  Directory.register Directory.empty service (fun _prefix () () ->
      Lwt.return
        (`Ok
          {
            Block_services.current_protocol = protocol_hash;
            next_protocol = protocol_hash;
          }))

let monitor (rpc_context : Tezos_protocol_environment.rpc_context) =
  let open Tezos_protocol_environment in
  let {block_hash; block_header; _} = rpc_context in
  Tezos_rpc.RPC_directory.gen_register
    Directory.empty
    Monitor_services.S.bootstrapped
    (fun () () () -> RPC_answer.return (block_hash, block_header.timestamp))

let block_hash (rpc_context : Tezos_protocol_environment.rpc_context) =
  let path =
    let open Tezos_rpc.RPC_path in
    prefix Block_services.chain_path Block_services.path
  in
  let service =
    Tezos_rpc.RPC_service.prefix path Block_services.Empty.S.hash
  in
  (* Always return the head. *)
  Directory.register Directory.empty service (fun _prefix () () ->
      RPC_answer.return rpc_context.block_hash)

let preapply (mockup_env : Registration.mockup_environment)
    (rpc_context : Tezos_protocol_environment.rpc_context) =
  let (module Mockup_environment) = mockup_env in
  Directory.prefix
    (Tezos_rpc.RPC_path.prefix
       Tezos_shell_services.Chain_services.path
       Block_services.path)
    (Directory.register
       Directory.empty
       Mockup_environment.Block_services.S.Helpers.Preapply.operations
       (fun _prefix () op_list ->
         let outcome =
           Lwt_main.run
             (let predecessor = rpc_context.block_hash in
              let header = rpc_context.block_header in
              let predecessor_context = rpc_context.context in
              let chain_id = Chain_id.zero in
              Mockup_environment.Protocol.begin_construction
                ~chain_id
                ~predecessor_context
                ~predecessor_timestamp:header.timestamp
                ~predecessor_level:header.level
                ~predecessor_fitness:header.fitness
                ~predecessor
                ~timestamp:
                  (Time.System.to_protocol
                     (Tezos_stdlib_unix.Systime_os.now ()))
                ()
              >>=? fun state ->
              fold_left_s
                (fun (state, acc) op ->
                  Mockup_environment.Protocol.apply_operation state op
                  >>=? fun (state, result) ->
                  return (state, (op.protocol_data, result) :: acc))
                (state, [])
                op_list
              >>=? fun (state, acc) ->
              Mockup_environment.Protocol.finalize_block state
              >>=? fun _ -> return (List.rev acc))
         in
         match outcome with
         | Ok result ->
             RPC_answer.return result
         | Error errs ->
             RPC_answer.fail errs))

let inject_operation (mockup_env : Registration.mockup_environment)
    (rpc_context : Tezos_protocol_environment.rpc_context) (mem_only : bool)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  Directory.register
    Directory.empty
    Tezos_shell_services.Injection_services.S.operation
    (fun _q _contents operation_bytes ->
      if mem_only then RPC_answer.fail [Injection_not_possible]
      else
        match
          Data_encoding.Binary.of_bytes Operation.encoding operation_bytes
        with
        | Error _ ->
            RPC_answer.fail [Cannot_parse_op]
        | Ok ({Operation.shell = shell_header; proto} as op) -> (
            let operation_hash = Operation.hash op in
            let proto_op_opt =
              Data_encoding.Binary.of_bytes
                Mockup_environment.Protocol.operation_data_encoding
                proto
            in
            match proto_op_opt with
            | Error _ ->
                RPC_answer.fail [Cannot_parse_op]
            | Ok operation_data -> (
                let op =
                  {
                    Mockup_environment.Protocol.shell = shell_header;
                    protocol_data = operation_data;
                  }
                in
                let predecessor = rpc_context.block_hash in
                let header = rpc_context.block_header in
                let predecessor_context = rpc_context.context in
                let chain_id = Chain_id.zero in
                let result =
                  Lwt_main.run
                    ( Mockup_environment.Protocol.begin_construction
                        ~chain_id
                        ~predecessor_context
                        ~predecessor_timestamp:header.timestamp
                        ~predecessor_level:header.level
                        ~predecessor_fitness:header.fitness
                        ~predecessor
                        ~timestamp:
                          (Time.System.to_protocol
                             (Tezos_stdlib_unix.Systime_os.now ()))
                        ()
                    >>=? fun state ->
                    Mockup_environment.Protocol.apply_operation state op
                    >>=? fun (state, receipt) ->
                    Mockup_environment.Protocol.finalize_block state
                    >>=? fun (validation_result, _block_header_metadata) ->
                    return (validation_result, receipt) )
                in
                match result with
                | Ok ({context; _}, _receipt) ->
                    let rpc_context = {rpc_context with context} in
                    Lwt.bind
                      (write_context_callback rpc_context)
                      (fun result ->
                        match result with
                        | Ok () ->
                            RPC_answer.return operation_hash
                        | Error errs ->
                            RPC_answer.fail errs)
                | Error errs ->
                    RPC_answer.fail errs ) ))

let build_shell_directory (mockup_env : Registration.mockup_environment)
    (rpc_context : Tezos_protocol_environment.rpc_context) (mem_only : bool)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  let directory = ref Directory.empty in
  let merge dir = directory := Directory.merge dir !directory in
  merge (p2p ()) ;
  merge (chain ()) ;
  merge (monitor rpc_context) ;
  merge (protocols Mockup_environment.Protocol.hash) ;
  merge (block_hash rpc_context) ;
  merge (preapply mockup_env rpc_context) ;
  merge
    (inject_operation mockup_env rpc_context mem_only write_context_callback) ;
  !directory
