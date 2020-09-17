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
    (function Cannot_parse_op -> Some () | _ -> None)
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
      distributed_db_versions = Distributed_db_version.[zero; one];
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

let chain chain_id =
  Directory.prefix
    Tezos_shell_services.Chain_services.path
    (Directory.register
       Directory.empty
       Tezos_shell_services.Chain_services.S.chain_id
       (fun _ () () -> RPC_answer.return chain_id))

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

let check_chain_chain_id (chain : Block_services.chain) (chain_id : Chain_id.t)
    =
  let chain_chain_id =
    match chain with
    | `Main ->
        Chain_id.hash_string ["main"]
    | `Test ->
        Chain_id.hash_string ["test"]
    | `Hash cid ->
        cid
  in
  unless (Chain_id.equal chain_id chain_chain_id) (fun () ->
      let msg =
        let open Format in
        asprintf
          "Mismatched chain id: got %a but this mockup client expected %a."
          (fun ppf chain ->
            match chain with
            | `Main ->
                fprintf
                  ppf
                  "main (%a)"
                  Chain_id.pp
                  (Chain_id.hash_string ["main"])
            | `Test ->
                fprintf
                  ppf
                  "test (%a)"
                  Chain_id.pp
                  (Chain_id.hash_string ["test"])
            | `Hash chain_id ->
                Chain_id.pp ppf chain_id)
          chain
          Chain_id.pp
          chain_id
      in
      Lwt.fail_with msg)

let pending_operations (mockup_env : Registration.mockup_environment) chain_id
    (dirname : string) =
  let (module M) = mockup_env in
  (* TODO: Don't know why it fails *)
  let op_data_encoding = M.Protocol.operation_data_encoding in
  let read_operations () =
    let op_encoding =
      Data_encoding.(
        dynamic_size
        @@ obj2
             (req "shell_header" Operation.shell_header_encoding)
             (req "protocol_data" op_data_encoding))
    in
    let ops_encoding = Data_encoding.Variable.list op_encoding in
    (* Read current mempool *)
    let mempool_file = (Files.mempool ~dirname :> string) in
    (* Format.printf "Reading mempool file %s@." mempool_file ; *)
    Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file mempool_file
    >>=? fun json -> return @@ Data_encoding.Json.destruct ops_encoding json
  in
  Directory.register
    Directory.empty
    (* /chains/<chain_id>/mempool/pending_operations *)
    ( M.Block_services.S.Mempool.pending_operations
    @@ Block_services.mempool_path Block_services.chain_path )
    (fun ((), chain) () () ->
      check_chain_chain_id chain chain_id
      >>= function
      | Error errs ->
          RPC_answer.fail errs
      | Ok () -> (
          read_operations ()
          >>= function
          | Error errs ->
              RPC_answer.fail errs
          | Ok pooled_operations ->
              (* Format.printf "Retrieving unprocessed operations@." ; *)
              let unprocessed =
                List.fold_left
                  (fun map (shell_header, operation_data) ->
                    let op =
                      {
                        M.Protocol.shell = shell_header;
                        protocol_data = operation_data;
                      }
                    in
                    match
                      Data_encoding.Binary.to_bytes
                        op_data_encoding
                        operation_data
                    with
                    | Error _ ->
                        map
                    | Ok proto ->
                        let operation_hash =
                          Operation.hash
                            {Operation.shell = shell_header; proto}
                        in
                        (* Format.printf
                         *   "Pending operations op hash: %a@."
                         *   Operation_hash.pp
                         *   operation_hash ; *)
                        Operation_hash.Map.add operation_hash op map)
                  Operation_hash.Map.empty
                  pooled_operations
              in
              Lwt.return
                (`Ok
                  {
                    M.Block_services.Mempool.applied = [];
                    refused = Operation_hash.Map.empty;
                    branch_refused = Operation_hash.Map.empty;
                    branch_delayed = Operation_hash.Map.empty;
                    unprocessed;
                  }) ))

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

let live_blocks (mockup_env : Registration.mockup_environment)
    (rpc_context : Tezos_protocol_environment.rpc_context) =
  let (module Mockup_environment) = mockup_env in
  Directory.prefix
    (Tezos_rpc.RPC_path.prefix
       (* /chains/<chain> *)
       Tezos_shell_services.Chain_services.path
       (* blocks/<block_id> *)
       Block_services.path)
  @@ Directory.register
       Directory.empty
       Mockup_environment.Block_services.S.live_blocks
       (fun _ () () ->
         let set = Block_hash.Set.singleton rpc_context.block_hash in
         RPC_answer.return set)

let preapply_block (mockup_env : Registration.mockup_environment)
    (rpc_context : Tezos_protocol_environment.rpc_context) chain_id =
  let (module Mockup_environment) = mockup_env in
  Directory.prefix
    (Tezos_rpc.RPC_path.prefix
       (* /chains/<chain> *)
       Tezos_shell_services.Chain_services.path
       (* blocks/<block_id> *)
       Block_services.path)
  @@ Directory.register
       Directory.empty
       Mockup_environment.Block_services.S.Helpers.Preapply.block
       (fun (((), chain), _block) _ {operations; protocol_data = _} ->
         (* Why is operations a operation list list ?
            Is there any specific
            implicit meaning to the different sublists ? *)
         (* Format.printf
          *   "block %s with %d operation list @."
          *   (Block_services.to_string block)
          *   (List.length operations) ;
          * List.iteri
          *   (fun i ops ->
          *     Format.printf "@[<v>%i: " i ;
          *     if ops = [] then Format.printf "[]"
          *     else
          *       List.iter
          *         (fun Mockup_environment.Protocol.{protocol_data; _} ->
          *           let encoding =
          *             Mockup_environment.Protocol.operation_data_encoding
          *           in
          *           let json =
          *             Data_encoding.Json.construct encoding protocol_data
          *           in
          *           Format.printf "%a@;" Data_encoding.Json.pp json)
          *         ops ;
          *     Format.printf "@]@.")
          *   operations ; *)
         check_chain_chain_id chain chain_id
         >>= function
         | Error errs ->
             RPC_answer.fail errs
         | Ok () -> (
             (let predecessor = rpc_context.block_hash in
              let header = rpc_context.block_header in
              let predecessor_context = rpc_context.context in
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
              >>=? fun validation_state ->
              fold_left_s
                (fold_left_s (fun (validation_state, preapply_results) op ->
                     Mockup_environment.Protocol.apply_operation
                       validation_state
                       op
                     >>=? fun (validation_state, _) ->
                     match
                       Data_encoding.Binary.to_bytes
                         Mockup_environment.Protocol.operation_data_encoding
                         op.protocol_data
                     with
                     | Error _ ->
                         failwith "foo"
                     | Ok proto ->
                         let op_t = {Operation.shell = op.shell; proto} in
                         let hash = Operation.hash op_t in
                         return
                           ( validation_state,
                             Preapply_result.
                               {empty with applied = [(hash, op_t)]}
                             :: preapply_results )))
                (validation_state, [])
                operations
              >>=? fun (validation_state, preapply_results) ->
              Mockup_environment.Protocol.finalize_block validation_state
              >>=? fun (validation_result, _metadata) ->
              (* Similar to lib_shell.Prevalidation.preapply *)
              let operations_hash =
                let open Preapply_result in
                Operation_list_list_hash.compute
                @@ List.map
                     (fun x ->
                       Operation_list_hash.compute @@ List.map fst x.applied)
                     preapply_results
              in
              let shell_header =
                {
                  rpc_context.block_header with
                  level = Int32.succ rpc_context.block_header.level;
                  (* proto_level should be unchanged in mockup mode since we
                     cannot switch protocols *)
                  predecessor = rpc_context.block_hash;
                  timestamp =
                    Time.System.to_protocol
                      (Tezos_stdlib_unix.Systime_os.now ());
                  operations_hash;
                  validation_passes = List.length preapply_results;
                  fitness = validation_result.fitness;
                  context = Context_hash.zero (* TODO: is that correct ? *);
                }
              in
              return (shell_header, preapply_results))
             >>= function
             | Error errs -> RPC_answer.fail errs | Ok v -> RPC_answer.return v
             ))

let preapply (mockup_env : Registration.mockup_environment)
    (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context) =
  let (module Mockup_environment) = mockup_env in
  Directory.prefix
    (Tezos_rpc.RPC_path.prefix
       (* /chains/<chain> *)
       Tezos_shell_services.Chain_services.path
       (* blocks/<block_id> *)
       Block_services.path)
    (Directory.register
       Directory.empty
       (* /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations *)
       Mockup_environment.Block_services.S.Helpers.Preapply.operations
       (fun _prefix () op_list ->
         (let predecessor = rpc_context.block_hash in
          let header = rpc_context.block_header in
          let predecessor_context = rpc_context.context in
          Mockup_environment.Protocol.begin_construction
            ~chain_id
            ~predecessor_context
            ~predecessor_timestamp:header.timestamp
            ~predecessor_level:header.level
            ~predecessor_fitness:header.fitness
            ~predecessor
            ~timestamp:
              (Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ()))
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
         >>= fun outcome ->
         match outcome with
         | Ok result ->
             RPC_answer.return result
         | Error errs ->
             RPC_answer.fail errs))

let inject_operation_with_mempool
    (mockup_env : Registration.mockup_environment) (dirname : string)
    operation_bytes =
  let (module Mockup_environment) = mockup_env in
  let write_op op =
    let op_data_encoding =
      Mockup_environment.Protocol.operation_data_encoding
    in
    let op_encoding =
      Data_encoding.(
        dynamic_size
        @@ obj2
             (req "shell_header" Operation.shell_header_encoding)
             (req "protocol_data" op_data_encoding))
    in
    let ops_encoding = Data_encoding.Variable.list op_encoding in
    (* Read current mempool *)
    let mempool_file = (Files.mempool ~dirname :> string) in
    Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file mempool_file
    >>=? fun pooled_operations_json ->
    let ops =
      Data_encoding.Json.destruct ops_encoding pooled_operations_json
    in
    let ops' = op :: ops in
    Data_encoding.Json.construct ops_encoding ops'
    |> Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file mempool_file
  in
  match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
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
          let op = (shell_header, operation_data) in
          write_op op
          >>= function
          | Ok _ ->
              RPC_answer.return operation_hash
          | Error errs ->
              RPC_answer.fail errs ) )

let inject_operation_without_mempool
    (mockup_env : Registration.mockup_environment) (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t)
    operation_bytes =
  let (module Mockup_environment) = mockup_env in
  match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
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
          Mockup_environment.Protocol.begin_construction
            ~chain_id
            ~predecessor_context
            ~predecessor_timestamp:header.timestamp
            ~predecessor_level:header.level
            ~predecessor_fitness:header.fitness
            ~predecessor
            ~timestamp:
              (Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ()))
            ()
          >>=? (fun state ->
                 Mockup_environment.Protocol.apply_operation state op
                 >>=? fun (state, receipt) ->
                 Mockup_environment.Protocol.finalize_block state
                 >>=? fun (validation_result, _block_header_metadata) ->
                 return (validation_result, receipt))
          >>= fun result ->
          match result with
          | Ok ({context; _}, _receipt) ->
              let rpc_context = {rpc_context with context} in
              Lwt.bind (write_context_callback rpc_context) (fun result ->
                  match result with
                  | Ok () ->
                      RPC_answer.return operation_hash
                  | Error errs ->
                      RPC_answer.fail errs)
          | Error errs ->
              RPC_answer.fail errs ) )

(* [inject_block] is a feature that assumes that the mockup is on-disk and
 * uses a mempool. *)
let inject_block (mockup_env : Registration.mockup_environment)
    (dirname : string) (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  let op_data_encoding = Mockup_environment.Protocol.operation_data_encoding in
  let op_encoding =
    Data_encoding.(
      dynamic_size
      @@ obj2
           (req "shell_header" Operation.shell_header_encoding)
           (req "protocol_data" op_data_encoding))
  in
  let ops_encoding = Data_encoding.Variable.list op_encoding in
  let reconstruct (operations : Operation.t list list) =
    let predecessor = rpc_context.block_hash in
    let header = rpc_context.block_header in
    let predecessor_context = rpc_context.context in
    Mockup_environment.Protocol.begin_construction
      ~chain_id
      ~predecessor_context
      ~predecessor_timestamp:header.timestamp
      ~predecessor_level:header.level
      ~predecessor_fitness:header.fitness
      ~predecessor
      ~timestamp:
        (Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ()))
      ()
    >>=? fun validation_state ->
    Format.printf "Got %d operations@." (List.length @@ List.flatten operations) ;
    let i = ref 0 in
    fold_left_s
      (fold_left_s (fun (validation_state, _results) op ->
           incr i ;
           match
             Data_encoding.Binary.of_bytes op_data_encoding op.Operation.proto
           with
           | Error _ ->
               failwith "Cannot parse"
           | Ok operation_data ->
               let op =
                 {
                   Mockup_environment.Protocol.shell = op.shell;
                   protocol_data = operation_data;
                 }
               in
               Mockup_environment.Protocol.apply_operation validation_state op
               >>=? fun (validation_state, _receipt) ->
               return (validation_state, _receipt :: _results)))
      (validation_state, [])
      operations
    >>=? fun (validation_state, _) ->
    Mockup_environment.Protocol.finalize_block validation_state
  in
  Directory.register
    Directory.empty
    (* /injection/block *)
    Tezos_shell_services.Injection_services.S.block
    (* See injection_directory.ml for vanilla implementation *)
    (fun () _ (bytes, operations) ->
      (* TODO: This is probably where it all comes together i.e., where the
         mempool  is actually modified and the context updated at the same time.

         Do we need to ensure an invariant that avoids being in some weird state
         where the mempool is updated on disk but somehow the context fails (or
         vice versa) ?
       *)
      assert (Files.has_mempool ~dirname) ;
      let block_hash = Block_hash.hash_bytes [bytes] in
      match Block_header.of_bytes bytes with
      | None ->
          RPC_answer.fail [Cannot_parse_op]
      | Some block_header -> (
          (* Format.printf
           *   "inject_block: %a - %d bytes from %d operations@."
           *   Block_header.pp
           *   block_header
           *   (Bytes.length bytes)
           *   (List.length operations) ; *)
          let mempool_file = (Files.mempool ~dirname :> string) in
          Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file mempool_file
          >>= function
          | Error errs ->
              RPC_answer.fail errs
          | Ok pooled_operations_json -> (
              (* Let's construct an updated mempool in 3 steps:
                 - reads back the current mempool_file (as map),
                 - removing operations that are to be injected in the next block
                 - then reconstruct a mempool structure to be written back.  *)
              let ops =
                Data_encoding.Json.destruct ops_encoding pooled_operations_json
              in
              let mempool_map =
                List.fold_left
                  (fun map ((shell_header, operation_data) as v) ->
                    match
                      Data_encoding.Binary.to_bytes
                        op_data_encoding
                        operation_data
                    with
                    | Error _ ->
                        assert false
                    | Ok proto ->
                        let h =
                          Operation.hash
                            {Operation.shell = shell_header; proto}
                        in
                        Operation_hash.Map.add h v map)
                  Operation_hash.Map.empty
                  ops
              in
              let new_mempool_map =
                List.fold_left
                  (List.fold_left (fun mempool op ->
                       Operation_hash.Map.remove (Operation.hash op) mempool))
                  mempool_map
                  operations
              in
              Operation_hash.Map.fold (fun _k v l -> v :: l) new_mempool_map []
              |> Data_encoding.Json.construct ops_encoding
              |> Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file mempool_file
              >>= function
              | Error errs ->
                  RPC_answer.fail errs
              | Ok () -> (
                  reconstruct operations
                  >>= function
                  | Error _ ->
                      assert false
                  | Ok ({context; _}, _) -> (
                      Format.printf
                        "Block header: %a@."
                        Data_encoding.Json.pp
                        (Data_encoding.Json.construct
                           Block_header.shell_header_encoding
                           block_header.shell) ;
                      let context' = context in
                      let rpc_context =
                        let block_header =
                          block_header.shell
                          (* with
                             * level = Int32.succ block_header.shell.level;
                             * predecessor = rpc_context.block_hash;
                             * timestamp =
                             *   (\* Here we do not assume that baking should always
                             *      succeed in mockup mode, independently of
                             *      minimal time interval constraint.contqext.
                             *
                             *      If this was actually needed, the new timestamp
                             *      could be the max of now and
                             *      [Baking.minimal_timestamp] *\)
                             *   Time.System.to_protocol
                             *     (Tezos_stdlib_unix.Systime_os.now ()); *)
                        in
                        {
                          {rpc_context with context = context'} with
                          block_hash;
                          block_header;
                        }
                      in
                      write_context_callback rpc_context
                      >>= function
                      | Ok _ ->
                          RPC_answer.return block_hash
                      | Error errs -> (
                          (* We couldn't write the new context. Let's first try to
                             roll back the mempool before raising the errors. *)
                          Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file
                            mempool_file
                            pooled_operations_json
                          >>= function
                          | Error errs' ->
                              RPC_answer.fail (errs @ errs')
                          | Ok () ->
                              RPC_answer.fail errs ) ) ) ) ))

let inject_operation (mockup_env : Registration.mockup_environment)
    (dirname : string) (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context) (mem_only : bool)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  Directory.register
    Directory.empty
    (* /injection/operation, vanilla client implementation is in
      injection_directory.ml *)
    Tezos_shell_services.Injection_services.S.operation
    (fun _q _contents operation_bytes ->
      if mem_only then RPC_answer.fail [Injection_not_possible]
      else if
        (* Looking at the implementations of the two inject_operation_*
           functions it looks like there is code to share (proto_op_opt,
           operation_data), but it's not that easy to do;
           because types of concerned variables depend on Mockup_environment,
           which cannot cross functions boundaries without putting all that in
           Mockup_sig *)
        Files.has_mempool ~dirname
      then inject_operation_with_mempool mockup_env dirname operation_bytes
      else
        inject_operation_without_mempool
          mockup_env
          chain_id
          rpc_context
          write_context_callback
          operation_bytes)

let build_shell_directory (base_dir : string)
    (mockup_env : Registration.mockup_environment) chain_id
    (rpc_context : Tezos_protocol_environment.rpc_context) (mem_only : bool)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  let directory = ref Directory.empty in
  let merge dir = directory := Directory.merge dir !directory in
  merge (p2p ()) ;
  merge (chain chain_id) ;
  merge (monitor rpc_context) ;
  merge (protocols Mockup_environment.Protocol.hash) ;
  merge (block_hash rpc_context) ;
  merge (preapply mockup_env chain_id rpc_context) ;
  merge (pending_operations mockup_env chain_id base_dir) ;
  merge
    (inject_operation
       mockup_env
       base_dir
       chain_id
       rpc_context
       mem_only
       write_context_callback) ;
  merge
    (inject_block
       mockup_env
       base_dir
       chain_id
       rpc_context
       write_context_callback) ;
  merge (live_blocks mockup_env rpc_context) ;
  merge (preapply_block mockup_env rpc_context chain_id) ;
  !directory
