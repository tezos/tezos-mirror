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

(* [MENV] is a thin extension of [Registration.MOCKUP] comprising some
 * parameters used in most functions. *)
module type MENV = sig
  include Registration.MOCKUP

  val chain_id : Chain_id.t

  val rpc_context : Tezos_protocol_environment.rpc_context

  val base_dir : string
end

module Make (E : MENV) = struct
  (* We need to construct a dummy p2p to build the associated
   rpc directory. *)
  let init_fake_p2p =
    let open Tezos_p2p in
    let peer_meta_config =
      {
        P2p_params.peer_meta_encoding =
          Tezos_p2p_services.Peer_metadata.encoding;
        peer_meta_initial = Tezos_p2p_services.Peer_metadata.empty;
        score = (fun _ -> 0.0);
      }
    in
    let message_config : unit P2p_params.message_config =
      {
        encoding = [];
        chain_name =
          Distributed_db_version.Name.of_string "TEZOS_CLIENT_MOCKUP";
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

  let chain () =
    Directory.prefix
      Tezos_shell_services.Chain_services.path
      (Directory.register
         Directory.empty
         Tezos_shell_services.Chain_services.S.chain_id
         (fun _ () () -> RPC_answer.return E.chain_id))

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

  let monitor () =
    let open Tezos_protocol_environment in
    let {block_hash; block_header; _} = E.rpc_context in
    Tezos_rpc.RPC_directory.gen_register
      Directory.empty
      Monitor_services.S.bootstrapped
      (fun () () () -> RPC_answer.return (block_hash, block_header.timestamp))

  let check_chain (chain : Block_services.chain) =
    let chain_chain_id =
      match chain with
      | `Main ->
          Chain_id.hash_string ["main"]
      | `Test ->
          Chain_id.hash_string ["test"]
      | `Hash cid ->
          cid
    in
    unless (Chain_id.equal E.chain_id chain_chain_id) (fun () ->
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
            E.chain_id
        in
        Lwt.fail_with msg)

  let begin_construction =
    let predecessor = E.rpc_context.block_hash in
    let header = E.rpc_context.block_header in
    let predecessor_context = E.rpc_context.context in
    E.Protocol.begin_construction
      ~chain_id:E.chain_id
      ~predecessor_context
      ~predecessor_timestamp:header.timestamp
      ~predecessor_level:header.level
      ~predecessor_fitness:header.fitness
      ~predecessor
      ~timestamp:
        (Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ()))

  let op_data_encoding = E.Protocol.operation_data_encoding

  let op_encoding =
    Data_encoding.(
      dynamic_size
      @@ obj2
           (req "shell_header" Operation.shell_header_encoding)
           (req "protocol_data" op_data_encoding))

  let ops_encoding = Data_encoding.Variable.list op_encoding

  module L = struct
    module S = Internal_event.Simple

    let section = ["mockup"; "local_services"]

    let warn_trashpool_append =
      let pp1 ppf l =
        match List.length l with
        (* This should not happend as the lone call to this function is
           protected by a "unless"*)
        | 0 ->
            Format.pp_print_string ppf "nothing"
        | 1 ->
            Format.pp_print_string ppf "1 operation"
        | n ->
            Format.fprintf ppf "%d operations" n
      in
      S.declare_1
        ~section
        ~name:"thraspool_append"
        ~msg:"Appending {operations} to trashpool"
        ~level:Internal_event.Warning
        ~pp1
        ("operations", ops_encoding)

    let warn_mempool_mem =
      S.declare_0
        ~section
        ~name:"mempool_mem"
        ~msg:
          "This operation already exists in the mempool and will thus be \
           ignored."
        ~level:Internal_event.Warning
        ()
  end

  type write_mode = Append | Zero_truncate

  module Rw (File_accessor : Files.ACCESSOR) = struct
    let file = (File_accessor.get ~dirname:E.base_dir :> string)

    let unsafe_read () =
      Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file
      >>=? fun json -> return @@ Data_encoding.Json.destruct ops_encoding json

    let read () =
      File_accessor.exists ~dirname:E.base_dir
      >>= function true -> unsafe_read () | false -> return []

    let write ~mode operations =
      ( match mode with
      | Append ->
          read () >>=? fun ops -> return (ops @ operations)
      | Zero_truncate ->
          return operations )
      >>=? fun ops ->
      let json = Data_encoding.Json.construct ops_encoding ops in
      Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file file json

    let append = write ~mode:Append
  end

  module Mempool = Rw (Files.Mempool)
  module Trashpool = Rw (Files.Trashpool)

  let pending_operations () =
    Directory.register
      Directory.empty
      (* /chains/<chain_id>/mempool/pending_operations *)
      ( E.Block_services.S.Mempool.pending_operations
      @@ Block_services.mempool_path Block_services.chain_path )
      (fun ((), chain) () () ->
        check_chain chain
        >>= function
        | Error errs ->
            RPC_answer.fail errs
        | Ok () -> (
            Mempool.read ()
            >>= function
            | Error errs ->
                RPC_answer.fail errs
            | Ok pooled_operations -> (
                List.map_es
                  (fun (shell_header, operation_data) ->
                    let op =
                      {
                        E.Protocol.shell = shell_header;
                        protocol_data = operation_data;
                      }
                    in
                    match
                      Data_encoding.Binary.to_bytes
                        op_data_encoding
                        operation_data
                    with
                    | Error _ ->
                        failwith "mockup pending_operations"
                    | Ok proto ->
                        let operation_hash =
                          Operation.hash
                            {Operation.shell = shell_header; proto}
                        in
                        return (operation_hash, op))
                  pooled_operations
                >>= function
                | Error _ ->
                    RPC_answer.fail [Cannot_parse_op]
                | Ok applied ->
                    Lwt.return
                      (`Ok
                        {
                          E.Block_services.Mempool.applied;
                          refused = Operation_hash.Map.empty;
                          branch_refused = Operation_hash.Map.empty;
                          branch_delayed = Operation_hash.Map.empty;
                          unprocessed = Operation_hash.Map.empty;
                        }) ) ))

  let with_chain chain k =
    check_chain chain
    >>= function Error errs -> RPC_answer.fail errs | Ok () -> k ()

  let shell_header () =
    Directory.prefix
      (Tezos_rpc.RPC_path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.Header.shell_header
         (fun _prefix () () -> RPC_answer.return E.rpc_context.block_header)

  let block_hash () =
    let path =
      let open Tezos_rpc.RPC_path in
      prefix Block_services.chain_path Block_services.path
    in
    let service =
      Tezos_rpc.RPC_service.prefix path Block_services.Empty.S.hash
    in
    (* Always return the head. *)
    Directory.register Directory.empty service (fun _prefix () () ->
        RPC_answer.return E.rpc_context.block_hash)

  let live_blocks () =
    Directory.prefix
      (Tezos_rpc.RPC_path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.live_blocks
         (fun (((), chain), _block) () () ->
           with_chain chain (fun () ->
               let set = Block_hash.Set.singleton E.rpc_context.block_hash in
               RPC_answer.return set))

  let simulate_operation (validation_state, preapply_results) op =
    E.Protocol.apply_operation validation_state op
    >>=? fun (validation_state, _) ->
    match
      Data_encoding.Binary.to_bytes
        E.Protocol.operation_data_encoding
        op.protocol_data
    with
    | Error _ ->
        failwith "mockup preapply_block"
    | Ok proto ->
        let op_t = {Operation.shell = op.shell; proto} in
        let hash = Operation.hash op_t in
        return
          ( validation_state,
            Preapply_result.{empty with applied = [(hash, op_t)]}
            :: preapply_results )

  let preapply_block () =
    Directory.prefix
      (Tezos_rpc.RPC_path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.Helpers.Preapply.block
         (fun (((), chain), _block) o {operations; protocol_data = _} ->
           with_chain chain (fun () ->
               begin_construction ()
               >>=? (fun validation_state ->
                      List.fold_left_es
                        (List.fold_left_es simulate_operation)
                        (validation_state, [])
                        operations
                      >>=? fun (validation_state, preapply_results) ->
                      E.Protocol.finalize_block validation_state
                      >>=? fun (validation_result, _metadata) ->
                      (* Similar to lib_shell.Prevalidation.preapply *)
                      let operations_hash =
                        let open Preapply_result in
                        Operation_list_list_hash.compute
                        @@ List.map
                             (fun x ->
                               Operation_list_hash.compute
                               @@ List.map fst x.applied)
                             preapply_results
                      in
                      let shell_header =
                        {
                          E.rpc_context.block_header with
                          level = Int32.succ E.rpc_context.block_header.level;
                          (* proto_level should be unchanged in mockup mode
                             since we cannot switch protocols *)
                          predecessor = E.rpc_context.block_hash;
                          timestamp =
                            (* The timestamp exists if --minimal-timestamp has
                               been given on the command line *)
                            ( match o#timestamp with
                            | None ->
                                Time.System.to_protocol
                                  (Tezos_stdlib_unix.Systime_os.now ())
                            | Some t ->
                                t );
                          operations_hash;
                          validation_passes = List.length preapply_results;
                          fitness = validation_result.fitness;
                          context =
                            Context_hash.zero (* TODO: is that correct ? *);
                        }
                      in
                      return (shell_header, preapply_results))
               >>= function
               | Error errs ->
                   RPC_answer.fail errs
               | Ok v ->
                   RPC_answer.return v))

  let preapply () =
    Directory.prefix
      (Tezos_rpc.RPC_path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
      (Directory.register
         Directory.empty
         (* /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations *)
         E.Block_services.S.Helpers.Preapply.operations
         (fun ((_, chain), _block) () op_list ->
           with_chain chain (fun () ->
               begin_construction ()
               >>=? (fun state ->
                      List.fold_left_es
                        (fun (state, acc) op ->
                          E.Protocol.apply_operation state op
                          >>=? fun (state, result) ->
                          return (state, (op.protocol_data, result) :: acc))
                        (state, [])
                        op_list
                      >>=? fun (state, acc) ->
                      E.Protocol.finalize_block state
                      >>=? fun _ -> return (List.rev acc))
               >>= fun outcome ->
               match outcome with
               | Ok result ->
                   RPC_answer.return result
               | Error errs ->
                   RPC_answer.fail errs)))

  let need_operation shell_header operation_data =
    Mempool.read ()
    >>=? fun mempool_operations ->
    let op = (shell_header, operation_data) in
    if List.mem op mempool_operations then return_false
    else
      let operations = op :: mempool_operations in
      begin_construction ()
      >>=? fun validation_state ->
      List.fold_left_es
        (fun rstate (shell, protocol_data) ->
          simulate_operation rstate E.Protocol.{shell; protocol_data})
        (validation_state, [])
        operations
      >>=? fun (validation_state, _) ->
      E.Protocol.finalize_block validation_state >>=? fun _ -> return_true

  let inject_operation_with_mempool operation_bytes =
    match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
    | Error _ ->
        RPC_answer.fail [Cannot_parse_op]
    | Ok ({Operation.shell = shell_header; proto} as op) -> (
        let operation_hash = Operation.hash op in
        let proto_op_opt =
          Data_encoding.Binary.of_bytes
            E.Protocol.operation_data_encoding
            proto
        in
        match proto_op_opt with
        | Error _ ->
            RPC_answer.fail [Cannot_parse_op]
        | Ok operation_data -> (
            need_operation shell_header operation_data
            >>=? (function
                   | true ->
                       Mempool.append [(shell_header, operation_data)]
                   | false ->
                       L.(S.emit warn_mempool_mem) ()
                       >>= fun _ ->
                       Trashpool.append [(shell_header, operation_data)])
            >>= function
            | Ok _ ->
                RPC_answer.return operation_hash
            | Error errs -> (
                Trashpool.append [(shell_header, operation_data)]
                >>= function
                | Ok _ ->
                    RPC_answer.fail errs
                | Error errs2 ->
                    RPC_answer.fail (errs @ errs2) ) ) )

  let inject_operation_without_mempool
      (write_context_callback :
        Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t)
      operation_bytes =
    match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
    | Error _ ->
        RPC_answer.fail [Cannot_parse_op]
    | Ok ({Operation.shell = shell_header; proto} as op) -> (
        let operation_hash = Operation.hash op in
        let proto_op_opt =
          Data_encoding.Binary.of_bytes
            E.Protocol.operation_data_encoding
            proto
        in
        match proto_op_opt with
        | Error _ ->
            RPC_answer.fail [Cannot_parse_op]
        | Ok operation_data -> (
            let op =
              {E.Protocol.shell = shell_header; protocol_data = operation_data}
            in
            begin_construction ()
            >>=? (fun state ->
                   E.Protocol.apply_operation state op
                   >>=? fun (state, receipt) ->
                   E.Protocol.finalize_block state
                   >>=? fun (validation_result, _block_header_metadata) ->
                   return (validation_result, receipt))
            >>= fun result ->
            match result with
            | Ok ({context; _}, _receipt) ->
                let rpc_context = {E.rpc_context with context} in
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
  let inject_block
      (write_context_callback :
        Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
    let reconstruct (operations : Operation.t list list)
        (block_header : Block_header.t) =
      let header = E.rpc_context.block_header in
      let predecessor_context = E.rpc_context.context in
      E.Protocol.begin_application
        ~chain_id:E.chain_id
        ~predecessor_context
        ~predecessor_timestamp:header.timestamp
        ~predecessor_fitness:header.fitness
        {
          shell = block_header.shell;
          protocol_data =
            Data_encoding.Binary.of_bytes_exn
              E.Protocol.block_header_data_encoding
              block_header.protocol_data;
        }
      >>=? fun validation_state ->
      let i = ref 0 in
      List.fold_left_es
        (List.fold_left_es (fun (validation_state, _results) op ->
             incr i ;
             match
               Data_encoding.Binary.of_bytes
                 op_data_encoding
                 op.Operation.proto
             with
             | Error _ ->
                 failwith "Cannot parse"
             | Ok operation_data ->
                 let op =
                   {
                     E.Protocol.shell = op.shell;
                     protocol_data = operation_data;
                   }
                 in
                 E.Protocol.apply_operation validation_state op
                 >>=? fun (validation_state, _receipt) ->
                 return (validation_state, _receipt :: _results)))
        (validation_state, [])
        operations
      >>=? fun (validation_state, _) ->
      E.Protocol.finalize_block validation_state
    in
    Directory.register
      Directory.empty
      (* /injection/block *)
      Tezos_shell_services.Injection_services.S.block
      (* See injection_directory.ml for vanilla implementation *)
      (fun () _ (bytes, operations) ->
        (* assert (Files.Mempool.exists ~dirname:E.base_dir) ; *)
        let block_hash = Block_hash.hash_bytes [bytes] in
        match Block_header.of_bytes bytes with
        | None ->
            RPC_answer.fail [Cannot_parse_op]
        | Some block_header -> (
            reconstruct operations block_header
            >>=? (fun ({context; _}, _) ->
                   let rpc_context =
                     Tezos_protocol_environment.
                       {
                         context;
                         block_hash;
                         block_header =
                           (* block_header.shell has been carefully constructed in
                            * preapply_block. *)
                           block_header.shell;
                       }
                   in
                   write_context_callback rpc_context
                   >>=? fun () ->
                   Mempool.read ()
                   >>=? fun mempool_operations ->
                   List.fold_left_es
                     (fun map ((shell_header, operation_data) as v) ->
                       match
                         Data_encoding.Binary.to_bytes
                           op_data_encoding
                           operation_data
                       with
                       | Error _ ->
                           failwith
                             "mockup inject block: byte encoding operation \
                              failed"
                       | Ok proto ->
                           let h =
                             Operation.hash
                               {Operation.shell = shell_header; proto}
                           in
                           return @@ Operation_hash.Map.add h v map)
                     Operation_hash.Map.empty
                     mempool_operations
                   >>=? fun mempool_map ->
                   let refused_map =
                     List.fold_left
                       (List.fold_left (fun mempool op ->
                            Operation_hash.Map.remove
                              (Operation.hash op)
                              mempool))
                       mempool_map
                       operations
                   in
                   unless (Operation_hash.Map.is_empty refused_map) (fun () ->
                       let refused_ops =
                         Operation_hash.Map.fold
                           (fun _k v l -> v :: l)
                           refused_map
                           []
                       in
                       L.(S.emit warn_trashpool_append) refused_ops
                       >>= fun () -> Trashpool.append refused_ops)
                   >>=? fun () -> Mempool.write ~mode:Zero_truncate [])
            >>= function
            | Error errs ->
                RPC_answer.fail errs
            | Ok () ->
                RPC_answer.return block_hash ))

  let inject_operation (mem_only : bool)
      (write_context_callback :
        Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
    Directory.register
      Directory.empty
      (* /injection/operation, vanilla client implementation is in
      injection_directory.ml *)
      Tezos_shell_services.Injection_services.S.operation
      (fun _q _contents operation_bytes ->
        if mem_only then RPC_answer.fail [Injection_not_possible]
        else
          (* Looking at the implementations of the two inject_operation_*
           functions it looks like there is code to share (proto_op_opt,
           operation_data), but it's not that easy to do;
           because types of concerned variables depend on E,
           which cannot cross functions boundaries without putting all that in
           MOCKUP *)
          Files.Mempool.exists ~dirname:E.base_dir
          >>= function
          | true ->
              inject_operation_with_mempool operation_bytes
          | false ->
              inject_operation_without_mempool
                write_context_callback
                operation_bytes)

  let build_shell_directory (mem_only : bool)
      (write_context_callback :
        Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
    let merge = Directory.merge in
    Directory.empty
    |> merge (p2p ())
    |> merge (chain ())
    |> merge (shell_header ())
    |> merge (monitor ())
    |> merge (protocols E.Protocol.hash)
    |> merge (block_hash ())
    |> merge (preapply ())
    |> merge (pending_operations ())
    |> merge (inject_operation mem_only write_context_callback)
    |> merge (inject_block write_context_callback)
    |> merge (live_blocks ())
    |> merge (preapply_block ())
end

let build_shell_directory (base_dir : string)
    (mockup_env : Registration.mockup_environment) chain_id
    (rpc_context : Tezos_protocol_environment.rpc_context) (mem_only : bool)
    (write_context_callback :
      Tezos_protocol_environment.rpc_context -> unit tzresult Lwt.t) =
  let (module Mockup_environment) = mockup_env in
  let module M = Make (struct
    include Mockup_environment

    let chain_id = chain_id

    let base_dir = base_dir

    let rpc_context = rpc_context
  end) in
  M.build_shell_directory mem_only write_context_callback

(** The directory of RPCs that the mockup client honors. Parameters are:

    [mem_only] specifies whether the mockup uses a persistent state.
    [mockup_env] is the implementation provided by the protocol.
    [chain_id] is the only chain that the mockup honors.
    [rpc_context] is data used when honoring an RPC.
 *)
let build_directory (base_dir : string) (mem_only : bool)
    (mockup_env : Registration.mockup_environment) (chain_id : Chain_id.t)
    (rpc_context : Tezos_protocol_environment.rpc_context) :
    unit RPC_directory.t =
  let write_context rpc_context =
    let (module Mockup_environment) = mockup_env in
    Persistence.overwrite_mockup
      ~chain_id
      ~protocol_hash:Mockup_environment.protocol_hash
      ~rpc_context
      ~base_dir
  in
  let (module Mockup_environment) = mockup_env in
  let proto_directory =
    (* register protocol-specific RPCs *)
    Directory.prefix
      Tezos_shell_services.Chain_services.path
      (Directory.prefix
         Tezos_shell_services.Block_services.path
         (Directory.map
            (fun (_chain, _block) -> Lwt.return rpc_context)
            Mockup_environment.directory))
  in
  let shell_directory =
    let (module Mockup_environment) = mockup_env in
    build_shell_directory
      base_dir
      mockup_env
      chain_id
      rpc_context
      mem_only
      write_context
  in
  let base = Directory.merge shell_directory proto_directory in
  RPC_directory.register_describe_directory_service
    base
    RPC_service.description_service
