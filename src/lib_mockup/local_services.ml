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

module Directory = Resto_directory.Make (Tezos_rpc.Encoding)
open Tezos_shell_services

type error += Injection_not_possible

type error += Cannot_parse_op

type error += Cannot_parse_proto_data

type callback_writer =
  Tezos_protocol_environment.rpc_context -> bytes -> unit tzresult Lwt.t

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
    (fun () -> Cannot_parse_op)

let () =
  register_error_kind
    `Temporary
    ~id:"local_services.Cannot_parse_proto_data"
    ~title:"Cannot_parse_proto_data"
    ~description:"Cannot parse protocol data"
    ~pp:(fun ppf () -> Format.pp_print_string ppf "Cannot parse protocol data.")
    Data_encoding.unit
    (function Cannot_parse_proto_data -> Some () | _ -> None)
    (fun () -> Cannot_parse_proto_data)

(* Since we bypass the node but still use the RPC mechanism for procedure
   calls, we have to register some RPCs ourselves. *)

(* [MENV] is a thin extension of [Registration.MOCKUP] comprising some
 * parameters used in most functions. *)
module type MENV = sig
  include Registration.MOCKUP

  val chain_id : Chain_id.t

  val rpc_context : Tezos_protocol_environment.rpc_context

  val base_dir : string

  val protocol_data : bytes
end

module Make (E : MENV) = struct
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

  let chain () =
    Directory.prefix
      Tezos_shell_services.Chain_services.path
      (Directory.register
         Directory.empty
         Tezos_shell_services.Chain_services.S.chain_id
         (fun _ () () -> Tezos_rpc.Answer.return E.chain_id))

  let protocols protocol_hash =
    let path =
      let open Tezos_rpc.Path in
      prefix Block_services.chain_path Block_services.path
    in
    let service =
      Tezos_rpc.Service.prefix path Block_services.Empty.S.protocols
    in
    Directory.register Directory.empty service (fun _prefix () () ->
        let current_protocol =
          if Compare.Int32.(E.rpc_context.block_header.level = 0l) then
            Protocol_hash.zero
          else protocol_hash
        in
        Lwt.return
          (`Ok {Block_services.current_protocol; next_protocol = protocol_hash}))

  let monitor () =
    let open Tezos_protocol_environment in
    let {block_hash; block_header; _} = E.rpc_context in
    Tezos_rpc.Directory.gen_register
      Directory.empty
      Monitor_services.S.bootstrapped
      (fun () () () ->
        Tezos_rpc.Answer.return (block_hash, block_header.timestamp))

  let chain_chain_id = function
    | `Main -> Chain_id.hash_string ["main"]
    | `Test -> Chain_id.hash_string ["test"]
    | `Hash cid -> cid

  let check_chain ?caller_name (chain : Block_services.chain) =
    unless
      (Chain_id.equal E.chain_id (chain_chain_id chain))
      (fun () ->
        let msg =
          let open Format in
          asprintf
            "Mismatched chain id %a: got %a but this mockup client expected %a."
            (Format.pp_print_option (fun ppf v -> Format.fprintf ppf "(%s)" v))
            caller_name
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
              | `Hash chain_id -> Chain_id.pp ppf chain_id)
            chain
            Chain_id.pp
            E.chain_id
        in
        Lwt.fail_with msg)

  let proto_data_bytes_to_block_header_opt () =
    Data_encoding.Binary.of_bytes_opt
      E.Protocol.block_header_data_encoding
      E.protocol_data

  let begin_validation_and_application ctxt chain_id mode ~predecessor ~cache =
    let open Lwt_result_syntax in
    let* validation_state =
      E.Protocol.begin_validation ctxt chain_id mode ~predecessor ~cache
    in
    let* application_state =
      E.Protocol.begin_application ctxt chain_id mode ~predecessor ~cache
    in
    return (validation_state, application_state)

  let validate_and_apply_operation (validation_state, application_state) oph op
      =
    let open Lwt_result_syntax in
    let* validation_state =
      E.Protocol.validate_operation validation_state oph op
    in
    let* application_state, receipt =
      E.Protocol.apply_operation application_state oph op
    in
    return ((validation_state, application_state), receipt)

  let finalize_validation_and_application (validation_state, application_state)
      shell_header =
    let open Lwt_result_syntax in
    let* () = E.Protocol.finalize_validation validation_state in
    E.Protocol.finalize_application application_state shell_header

  let partial_construction ~cache () =
    let predecessor_hash = E.rpc_context.block_hash in
    let predecessor = E.rpc_context.block_header in
    let predecessor_context = E.rpc_context.context in
    let timestamp = Time.System.to_protocol @@ Tezos_base.Time.System.now () in
    begin_validation_and_application
      predecessor_context
      E.chain_id
      (Partial_construction {predecessor_hash; timestamp})
      ~predecessor
      ~cache

  let full_construction ?timestamp ~protocol_data ~cache () =
    let predecessor_hash = E.rpc_context.block_hash in
    let predecessor = E.rpc_context.block_header in
    let predecessor_context = E.rpc_context.context in
    let timestamp =
      let default () =
        Time.System.to_protocol @@ Tezos_base.Time.System.now ()
      in
      Option.value_f timestamp ~default
    in
    begin_validation_and_application
      predecessor_context
      E.chain_id
      (Construction
         {predecessor_hash; timestamp; block_header_data = protocol_data})
      ~predecessor
      ~cache

  let op_encoding =
    Data_encoding.(
      dynamic_size
      @@ obj2
           (req "shell_header" Operation.shell_header_encoding)
           (req "protocol_data" E.Protocol.operation_data_encoding))

  let ops_encoding = Data_encoding.Variable.list op_encoding

  module L = struct
    module S = Internal_event.Simple

    let section = ["mockup"; "local_services"]

    let warn_trashpool_append =
      let pp1 ppf l =
        match List.length l with
        (* This should never happen as the lone call to this function is
           protected by a "unless" *)
        | 0 -> Format.pp_print_string ppf "nothing"
        | 1 -> Format.pp_print_string ppf "1 operation"
        | n -> Format.fprintf ppf "%d operations" n
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

    let warn msg =
      S.declare_0
        ~section
        ~name:(Printf.sprintf "local_services_warn_%s" msg)
        ~msg:(Printf.sprintf "warning: %s" msg)
        ~level:Internal_event.Warning
        ()
  end

  type write_mode = Append | Zero_truncate

  module Rw (File_accessor : Files.ACCESSOR) = struct
    let file = (File_accessor.get ~dirname:E.base_dir :> string)

    let unsafe_read () =
      let open Lwt_result_syntax in
      let* json = Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file in
      return @@ Data_encoding.Json.destruct ops_encoding json

    let read () =
      let open Lwt_syntax in
      let* b = File_accessor.exists ~dirname:E.base_dir in
      match b with true -> unsafe_read () | false -> return_ok []

    let write ~mode operations =
      let open Lwt_result_syntax in
      let* ops =
        match mode with
        | Append ->
            let* ops = read () in
            return (ops @ operations)
        | Zero_truncate -> return operations
      in
      let json = Data_encoding.Json.construct ops_encoding ops in
      Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file file json

    let append = write ~mode:Append
  end

  module Mempool = Rw (Files.Mempool)
  module Trashpool = Rw (Files.Trashpool)

  let to_validated (shell_header, operation_data) =
    let open Lwt_result_syntax in
    let op =
      {E.Protocol.shell = shell_header; protocol_data = operation_data}
    in
    match
      Data_encoding.Binary.to_bytes
        E.Protocol.operation_data_encoding
        operation_data
    with
    | Error _ -> failwith "mockup pending_operations"
    | Ok proto ->
        let operation_hash =
          Operation.hash {Operation.shell = shell_header; proto}
        in
        return (operation_hash, op)

  let with_chain ?caller_name chain k =
    let open Lwt_syntax in
    let* r = check_chain ?caller_name chain in
    match r with Error errs -> Tezos_rpc.Answer.fail errs | Ok () -> k ()

  let pending_operations () =
    let open Lwt_result_syntax in
    Directory.register
      Directory.empty
      (* /chains/<chain_id>/mempool/pending_operations *)
      (E.Block_services.S.Mempool.pending_operations
      @@ Block_services.mempool_path Block_services.chain_path)
      (fun ((), chain) params () ->
        let*! pending_operations =
          let* () = check_chain chain in
          let* pooled_operations = Mempool.read () in
          let* validated = List.map_es to_validated pooled_operations in
          let pending_operations =
            {
              E.Block_services.Mempool.validated;
              refused = Operation_hash.Map.empty;
              outdated = Operation_hash.Map.empty;
              branch_refused = Operation_hash.Map.empty;
              branch_delayed = Operation_hash.Map.empty;
              unprocessed = Operation_hash.Map.empty;
            }
          in
          return pending_operations
        in
        match pending_operations with
        | Error errs -> Tezos_rpc.Answer.fail errs
        | Ok pending_operations ->
            Tezos_rpc.Answer.return (params#version, pending_operations))

  let shell_header () =
    Directory.prefix
      (Tezos_rpc.Path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.Header.shell_header
         (fun _prefix () () ->
           Tezos_rpc.Answer.return E.rpc_context.block_header)

  let block_hash () =
    let path =
      let open Tezos_rpc.Path in
      prefix Block_services.chain_path Block_services.path
    in
    let service = Tezos_rpc.Service.prefix path Block_services.Empty.S.hash in
    (* Always return the head. *)
    Directory.register Directory.empty service (fun _prefix () () ->
        Tezos_rpc.Answer.return E.rpc_context.block_hash)

  let live_blocks () =
    Directory.prefix
      (Tezos_rpc.Path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.live_blocks
         (fun (((), chain), _block) () () ->
           with_chain ~caller_name:"live blocks" chain (fun () ->
               let set = Block_hash.Set.singleton E.rpc_context.block_hash in
               Tezos_rpc.Answer.return set))

  let simulate_operation (state, preapply_result) op =
    let open Lwt_result_syntax in
    match
      Data_encoding.Binary.to_bytes
        E.Protocol.operation_data_encoding
        op.E.Protocol.protocol_data
    with
    | Error _ -> failwith "mockup preapply_block: cannot deserialize operation"
    | Ok proto -> (
        let op_t = {Operation.shell = op.shell; proto} in
        let hash = Operation.hash op_t in
        let*! r = validate_and_apply_operation state hash op in
        match r with
        | Error e ->
            let open Preapply_result in
            return
              ( state,
                {
                  preapply_result with
                  refused =
                    Operation_hash.Map.add
                      hash
                      (op_t, e)
                      preapply_result.refused;
                } )
        | Ok (state, _) ->
            let open Preapply_result in
            return
              ( state,
                {
                  preapply_result with
                  applied = (hash, op_t) :: preapply_result.applied;
                } ))

  let preapply_block () =
    let open Lwt_result_syntax in
    Directory.prefix
      (Tezos_rpc.Path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.Helpers.Preapply.block
         (fun (((), chain), _block) o {operations; protocol_data} ->
           with_chain ~caller_name:"preapply_block" chain (fun () ->
               let*! r =
                 let timestamp = o#timestamp in
                 let* proto_state =
                   full_construction
                     ~cache:`Lazy
                     ?timestamp:o#timestamp
                     ~protocol_data
                     ()
                 in
                 let* validation_passes, proto_state, preapply_results =
                   List.fold_left_es
                     (fun (validation_passes, proto_state, validation_result)
                          operations
                        ->
                       let* state, result =
                         List.fold_left_es
                           simulate_operation
                           (proto_state, Preapply_result.empty)
                           operations
                       in
                       let open Preapply_result in
                       let p_result =
                         {result with applied = List.rev result.applied}
                       in
                       return
                         ( succ validation_passes,
                           state,
                           p_result :: validation_result ))
                     (0, proto_state, [])
                     operations
                 in
                 let* validation_result, _metadata =
                   finalize_validation_and_application
                     proto_state
                     (Some E.rpc_context.block_header)
                 in
                 (* Similar to lib_shell.Prevalidation.preapply *)
                 let operations_hash =
                   let open Preapply_result in
                   Operation_list_list_hash.compute
                   @@ List.rev_map
                        (fun x ->
                          Operation_list_hash.compute @@ List.map fst x.applied)
                        preapply_results
                 in
                 let timestamp =
                   Option.value_f
                     ~default:(fun () ->
                       Time.System.to_protocol (Time.System.now ()))
                     timestamp
                 in
                 let shell_header =
                   {
                     E.rpc_context.block_header with
                     level = Int32.succ E.rpc_context.block_header.level;
                     (* proto_level should be unchanged in mockup mode
                        since we cannot switch protocols *)
                     predecessor = E.rpc_context.block_hash;
                     timestamp
                     (* The timestamp exists if --minimal-timestamp has
                        been given on the command line *);
                     operations_hash;
                     validation_passes;
                     fitness = validation_result.fitness;
                     context = Context_hash.zero (* TODO: is that correct ? *);
                   }
                 in
                 return (shell_header, List.rev preapply_results)
               in
               match r with
               | Error errs -> Tezos_rpc.Answer.fail errs
               | Ok v -> Tezos_rpc.Answer.return v))

  let hash_protocol_operation op =
    match
      Data_encoding.Binary.to_bytes
        E.Protocol.operation_data_encoding
        op.E.Protocol.protocol_data
    with
    | Error _ ->
        failwith "mockup preapply_operations: cannot deserialize operation"
    | Ok proto ->
        let op_t = {Operation.shell = op.shell; proto} in
        Lwt_result.return (Operation.hash op_t)

  let preapply () =
    let open Lwt_result_syntax in
    Directory.prefix
      (Tezos_rpc.Path.prefix
         (* /chains/<chain> *)
         Tezos_shell_services.Chain_services.path
         (* blocks/<block_id> *)
         Block_services.path)
      (Directory.register
         Directory.empty
         (* /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations *)
         E.Block_services.S.Helpers.Preapply.operations
         (fun ((_, chain), _block) params op_list ->
           with_chain ~caller_name:"preapply operations" chain (fun () ->
               let*! outcome =
                 let* proto_state = partial_construction ~cache:`Lazy () in
                 let* proto_state, acc =
                   List.fold_left_es
                     (fun (proto_state, acc) op ->
                       let* oph = hash_protocol_operation op in
                       let* proto_state, result =
                         validate_and_apply_operation proto_state oph op
                       in
                       return (proto_state, (op.protocol_data, result) :: acc))
                     (proto_state, [])
                     op_list
                 in
                 (* A pre-application should not commit into the
                    protocol caches. For this reason, [cache_nonce]
                    is [None]. *)
                 let* _ =
                   finalize_validation_and_application proto_state None
                 in
                 return (List.rev acc)
               in
               match outcome with
               | Ok result -> Tezos_rpc.Answer.return (params#version, result)
               | Error errs -> Tezos_rpc.Answer.fail errs)))

  let hash_op (shell, proto) =
    let proto =
      Data_encoding.Binary.to_bytes_exn E.Protocol.operation_data_encoding proto
    in
    Operation.hash {shell; proto}

  let equal_op (a_shell_header, a_operation_data)
      (b_shell_header, b_operation_data) =
    Block_hash.equal
      a_shell_header.Operation.branch
      b_shell_header.Operation.branch
    &&
    (* FIXME: the protocol should export equality/comparison functions for
          its abstract types such as operation_data.  WARNING: the following
          expression causes an exception to be raised, complaining about
          functional values Stdlib.( = ) a_operation_data b_operation_data *)
    Stdlib.compare a_operation_data b_operation_data = 0

  let need_operation op =
    let open Lwt_result_syntax in
    let* mempool_operations = Mempool.read () in
    if List.mem ~equal:equal_op op mempool_operations then return `Equal
    else
      let operations = op :: mempool_operations in
      let* proto_state = partial_construction ~cache:`Lazy () in
      let* proto_state, preapply_result =
        List.fold_left_es
          (fun rstate (shell, protocol_data) ->
            simulate_operation rstate E.Protocol.{shell; protocol_data})
          (proto_state, Preapply_result.empty)
          operations
      in
      if Operation_hash.Map.is_empty preapply_result.refused then
        let* _ = finalize_validation_and_application proto_state None in
        return `Applicable
      else return `Refused

  let append_to_thraspool ~notification_msg op =
    let open Lwt_result_syntax in
    let* () = Trashpool.append [op] in
    failwith "%s" notification_msg

  let inject_operation_with_mempool operation_bytes =
    let open Lwt_result_syntax in
    match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
    | Error _ -> Tezos_rpc.Answer.fail [Cannot_parse_op]
    | Ok ({Operation.shell = shell_header; proto} as op) -> (
        let operation_hash = Operation.hash op in
        let proto_op_opt =
          Data_encoding.Binary.of_bytes E.Protocol.operation_data_encoding proto
        in
        match proto_op_opt with
        | Error _ -> Tezos_rpc.Answer.fail [Cannot_parse_op]
        | Ok operation_data -> (
            let op = (shell_header, operation_data) in
            let*! r =
              let* n = need_operation op in
              match n with
              | `Applicable -> Mempool.append [op]
              | `Equal ->
                  let*! () = L.(S.emit warn_mempool_mem) () in
                  append_to_thraspool
                    ~notification_msg:"Last operation is a duplicate"
                    op
              | `Refused ->
                  append_to_thraspool
                    ~notification_msg:"Last operation is refused"
                    op
            in
            match r with
            | Ok _ -> Tezos_rpc.Answer.return operation_hash
            | Error errs -> (
                let*! r = Trashpool.append [op] in
                match r with
                | Ok _ -> Tezos_rpc.Answer.fail errs
                | Error errs2 -> Tezos_rpc.Answer.fail (errs @ errs2))))

  let inject_operation_without_mempool
      (write_context_callback : callback_writer) operation_bytes =
    let open Lwt_result_syntax in
    match Data_encoding.Binary.of_bytes Operation.encoding operation_bytes with
    | Error _ -> Tezos_rpc.Answer.fail [Cannot_parse_op]
    | Ok ({Operation.shell = shell_header; proto} as op) -> (
        let operation_hash = Operation.hash op in
        let proto_op_opt =
          Data_encoding.Binary.of_bytes E.Protocol.operation_data_encoding proto
        in
        match proto_op_opt with
        | Error _ -> Tezos_rpc.Answer.fail [Cannot_parse_op]
        | Ok operation_data -> (
            let op =
              {E.Protocol.shell = shell_header; protocol_data = operation_data}
            in
            let*! result =
              let* proto_state = partial_construction ~cache:`Lazy () in
              let* proto_state, receipt =
                validate_and_apply_operation proto_state operation_hash op
              in
              let* validation_result, _block_header_metadata =
                finalize_validation_and_application proto_state None
              in
              return (validation_result, receipt)
            in
            match result with
            | Ok ({context; _}, _receipt) -> (
                let rpc_context = {E.rpc_context with context} in
                let*! result = write_context_callback rpc_context proto in
                match result with
                | Ok () -> Tezos_rpc.Answer.return operation_hash
                | Error errs -> Tezos_rpc.Answer.fail errs)
            | Error errs -> Tezos_rpc.Answer.fail errs))

  let inject_block_generic (write_context_callback : callback_writer)
      (update_mempool_callback : Operation.t list list -> unit tzresult Lwt.t) =
    let open Lwt_result_syntax in
    let reconstruct (operations : Operation.t list list)
        (block_header : Block_header.t) =
      match
        Data_encoding.Binary.of_bytes_opt
          E.Protocol.block_header_data_encoding
          block_header.protocol_data
      with
      | None -> assert false
      | Some protocol_data ->
          let predecessor = E.rpc_context.block_header in
          let predecessor_context = E.rpc_context.context in
          let mode =
            E.Protocol.Application {shell = block_header.shell; protocol_data}
          in
          let* proto_state =
            begin_validation_and_application
              predecessor_context
              E.chain_id
              mode
              ~predecessor
              ~cache:`Lazy
          in
          let* proto_state, _ =
            List.fold_left_es
              (List.fold_left_es (fun (proto_state, results) op ->
                   match
                     Data_encoding.Binary.of_bytes
                       E.Protocol.operation_data_encoding
                       op.Operation.proto
                   with
                   | Error _ -> failwith "Cannot parse"
                   | Ok operation_data ->
                       let oph = Operation.hash op in
                       let op =
                         {
                           E.Protocol.shell = op.shell;
                           protocol_data = operation_data;
                         }
                       in
                       let* proto_state, receipt =
                         validate_and_apply_operation proto_state oph op
                       in
                       return (proto_state, receipt :: results)))
              (proto_state, [])
              operations
          in
          finalize_validation_and_application
            proto_state
            (Some block_header.shell)
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
        | None -> Tezos_rpc.Answer.fail [Cannot_parse_op]
        | Some block_header -> (
            let*! r =
              let* {context; _}, _ = reconstruct operations block_header in
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
              let* () =
                write_context_callback rpc_context block_header.protocol_data
              in
              update_mempool_callback operations
            in
            match r with
            | Error errs -> Tezos_rpc.Answer.fail errs
            | Ok () -> Tezos_rpc.Answer.return block_hash))

  (** [inject_block] is a feature that assumes that the mockup is on-disk
      and uses a mempool. *)
  let inject_block (write_context_callback : callback_writer) =
    inject_block_generic write_context_callback (fun operations ->
        let open Lwt_result_syntax in
        let* mempool_operations = Mempool.read () in
        let* mempool_map =
          List.fold_left_es
            (fun map ((shell_header, operation_data) as v) ->
              match
                Data_encoding.Binary.to_bytes
                  E.Protocol.operation_data_encoding
                  operation_data
              with
              | Error _ ->
                  failwith "mockup inject block: byte encoding operation failed"
              | Ok proto ->
                  let h =
                    Operation.hash {Operation.shell = shell_header; proto}
                  in
                  return @@ Operation_hash.Map.add h v map)
            Operation_hash.Map.empty
            mempool_operations
        in
        let refused_map =
          List.fold_left
            (List.fold_left (fun mempool op ->
                 Operation_hash.Map.remove (Operation.hash op) mempool))
            mempool_map
            operations
        in
        let* () =
          unless (Operation_hash.Map.is_empty refused_map) (fun () ->
              let refused_ops =
                Operation_hash.Map.fold (fun _k v l -> v :: l) refused_map []
              in
              let*! () = L.(S.emit warn_trashpool_append) refused_ops in
              Trashpool.append refused_ops)
        in
        Mempool.write ~mode:Zero_truncate [])

  let inject_operation (mem_only : bool)
      (write_context_callback : callback_writer) =
    let open Lwt_syntax in
    Directory.register
      Directory.empty
      (* /injection/operation, vanilla client implementation is in
         injection_directory.ml *)
      Tezos_shell_services.Injection_services.S.operation
      (fun _q _contents operation_bytes ->
        if mem_only then Tezos_rpc.Answer.fail [Injection_not_possible]
        else
          (* Looking at the implementations of the two inject_operation_*
             functions it looks like there is code to share (proto_op_opt,
             operation_data), but it's not that easy to do;
             because types of concerned variables depend on E,
             which cannot cross functions boundaries without putting all that in
             MOCKUP *)
          let* b = Files.Mempool.exists ~dirname:E.base_dir in
          match b with
          | true -> inject_operation_with_mempool operation_bytes
          | false ->
              inject_operation_without_mempool
                write_context_callback
                operation_bytes)

  let monitor_validated_blocks () =
    Directory.register
      Directory.empty
      Tezos_shell_services.Monitor_services.S.validated_blocks
      (fun () q () ->
        let chain =
          match List.hd q#chains with None -> `Main | Some chain -> chain
        in
        with_chain ~caller_name:"monitor validated blocks" chain (fun () ->
            let block_header =
              Block_header.
                {
                  shell = E.rpc_context.block_header;
                  protocol_data = E.protocol_data;
                }
            in
            let block_hash = E.rpc_context.block_hash in
            Tezos_rpc.Answer.return
              (E.chain_id, block_hash, block_header, [[]; []; []; []])))

  let monitor_heads () =
    Directory.register
      Directory.empty
      Tezos_shell_services.Monitor_services.S.heads
      (fun ((), chain) _next_protocol () ->
        with_chain ~caller_name:"monitor heads" chain (fun () ->
            let block_header =
              Block_header.
                {
                  shell = E.rpc_context.block_header;
                  protocol_data = E.protocol_data;
                }
            in
            let block_hash = E.rpc_context.block_hash in
            Tezos_rpc.Answer.return (block_hash, block_header)))

  let raw_header () =
    Directory.prefix
      (Tezos_rpc.Path.prefix Chain_services.path Block_services.path)
      (Directory.register
         Directory.empty
         E.Block_services.S.raw_header
         (fun (((), chain), _block) () () ->
           with_chain ~caller_name:"raw header" chain (fun () ->
               let block_header_b =
                 Data_encoding.Binary.to_bytes_exn
                   Tezos_base.Block_header.encoding
                   {
                     shell = E.rpc_context.block_header;
                     protocol_data = E.protocol_data;
                   }
               in
               Tezos_rpc.Answer.return block_header_b)))

  let header () =
    Directory.prefix
      (Tezos_rpc.Path.prefix Chain_services.path Block_services.path)
      (Directory.register
         Directory.empty
         E.Block_services.S.header
         (fun (((), chain), _block) () () ->
           with_chain ~caller_name:"header" chain (fun () ->
               match proto_data_bytes_to_block_header_opt () with
               | None -> assert false
               | Some protocol_data ->
                   let block_header =
                     E.Block_services.
                       {
                         chain_id = E.chain_id;
                         hash = E.rpc_context.block_hash;
                         shell = E.rpc_context.block_header;
                         protocol_data;
                       }
                   in
                   Tezos_rpc.Answer.return block_header)))

  let resulting_context_hash () =
    Directory.prefix
      (Tezos_rpc.Path.prefix Chain_services.path Block_services.path)
      (Directory.register
         Directory.empty
         E.Block_services.S.resulting_context_hash
         (fun (((), chain), _block) () () ->
           with_chain ~caller_name:"resulting_context_hash" chain (fun () ->
               (* This is not sufficient but this library doesn't have
                  what's necessary to determine the correct value. *)
               Tezos_rpc.Answer.return E.rpc_context.block_header.context)))

  let protocol_data_raw () =
    Directory.prefix
      (Tezos_rpc.Path.prefix Chain_services.path Block_services.path)
      (Directory.register
         Directory.empty
         E.Block_services.S.Header.raw_protocol_data
         (fun (((), chain), _block) () () ->
           with_chain ~caller_name:"protocol_data_raw" chain (fun () ->
               Tezos_rpc.Answer.return E.protocol_data)))

  let operations () =
    Directory.prefix
      (Tezos_rpc.Path.prefix Chain_services.path Block_services.path)
    @@ Directory.register
         Directory.empty
         E.Block_services.S.Operations.operations
         (fun (((), chain), _block) query () ->
           with_chain ~caller_name:"operations" chain (fun () ->
               (* FIXME: Better answer here *)
               Tezos_rpc.Answer.return (query#version, [[]; []; []; []])))

  let monitor_operations () =
    let open Lwt_syntax in
    Directory.register
      Directory.empty
      (E.Block_services.S.Mempool.monitor_operations
      @@ Block_services.mempool_path Block_services.chain_path)
      (* FIXME: Return real operations from the mempool *)
      (fun (_, chain) o () ->
        with_chain ~caller_name:"monitor operations" chain (fun () ->
            let on b msg =
              if b then L.(S.emit (warn msg)) () else Lwt.return_unit
            in
            let* () = on o#branch_delayed "branch_delayed ignored" in
            let* () = on o#branch_refused "branch_refused ignored" in
            let* () = on o#refused "refused ignored" in
            let _ = o#validated in
            Tezos_rpc.Answer.(
              return_stream
                {next = (fun () -> Lwt.return_none); shutdown = (fun () -> ())})))

  let build_shell_directory (mem_only : bool)
      (write_context_callback : callback_writer) =
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
    |> merge (monitor_validated_blocks ())
    |> merge (monitor_heads ())
    |> merge (raw_header ())
    |> merge (header ())
    |> merge (operations ())
    |> merge (resulting_context_hash ())
    |> merge (protocol_data_raw ())
    |> merge (monitor_operations ())
end

let build_shell_directory (base_dir : string)
    (mockup_env : Registration.mockup_environment) chain_id
    (rpc_context : Tezos_protocol_environment.rpc_context)
    (protocol_data : bytes) (mem_only : bool)
    (write_context_callback : callback_writer) =
  let (module Mockup_environment) = mockup_env in
  let module M = Make (struct
    include Mockup_environment

    let chain_id = chain_id

    let base_dir = base_dir

    let rpc_context = rpc_context

    let protocol_data = protocol_data
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
    (rpc_context : Tezos_protocol_environment.rpc_context) protocol_data :
    unit Tezos_rpc.Directory.t =
  let write_context rpc_context protocol_data =
    let (module Mockup_environment) = mockup_env in
    Persistence.overwrite_mockup
      ~chain_id
      ~protocol_hash:Mockup_environment.protocol_hash
      ~protocol_data
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
      protocol_data
      mem_only
      write_context
  in
  let base = Directory.merge shell_directory proto_directory in
  Tezos_rpc.Directory.register_describe_directory_service
    base
    Tezos_rpc.Service.description_service
