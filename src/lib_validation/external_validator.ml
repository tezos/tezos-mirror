(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

module Events = struct
  open Internal_event.Simple

  let section = ["external_validator"]

  let initialized =
    declare_0
      ~section
      ~level:Info
      ~name:"initialized"
      ~msg:"validator initialized and listening"
      ()

  let terminated =
    declare_0
      ~section
      ~level:Info
      ~name:"terminated_request"
      ~msg:"validator terminated"
      ()

  let dynload_protocol =
    declare_1
      ~section
      ~level:Info
      ~name:"dynload_protocol"
      ~msg:"dynamic loading of protocol {protocol}"
      ~pp1:Protocol_hash.pp
      ("protocol", Protocol_hash.encoding)

  let validation_request =
    declare_1
      ~section
      ~level:Info
      ~name:"validation_request"
      ~msg:"validating block {block}"
      ~pp1:(fun fmt header -> Block_hash.pp fmt (Block_header.hash header))
      ("block", Block_header.encoding)

  let precheck_request =
    declare_1
      ~section
      ~level:Info
      ~name:"precheck_request"
      ~msg:"prechecking block {hash}"
      ~pp1:Block_hash.pp
      ("hash", Block_hash.encoding)

  let commit_genesis_request =
    declare_1
      ~section
      ~level:Info
      ~name:"commit_genesis_request"
      ~msg:"committing genesis block {genesis}"
      ~pp1:Block_hash.pp
      ("genesis", Block_hash.encoding)

  let initialization_request =
    declare_0
      ~section
      ~level:Info
      ~name:"initialization_request"
      ~msg:"initializing validator's environment"
      ()

  let fork_test_chain_request =
    declare_1
      ~section
      ~level:Info
      ~name:"fork_testchain_request"
      ~msg:"forking test chain at block {block}"
      ~pp1:Block_header.pp
      ("block", Block_header.encoding)

  let context_gc_request =
    declare_1
      ~section
      ~level:Info
      ~name:"context_gc_request"
      ~msg:"garbage collecting context below {context_hash}"
      ~pp1:Context_hash.pp
      ("context_hash", Context_hash.encoding)

  let context_split_request =
    declare_0
      ~section
      ~level:Info
      ~name:"context_split_request"
      ~msg:"spliting context"
      ()

  let termination_request =
    declare_0
      ~section
      ~level:Info
      ~name:"termination_request"
      ~msg:"validator terminated"
      ()

  let emit = Internal_event.Simple.emit
end

open Filename.Infix

let load_protocol proto protocol_root =
  let open Lwt_result_syntax in
  if Registered_protocol.mem proto then return_unit
  else
    let cmxs_file =
      protocol_root
      // Protocol_hash.to_short_b58check proto
      // Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp proto
    in
    let*! () = Events.(emit dynload_protocol proto) in
    match Dynlink.loadfile_private cmxs_file with
    | () -> return_unit
    | exception Dynlink.Error err ->
        Format.ksprintf
          (fun msg ->
            tzfail
              Block_validator_errors.(
                Validation_process_failed (Protocol_dynlink_failure msg)))
          "Cannot load file: %s. (Expected location: %s.)"
          (Dynlink.error_message err)
          cmxs_file

let with_retry_to_load_protocol protocol_root f =
  let open Lwt_syntax in
  let* r = f () in
  match r with
  | Error [Block_validator_errors.Unavailable_protocol {protocol; _}] as
    original_error -> (
      (* If `next_protocol` is missing, try to load it *)
      let* r = load_protocol protocol protocol_root in
      match r with Error _ -> Lwt.return original_error | Ok () -> f ())
  | _ -> Lwt.return r

let inconsistent_handshake msg =
  Block_validator_errors.(
    Validation_process_failed (Inconsistent_handshake msg))

(* Handshake with the node. See
   [Block_validator_process.process_handshake] for the handshake
   scenario. *)
let handshake input output =
  let open Lwt_syntax in
  let* () =
    External_validation.send
      output
      Data_encoding.Variable.bytes
      External_validation.magic
  in
  let* magic = External_validation.recv input Data_encoding.Variable.bytes in
  fail_when
    (not (Bytes.equal magic External_validation.magic))
    (inconsistent_handshake "bad magic")

(* Initialization of the external process thanks to the parameters
   sent by the node. This is expected to be run after the
   [handshake]. See [Block_validator_process.process_init] for
   the init scenario. *)
let init ~readonly input output =
  let open Lwt_result_syntax in
  let*! () = Events.(emit initialization_request ()) in
  let*! {
          context_root;
          protocol_root;
          genesis;
          sandbox_parameters;
          user_activated_upgrades;
          user_activated_protocol_overrides;
          operation_metadata_size_limit;
          dal_config;
          internal_events;
        } =
    External_validation.recv input External_validation.parameters_encoding
  in
  let* () = Tezos_crypto_dal.Cryptobox.Config.init_verifier_dal dal_config in
  let sandbox_parameters =
    Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
  in
  let*! context_index =
    Context.init
      ~patch_context:(fun ctxt ->
        let open Lwt_result_syntax in
        let ctxt = Shell_context.wrap_disk_context ctxt in
        let+ ctxt =
          Patch_context.patch_context genesis sandbox_parameters ctxt
        in
        Shell_context.unwrap_disk_context ctxt)
      ~readonly
      context_root
  in
  (* It is necessary to send the ok result, as a blocking promise for
     the node (see [Block_validator_process.process_init]), after a
     complete initialization as the node relies on the external
     validator for the context's initialization. *)
  let*! () =
    External_validation.send
      output
      (Error_monad.result_encoding Data_encoding.empty)
      (Ok ())
  in
  return
    ( ( context_index,
        protocol_root,
        genesis,
        user_activated_upgrades,
        user_activated_protocol_overrides,
        operation_metadata_size_limit ),
      internal_events )

let handle_request :
    type a.
    _ ->
    Context_ops.Environment_context.block_cache option ->
    (Block_validation.apply_result * Context_ops.Environment_context.t) option ->
    a External_validation.request ->
    [ `Continue of
      a tzresult
      * Context_ops.Environment_context.block_cache option
      * (Block_validation.apply_result * Context_ops.Environment_context.t)
        option
    | `Stop ]
    Lwt.t =
 fun ( context_index,
       protocol_root,
       genesis,
       user_activated_upgrades,
       user_activated_protocol_overrides,
       operation_metadata_size_limit )
     cache
     cached_result ->
  let open Lwt_result_syntax in
  let continue res cache cached_result =
    Lwt.return (`Continue (res, cache, cached_result))
  in
  function
  | Commit_genesis {chain_id} ->
      let*! () = Events.(emit commit_genesis_request genesis.Genesis.block) in
      let*! commit =
        Error_monad.catch_es (fun () ->
            Context.commit_genesis
              context_index
              ~chain_id
              ~time:genesis.time
              ~protocol:genesis.protocol)
      in
      continue commit cache None
  | Validate
      {
        chain_id;
        block_header;
        predecessor_block_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        predecessor_resulting_context_hash;
        operations;
        max_operations_ttl;
        should_precheck;
        simulate;
      } ->
      let*! () = Events.(emit validation_request block_header) in
      let*! block_application_result =
        let* predecessor_context =
          Error_monad.catch_es (fun () ->
              let*! o =
                Context.checkout
                  context_index
                  predecessor_resulting_context_hash
              in
              match o with
              | Some c -> return (Shell_context.wrap_disk_context c)
              | None ->
                  tzfail
                    (Block_validator_errors.Failed_to_checkout_context
                       predecessor_resulting_context_hash))
        in
        let*! protocol_hash = Context_ops.get_protocol predecessor_context in
        let* () = load_protocol protocol_hash protocol_root in
        let env =
          {
            Block_validation.chain_id;
            user_activated_upgrades;
            user_activated_protocol_overrides;
            operation_metadata_size_limit;
            max_operations_ttl;
            predecessor_block_header;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
            predecessor_context;
            predecessor_resulting_context_hash;
          }
        in
        let cache =
          match cache with
          | None -> `Load
          | Some cache -> `Inherited (cache, predecessor_resulting_context_hash)
        in
        with_retry_to_load_protocol protocol_root (fun () ->
            Block_validation.apply
              ~simulate
              ?cached_result
              ~should_precheck
              env
              block_header
              operations
              ~cache)
      in
      let block_application_result, cache =
        match block_application_result with
        | Error _ as err -> (err, cache)
        | Ok {result; cache} ->
            ( Ok result,
              Some
                {
                  context_hash = result.validation_store.resulting_context_hash;
                  cache;
                } )
      in
      continue block_application_result cache None
  | Preapply
      {
        chain_id;
        timestamp;
        protocol_data;
        live_blocks;
        live_operations;
        predecessor_shell_header;
        predecessor_hash;
        predecessor_max_operations_ttl;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        predecessor_resulting_context_hash;
        operations;
      } ->
      let*! block_preapplication_result =
        let* predecessor_context =
          Error_monad.catch_es (fun () ->
              let*! context =
                Context.checkout
                  context_index
                  predecessor_resulting_context_hash
              in
              match context with
              | Some context -> return (Shell_context.wrap_disk_context context)
              | None ->
                  tzfail
                    (Block_validator_errors.Failed_to_checkout_context
                       predecessor_resulting_context_hash))
        in
        let*! protocol_hash = Context_ops.get_protocol predecessor_context in
        let* () = load_protocol protocol_hash protocol_root in
        with_retry_to_load_protocol protocol_root (fun () ->
            Block_validation.preapply
              ~chain_id
              ~user_activated_upgrades
              ~user_activated_protocol_overrides
              ~operation_metadata_size_limit
              ~timestamp
              ~protocol_data
              ~live_blocks
              ~live_operations
              ~predecessor_context
              ~predecessor_shell_header
              ~predecessor_hash
              ~predecessor_max_operations_ttl
              ~predecessor_block_metadata_hash
              ~predecessor_ops_metadata_hash
              ~predecessor_resulting_context_hash
              operations)
      in
      let*! res, cachable_result =
        match block_preapplication_result with
        | Ok (res, last_preapplied_context) ->
            Lwt.return (Ok res, Some last_preapplied_context)
        | Error _ as err -> Lwt.return (err, None)
      in
      continue res cache cachable_result
  | External_validation.Precheck
      {
        chain_id;
        predecessor_block_header;
        predecessor_block_hash;
        predecessor_resulting_context_hash;
        header;
        operations;
        hash;
      } ->
      let*! () = Events.(emit precheck_request hash) in
      let*! block_precheck_result =
        let* predecessor_context =
          Error_monad.catch_es (fun () ->
              let*! o =
                Context.checkout
                  context_index
                  predecessor_resulting_context_hash
              in
              match o with
              | Some context -> return (Shell_context.wrap_disk_context context)
              | None ->
                  tzfail
                    (Block_validator_errors.Failed_to_checkout_context
                       predecessor_resulting_context_hash))
        in
        let cache =
          match cache with
          | None -> `Lazy
          | Some cache -> `Inherited (cache, predecessor_resulting_context_hash)
        in
        let*! protocol_hash = Context_ops.get_protocol predecessor_context in
        let* () = load_protocol protocol_hash protocol_root in
        with_retry_to_load_protocol protocol_root (fun () ->
            Block_validation.precheck
              ~chain_id
              ~predecessor_block_header
              ~predecessor_block_hash
              ~predecessor_context
              ~predecessor_resulting_context_hash
              ~cache
              header
              operations)
      in
      continue block_precheck_result cache cached_result
  | External_validation.Fork_test_chain {chain_id; context_hash; forked_header}
    ->
      let*! () = Events.(emit fork_test_chain_request forked_header) in
      let*! context_opt = Context.checkout context_index context_hash in
      let*! res =
        match context_opt with
        | Some ctxt ->
            let ctxt = Shell_context.wrap_disk_context ctxt in
            with_retry_to_load_protocol protocol_root (fun () ->
                Block_validation.init_test_chain chain_id ctxt forked_header)
        | None ->
            tzfail
              (Block_validator_errors.Failed_to_checkout_context context_hash)
      in
      continue res cache cached_result
  | External_validation.Context_garbage_collection
      {context_hash; gc_lockfile_path} ->
      let*! () = Events.(emit context_gc_request context_hash) in
      let*! () = Context.gc context_index context_hash in
      let*! lockfile =
        Lwt_unix.openfile
          gc_lockfile_path
          [Unix.O_CREAT; O_RDWR; O_CLOEXEC; O_SYNC]
          0o644
      in
      let*! () =
        Lwt.catch
          (fun () -> Lwt_unix.lockf lockfile Unix.F_LOCK 0)
          (fun exn ->
            let*! () = Lwt_unix.close lockfile in
            Lwt.fail exn)
      in
      let gc_waiter () =
        Lwt.finalize
          (fun () ->
            let*! () = Context.wait_gc_completion context_index in
            let*! () = Lwt_unix.lockf lockfile Unix.F_ULOCK 0 in
            Lwt.return_unit)
          (fun () -> Lwt_unix.close lockfile)
      in
      let () = Lwt.dont_wait gc_waiter (fun _exn -> ()) in
      continue (Ok ()) cache cached_result
  | External_validation.Context_split ->
      let*! () = Events.(emit context_split_request) () in
      let*! () = Context.split context_index in
      continue (Ok ()) cache cached_result
  | External_validation.Terminate ->
      let*! () = Lwt_io.flush_all () in
      let*! () = Events.(emit termination_request ()) in
      Lwt.return `Stop
  | External_validation.Reconfigure_event_logging config ->
      let*! res =
        Tezos_base_unix.Internal_event_unix.Configuration.reapply config
      in
      continue res cache cached_result

let run ~readonly ~using_std_channel input output =
  let open Lwt_result_syntax in
  let* () = handshake input output in
  let* external_env, internal_events = init ~readonly input output in
  let*! () =
    (* if the external validator is spawned in a standalone way and communicates
       with the node through stdin/stdoud, we do no start the logging system. *)
    if using_std_channel then Lwt.return_unit
    else Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  (* Main loop waiting for request to be processed, forever, until the
     [Terminate] request is received.
  *)
  let rec loop (cache : Tezos_protocol_environment.Context.block_cache option)
      cached_result =
    let*! (External_validation.Erequest recved) =
      External_validation.recv input External_validation.request_encoding
    in
    let*! action = handle_request external_env cache cached_result recved in
    match action with
    | `Continue (res, cache, cached_result) ->
        let*! () =
          External_validation.send
            output
            (Error_monad.result_encoding
               (External_validation.result_encoding recved))
            res
        in
        loop cache cached_result
    | `Stop -> return_unit
  in
  loop None None

let main ?socket_dir ~readonly () =
  let open Lwt_result_syntax in
  let canceler = Lwt_canceler.create () in
  let* in_channel, out_channel, using_std_channel =
    match socket_dir with
    | Some socket_dir ->
        let pid = Unix.getpid () in
        let socket_path = External_validation.socket_path ~socket_dir ~pid in
        let* socket_process =
          External_validation.create_socket_connect ~canceler ~socket_path
        in
        let socket_in = Lwt_io.of_fd ~mode:Input socket_process in
        let socket_out = Lwt_io.of_fd ~mode:Output socket_process in
        return (socket_in, socket_out, false)
    | None -> return (Lwt_io.stdin, Lwt_io.stdout, true)
  in
  let*! () = Events.(emit initialized ()) in
  let*! r =
    Error_monad.catch_es (fun () ->
        let* () = run ~readonly ~using_std_channel in_channel out_channel in
        let*! r = Lwt_canceler.cancel canceler in
        match r with
        | Ok () | Error [] -> return_unit
        | Error (exc :: excs) ->
            let texc = TzTrace.make (Error_monad.Exn exc) in
            let texcs =
              List.map (fun exc -> TzTrace.make (Error_monad.Exn exc)) excs
            in
            let t = TzTrace.conp_list texc texcs in
            Lwt.return (Error t))
  in
  match r with
  | Ok () ->
      let*! () = Events.(emit terminated ()) in
      return_unit
  | Error _ as errs ->
      let*! () =
        External_validation.send
          out_channel
          (Error_monad.result_encoding Data_encoding.unit)
          errs
      in
      Lwt.return errs
