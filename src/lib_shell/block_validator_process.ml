(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

type validator_environment = {
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
}

type validator_kind =
  | Internal : Store.Chain.chain_store -> validator_kind
  | External : {
      genesis : Genesis.t;
      readonly : bool;
      data_dir : string;
      context_root : string;
      protocol_root : string;
      process_path : string;
      sandbox_parameters : Data_encoding.json option;
      dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
      internal_events : Tezos_base.Internal_event_config.t;
    }
      -> validator_kind

type simple_kind = External_process | Single_process

(* A common interface for the two type of validation *)
module type S = sig
  type t

  val kind : simple_kind

  val close : t -> unit Lwt.t

  val apply_block :
    simulate:bool ->
    ?should_precheck:bool ->
    t ->
    Store.chain_store ->
    predecessor:Store.Block.t ->
    max_operations_ttl:int ->
    Block_header.t ->
    Block_validation.operation list list ->
    Block_validation.result tzresult Lwt.t

  val preapply_block :
    t ->
    chain_id:Chain_id.t ->
    timestamp:Time.Protocol.t ->
    protocol_data:bytes ->
    live_blocks:Block_hash.Set.t ->
    live_operations:Operation_hash.Set.t ->
    predecessor_shell_header:Block_header.shell_header ->
    predecessor_hash:Block_hash.t ->
    predecessor_max_operations_ttl:int ->
    predecessor_block_metadata_hash:Block_metadata_hash.t option ->
    predecessor_ops_metadata_hash:Operation_metadata_list_list_hash.t option ->
    predecessor_resulting_context_hash:Context_hash.t ->
    Block_validation.operation list list ->
    (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

  val precheck_block :
    t ->
    Store.chain_store ->
    predecessor:Store.Block.t ->
    Block_header.t ->
    Block_hash.t ->
    Block_validation.operation trace trace ->
    unit tzresult Lwt.t

  val context_garbage_collection :
    t ->
    Context_ops.index ->
    Context_hash.t ->
    gc_lockfile_path:string ->
    unit tzresult Lwt.t

  val context_split : t -> Context_ops.index -> unit tzresult Lwt.t

  val commit_genesis : t -> chain_id:Chain_id.t -> Context_hash.t tzresult Lwt.t

  (** [init_test_chain] must only be called on a forking block. *)
  val init_test_chain :
    t -> Chain_id.t -> Store.Block.t -> Block_header.t tzresult Lwt.t

  val reconfigure_event_logging :
    t -> Internal_event_unix.Configuration.t -> unit tzresult Lwt.t
end

(* We hide the validator (of type [S.t]) and the according module in a GADT.
   This way, we avoid one pattern matching. *)
type t =
  | E : {validator_process : (module S with type t = 'a); validator : 'a} -> t

(** The standard block validation method *)
module Internal_validator_process = struct
  module Events = struct
    open Internal_event.Simple

    let section = ["sequential_block_validator"]

    let init =
      declare_0
        ~section
        ~level:Notice
        ~name:"seq_initialized"
        ~msg:"internal validation initialized"
        ()

    let close =
      declare_0
        ~section
        ~level:Notice
        ~name:"seq_close"
        ~msg:"shutting down internal validation"
        ()

    let validation_request =
      declare_2
        ~section
        ~level:Debug
        ~name:"seq_validation_request"
        ~msg:"requesting validation of {block} for chain {chain}"
        ~pp1:Block_hash.pp
        ("block", Block_hash.encoding)
        ~pp2:Chain_id.pp
        ("chain", Chain_id.encoding)

    let validation_success =
      declare_2
        ~section
        ~level:Debug
        ~name:"seq_validation_success"
        ~msg:"block {block} successfully validated in {timespan}"
        ~pp1:Block_hash.pp
        ("block", Block_hash.encoding)
        ~pp2:Time.System.Span.pp_hum
        ("timespan", Time.System.Span.encoding)

    let emit = Internal_event.Simple.emit
  end

  type t = {
    chain_store : Store.chain_store;
    user_activated_upgrades : User_activated.upgrades;
    user_activated_protocol_overrides : User_activated.protocol_overrides;
    operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
    (*
       The cache must be updated by the component that owns the
       context, i.e., the component that has the writing permissions
       on the context. In the shell, this component is the block
       validator process. For this reason, we maintain the collection
       of caches passed from one block to the next one here.
    *)
    mutable cache : Tezos_protocol_environment.Context.block_cache option;
    mutable preapply_result :
      (Block_validation.apply_result * Tezos_protocol_environment.Context.t)
      option;
  }

  let init
      ({
         user_activated_upgrades;
         user_activated_protocol_overrides;
         operation_metadata_size_limit;
       } :
        validator_environment) chain_store =
    let open Lwt_syntax in
    let* () = Events.(emit init ()) in
    return_ok
      {
        chain_store;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        cache = None;
        preapply_result = None;
      }

  let kind = Single_process

  let close _ = Events.(emit close ())

  let get_context_index chain_store =
    Store.context_index (Store.Chain.global_store chain_store)

  let make_apply_environment
      {
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        _;
      } chain_store predecessor max_operations_ttl =
    let open Lwt_result_syntax in
    let chain_id = Store.Chain.chain_id chain_store in
    let predecessor_block_header = Store.Block.header predecessor in
    let* predecessor_resulting_context_hash =
      Store.Block.resulting_context_hash chain_store predecessor
    in
    let context_index = get_context_index chain_store in
    let* predecessor_context =
      let*! o =
        Context_ops.checkout context_index predecessor_resulting_context_hash
      in
      match o with
      | None ->
          tzfail
            (Block_validator_errors.Failed_to_checkout_context
               predecessor_resulting_context_hash)
      | Some ctx -> return ctx
    in
    let predecessor_block_metadata_hash =
      Store.Block.block_metadata_hash predecessor
    in
    let predecessor_ops_metadata_hash =
      Store.Block.all_operations_metadata_hash predecessor
    in
    return
      {
        Block_validation.max_operations_ttl;
        chain_id;
        predecessor_block_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        predecessor_context;
        predecessor_resulting_context_hash;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
      }

  let apply_block ~simulate ?(should_precheck = true) validator chain_store
      ~predecessor ~max_operations_ttl block_header operations =
    let open Lwt_result_syntax in
    let* env =
      make_apply_environment
        validator
        chain_store
        predecessor
        max_operations_ttl
    in
    let now = Time.System.now () in
    let block_hash = Block_header.hash block_header in
    let predecessor_resulting_context_hash =
      env.predecessor_resulting_context_hash
    in
    let*! () = Events.(emit validation_request (block_hash, env.chain_id)) in
    let cache =
      match validator.cache with
      | None -> `Load
      | Some block_cache ->
          `Inherited (block_cache, predecessor_resulting_context_hash)
    in
    let* {result; cache} =
      Block_validation.apply
        ~simulate
        ?cached_result:validator.preapply_result
        ~should_precheck
        env
        block_header
        operations
        ~cache
    in
    let timespan =
      let then_ = Time.System.now () in
      Ptime.diff then_ now
    in
    validator.cache <-
      Some
        {context_hash = result.validation_store.resulting_context_hash; cache} ;
    validator.preapply_result <- None ;
    let*! () = Events.(emit validation_success (block_hash, timespan)) in
    return result

  let preapply_block validator ~chain_id ~timestamp ~protocol_data ~live_blocks
      ~live_operations ~predecessor_shell_header ~predecessor_hash
      ~predecessor_max_operations_ttl ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~predecessor_resulting_context_hash
      operations =
    let open Lwt_result_syntax in
    let context_index =
      Store.context_index (Store.Chain.global_store validator.chain_store)
    in
    let* predecessor_context =
      let*! o =
        Context_ops.checkout context_index predecessor_resulting_context_hash
      in
      match o with
      | None ->
          tzfail
            (Block_validator_errors.Failed_to_checkout_context
               predecessor_resulting_context_hash)
      | Some ctx -> return ctx
    in
    let user_activated_upgrades = validator.user_activated_upgrades in
    let user_activated_protocol_overrides =
      validator.user_activated_protocol_overrides
    in
    let operation_metadata_size_limit =
      validator.operation_metadata_size_limit
    in
    let* result, apply_result =
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
        ~predecessor_resulting_context_hash
        ~predecessor_shell_header
        ~predecessor_hash
        ~predecessor_max_operations_ttl
        ~predecessor_block_metadata_hash
        ~predecessor_ops_metadata_hash
        operations
    in
    validator.preapply_result <- Some apply_result ;
    return result

  let precheck_block validator chain_store ~predecessor header _hash operations
      =
    let open Lwt_result_syntax in
    let chain_id = Store.Chain.chain_id chain_store in
    let context_index =
      Store.context_index (Store.Chain.global_store validator.chain_store)
    in
    let predecessor_block_header = Store.Block.header predecessor in
    let* predecessor_resulting_context_hash =
      Store.Block.resulting_context_hash chain_store predecessor
    in
    let* predecessor_context =
      let*! o =
        Context_ops.checkout context_index predecessor_resulting_context_hash
      in
      match o with
      | None ->
          tzfail
            (Block_validator_errors.Failed_to_checkout_context
               predecessor_resulting_context_hash)
      | Some ctx -> return ctx
    in
    let cache =
      match validator.cache with
      | None -> `Lazy
      | Some block_cache ->
          `Inherited (block_cache, predecessor_resulting_context_hash)
    in
    let predecessor_block_hash = Store.Block.hash predecessor in
    Block_validation.precheck
      ~chain_id
      ~predecessor_block_header
      ~predecessor_block_hash
      ~predecessor_context
      ~predecessor_resulting_context_hash
      ~cache
      header
      operations

  let context_garbage_collection _validator context_index context_hash
      ~gc_lockfile_path:_ =
    let open Lwt_result_syntax in
    let*! () = Context_ops.gc context_index context_hash in
    return_unit

  let context_split _validator context_index =
    let open Lwt_result_syntax in
    let*! () = Context_ops.split context_index in
    return_unit

  let commit_genesis validator ~chain_id =
    let context_index = get_context_index validator.chain_store in
    let genesis = Store.Chain.genesis validator.chain_store in
    Context_ops.commit_genesis
      context_index
      ~chain_id
      ~time:genesis.time
      ~protocol:genesis.protocol

  let init_test_chain validator chain_id forking_block =
    let open Lwt_result_syntax in
    let forked_header = Store.Block.header forking_block in
    let* context = Store.Block.context validator.chain_store forking_block in
    Block_validation.init_test_chain chain_id context forked_header

  let reconfigure_event_logging _ _ = Lwt_result_syntax.return_unit
end

(** Block validation using an external process *)
module External_validator_process = struct
  module Events = struct
    open Internal_event.Simple

    let section = ["external_block_validator"]

    let init =
      declare_0
        ~section
        ~level:Notice
        ~name:"proc_initialized"
        ~msg:"external validation initialized"
        ()

    let close =
      declare_0
        ~section
        ~level:Notice
        ~name:"proc_close"
        ~msg:"shutting down external validation"
        ()

    let process_exited_abnormally =
      let open Unix in
      let process_status_encoding =
        let open Data_encoding in
        union
          [
            case
              (Tag 0)
              ~title:"wexited"
              int31
              (function WEXITED i -> Some i | _ -> None)
              (fun i -> WEXITED i);
            case
              (Tag 1)
              ~title:"wsignaled"
              int31
              (function WSIGNALED i -> Some i | _ -> None)
              (fun i -> WSIGNALED i);
            case
              (Tag 2)
              ~title:"wstopped"
              int31
              (function WSTOPPED i -> Some i | _ -> None)
              (fun i -> WSTOPPED i);
          ]
      in
      declare_1
        ~section
        ~level:Error
        ~name:"proc_status"
        ~msg:"{status_msg}"
        ~pp1:(fun fmt status ->
          match status with
          | WEXITED i ->
              Format.fprintf
                fmt
                "process terminated abnormally with exit code %i"
                i
          | WSIGNALED i ->
              Format.fprintf
                fmt
                "process was killed by signal %s"
                (Lwt_exit.signal_name i)
          | WSTOPPED i ->
              Format.fprintf
                fmt
                "process was stopped by signal %s"
                (Lwt_exit.signal_name i))
        ("status_msg", process_status_encoding)

    let process_exited_normally =
      declare_0
        ~section
        ~level:Notice
        ~name:"proc_exited_normally"
        ~msg:"process terminated normally"
        ()

    let validator_started =
      declare_1
        ~section
        ~level:Notice
        ~name:"proc_validator_started"
        ~msg:"block validator process started with pid {pid}"
        ~pp1:Format.pp_print_int
        ("pid", Data_encoding.int31)

    let cannot_close =
      declare_0
        ~section
        ~level:Info
        ~name:"cannot_close"
        ~msg:"cannot close the block validation process: connection failed"
        ()

    let unresponsive_validator =
      declare_0
        ~section
        ~level:Notice
        ~name:"unresponsive_validator"
        ~msg:
          "force quitting the block validation process as it seems to be \
           unresponsive"
        ()

    let cannot_start_process =
      declare_0
        ~section
        ~level:Info
        ~name:"cannot_start_process"
        ~msg:"cannot start validation process: the node is shutting down"
        ()

    let request_for =
      declare_1
        ~section
        ~level:Debug
        ~name:"proc_request"
        ~msg:"request for {request}"
        ~pp1:External_validation.request_pp
        ("request", External_validation.request_encoding)

    let request_result =
      declare_2
        ~section
        ~level:Debug
        ~name:"proc_request_result"
        ~msg:"completion of {request_result} in {timespan}"
        ~pp1:External_validation.request_pp
        ("request_result", External_validation.request_encoding)
        ~pp2:Time.System.Span.pp_hum
        ("timespan", Time.System.Span.encoding)

    let emit = Internal_event.Simple.emit
  end

  type validator_process = {
    process : Lwt_process.process_none;
    process_socket : Lwt_unix.file_descr;
    input : Lwt_io.output_channel;
    output : Lwt_io.input_channel;
    canceler : Lwt_canceler.t;
    clean_up_callback_id : Lwt_exit.clean_up_callback_id;
  }

  type process_status = Uninitialized | Running of validator_process | Exiting

  type t = {
    genesis : Genesis.t;
    readonly : bool;
    data_dir : string;
    context_root : string;
    protocol_root : string;
    user_activated_upgrades : User_activated.upgrades;
    user_activated_protocol_overrides : User_activated.protocol_overrides;
    operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
    process_path : string;
    mutable validator_process : process_status;
    lock : Lwt_mutex.t;
    sandbox_parameters : Data_encoding.json option;
    dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
    internal_events : Tezos_base.Internal_event_config.t;
  }

  let kind = External_process

  (* The shutdown_timeout is used when closing the block validator
     process. It aims to allow it to shutdown gracefully. This delay
     is long enough to allow the validator to successfully terminate
     its current task and is short enough to avoid bothering the
     user. *)
  let shutdown_timeout = 5.

  (* Returns a temporary path for the socket to be
     spawned. $XDG_RUNTIME_DIR is returned if the environment variable
     is defined. Otherwise, the default temporary directory is used. *)
  let get_temporary_socket_dir () =
    match Sys.getenv_opt "XDG_RUNTIME_DIR" with
    | Some xdg_runtime_dir when xdg_runtime_dir <> "" -> xdg_runtime_dir
    | Some _ | None -> Filename.get_temp_dir_name ()

  (* Ad-hoc request to make the handshake with the external validator.
     This is expected to be used each time the external validator
     process is (re)started.
     The scenario of the handshake is the following:
     - simultaneously, the node and the external validator send some magic bytes
       to each others,
     - simultaneously, the node and the external validator wait for each others
       magic bytes and check their validity,
     - handshake is finished. *)
  let process_handshake process_input process_output =
    let open Lwt_result_syntax in
    let*! () =
      External_validation.send
        process_input
        Data_encoding.Variable.bytes
        External_validation.magic
    in
    let*! magic =
      External_validation.recv process_output Data_encoding.Variable.bytes
    in
    fail_when
      (not (Bytes.equal magic External_validation.magic))
      (Block_validator_errors.Validation_process_failed
         (Inconsistent_handshake "bad magic"))

  (* Ad-hoc request to pass startup arguments to the external
     validator. This is expected to be run after the
     [process_handshake].
     This is expected to be used each time the external validator
     process is (re)started.
     The scenario of the init is the following:
     - execute the handshake,
     - the node sends some parameters and waits for an ack,
     - the external validator initializes it's state thanks to the
       given parameters,
     - the external validator returns a ack to confirm a successful
       initialization,
     - the node receives the ack and continues,
     - initialization is finished.
  *)
  let process_init vp process_input process_output =
    let open Lwt_result_syntax in
    let parameters =
      {
        External_validation.context_root = vp.context_root;
        protocol_root = vp.protocol_root;
        sandbox_parameters = vp.sandbox_parameters;
        genesis = vp.genesis;
        user_activated_upgrades = vp.user_activated_upgrades;
        user_activated_protocol_overrides = vp.user_activated_protocol_overrides;
        operation_metadata_size_limit = vp.operation_metadata_size_limit;
        dal_config = vp.dal_config;
        internal_events = vp.internal_events;
      }
    in
    let*! () =
      External_validation.send
        process_input
        External_validation.parameters_encoding
        parameters
    in
    let* () =
      External_validation.recv
        process_output
        (Error_monad.result_encoding Data_encoding.empty)
    in
    return_unit

  (* Proceeds to a full initialization of the external validator
     process by opening the communication channels, spawning the
     external validator and calling the handshake and initialization
     functions.
     TODO: Add critical section for external validator launch, see
     https://gitlab.com/tezos/tezos/-/issues/5175
  *)
  let start_process vp =
    let open Lwt_result_syntax in
    let canceler = Lwt_canceler.create () in
    (* We assume that there is only one validation process per socket *)
    let socket_dir = get_temporary_socket_dir () in
    let args =
      ["octez-validator"; "--socket-dir"; socket_dir]
      @ match vp.readonly with true -> ["--readonly"] | false -> []
    in
    let env = Unix.environment () in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4837

       We unset the [env_var_name] environment variable environment
       variable so that events emitted by the external validator
       process are not mixed up with the events that could be printed
       by the external validator. This is a temporary fix and a better
       solution would be welcome! *)
    let env =
      Array.to_seq env
      |> Seq.filter (fun binding ->
             match String.split_on_char '=' binding with
             | env_var_name :: _
               when env_var_name
                    = Tezos_base_unix.Internal_event_unix.env_var_name ->
                 false
             | _ -> true)
      |> Array.of_seq
    in
    let process =
      Lwt_process.open_process_none ~env (vp.process_path, Array.of_list args)
    in
    let socket_path =
      External_validation.socket_path ~socket_dir ~pid:process#pid
    in
    (* Make sure that the mimicked anonymous file descriptor is
        removed if the spawn of the process is interupted. Thus, we
        avoid generating potential garbage in the [socket_dir].
        No interruption can occur since the resource was created
        because there are no yield points. *)
    let clean_process_fd socket_path =
      Lwt.catch
        (fun () -> Lwt_unix.unlink socket_path)
        (function
          | Unix.Unix_error (ENOENT, _, _) ->
              (* The file does not exist *)
              Lwt.return_unit
          | Unix.Unix_error (EACCES, _, _) ->
              (* We ignore failing on EACCES as no file was created *)
              Lwt.return_unit
          | exn -> Lwt.reraise exn)
    in
    let process_fd_cleaner =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          clean_process_fd socket_path)
    in
    let* process_socket =
      Lwt.finalize
        (fun () ->
          let* process_socket =
            External_validation.create_socket_listen
              ~canceler
              ~max_requests:1
              ~socket_path
          in
          let*! v, _ = Lwt_unix.accept process_socket in
          let*! () = Lwt_unix.close process_socket in
          return v)
        (fun () ->
          (* As the external validation process is now started, we can
              unlink the named socket. Indeed, the file descriptor will
              remain valid as long as at least one process keeps it
              open. This method mimics an anonymous file descriptor
              without relying on Linux specific features. It also
              trigger the clean up procedure if some sockets related
              errors are thrown. *)
          clean_process_fd socket_path)
    in
    Lwt_exit.unregister_clean_up_callback process_fd_cleaner ;
    (* Register clean up callback to ensure that the validator process
       will be terminated even if the node is brutally stopped. *)
    let clean_up_callback_id =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          Lwt.return (Stdlib.at_exit (fun () -> process#terminate)))
    in
    let process_input = Lwt_io.of_fd ~mode:Output process_socket in
    let process_output = Lwt_io.of_fd ~mode:Input process_socket in
    let*! () = Events.(emit validator_started process#pid) in
    vp.validator_process <-
      Running
        {
          process;
          process_socket;
          input = process_input;
          output = process_output;
          canceler;
          clean_up_callback_id;
        } ;
    let* () = process_handshake process_input process_output in
    let* () = process_init vp process_input process_output in
    return (process, process_input, process_output)

  (* Inspects the validator's state and return it. If the process is
     in an inconsistent state, it will be restarted automatically --
     by running [start_process]. *)
  let validator_state vp =
    let open Lwt_result_syntax in
    match vp.validator_process with
    | Running
        {
          process;
          process_socket;
          input = process_input;
          output = process_output;
          canceler;
          clean_up_callback_id;
        } -> (
        match process#state with
        | Running -> return (process, process_input, process_output)
        | Exited status ->
            (* When the process is in an inconsistent state, we restart
               it automatically. *)
            let*! () = Error_monad.cancel_with_exceptions canceler in
            Lwt_exit.unregister_clean_up_callback clean_up_callback_id ;
            let*! () =
              Lwt.catch
                (fun () -> Lwt_unix.close process_socket)
                (fun _ -> Lwt.return_unit)
            in
            vp.validator_process <- Uninitialized ;
            let*! () = Events.(emit process_exited_abnormally status) in
            start_process vp)
    | Uninitialized -> start_process vp
    | Exiting ->
        let*! () = Events.(emit cannot_start_process ()) in
        tzfail Block_validator_errors.Cannot_validate_while_shutting_down

  (* Sends the given request to the external validator. If the request
     failed to be fulfilled, the status of the external validator is
     set to Uninitialized and the associated error is propagated. *)
  let send_request vp request result_encoding =
    let open Lwt_result_syntax in
    let* process, process_input, process_output = validator_state vp in
    Lwt.catch
      (fun () ->
        (* Make sure that the promise is not cancelled between a send
           and recv *)
        let* res =
          Lwt.protected
            (Lwt_mutex.with_lock vp.lock (fun () ->
                 let now = Time.System.now () in
                 let*! () = Events.(emit request_for request) in
                 let*! () =
                   External_validation.send
                     process_input
                     External_validation.request_encoding
                     request
                 in
                 let*! res =
                   External_validation.recv_result
                     process_output
                     result_encoding
                 in
                 let timespan =
                   let then_ = Time.System.now () in
                   Ptime.diff then_ now
                 in
                 let*! () = Events.(emit request_result (request, timespan)) in
                 Lwt.return res))
        in
        match process#state with
        | Running -> return res
        | Exited status ->
            vp.validator_process <- Uninitialized ;
            let*! () = Events.(emit process_exited_abnormally status) in
            return res)
      (fun exn ->
        let*! () =
          match process#state with
          | Running -> Lwt.return_unit
          | Exited status ->
              let*! () = Events.(emit process_exited_abnormally status) in
              vp.validator_process <- Uninitialized ;
              Lwt.return_unit
        in
        fail_with_exn exn)

  (* The initialization phase aims to configure the external validator
     and start it's associated process. This will result in the call
     of [process_handshake] and [process_init].
     Note that it is important to have [init] as a blocking promise as
     the external validator must initialize the context (in RW) before
     the node tries to open it (in RO).*)
  let init
      ({
         user_activated_upgrades;
         user_activated_protocol_overrides;
         operation_metadata_size_limit;
         _;
       } :
        validator_environment) ~genesis ~data_dir ~readonly ~context_root
      ~protocol_root ~process_path ~sandbox_parameters ~dal_config
      ~internal_events =
    let open Lwt_result_syntax in
    let validator =
      {
        data_dir;
        readonly;
        genesis;
        context_root;
        protocol_root;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        process_path;
        validator_process = Uninitialized;
        lock = Lwt_mutex.create ();
        sandbox_parameters;
        dal_config;
        internal_events;
      }
    in
    let* (_ :
           Lwt_process.process_none
           * Lwt_io.output Lwt_io.channel
           * Lwt_io.input Lwt_io.channel) =
      start_process validator
    in
    let*! () = Events.(emit init ()) in
    return validator

  let apply_block ~simulate ?(should_precheck = true) validator chain_store
      ~predecessor ~max_operations_ttl block_header operations =
    let open Lwt_result_syntax in
    let chain_id = Store.Chain.chain_id chain_store in
    let predecessor_block_header = Store.Block.header predecessor in
    let predecessor_block_metadata_hash =
      Store.Block.block_metadata_hash predecessor
    in
    let predecessor_ops_metadata_hash =
      Store.Block.all_operations_metadata_hash predecessor
    in
    let* predecessor_resulting_context_hash =
      Store.Block.resulting_context_hash chain_store predecessor
    in
    let request =
      External_validation.Validate
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
        }
    in
    send_request validator request Block_validation.result_encoding

  let preapply_block validator ~chain_id ~timestamp ~protocol_data ~live_blocks
      ~live_operations ~predecessor_shell_header ~predecessor_hash
      ~predecessor_max_operations_ttl ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~predecessor_resulting_context_hash
      operations =
    let request =
      External_validation.Preapply
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
        }
    in
    send_request validator request Block_validation.preapply_result_encoding

  let precheck_block validator chain_store ~predecessor header hash operations =
    let open Lwt_result_syntax in
    let chain_id = Store.Chain.chain_id chain_store in
    let predecessor_block_header = Store.Block.header predecessor in
    let predecessor_block_hash = Store.Block.hash predecessor in
    let* predecessor_resulting_context_hash =
      Store.Block.resulting_context_hash chain_store predecessor
    in
    let request =
      External_validation.Precheck
        {
          chain_id;
          predecessor_block_header;
          predecessor_block_hash;
          predecessor_resulting_context_hash;
          header;
          operations;
          hash;
        }
    in
    send_request validator request Data_encoding.unit

  let context_garbage_collection validator _index context_hash ~gc_lockfile_path
      =
    let request =
      External_validation.Context_garbage_collection
        {context_hash; gc_lockfile_path}
    in
    send_request validator request Data_encoding.unit

  let context_split validator _index =
    let request = External_validation.Context_split in
    send_request validator request Data_encoding.unit

  let commit_genesis validator ~chain_id =
    let request = External_validation.Commit_genesis {chain_id} in
    send_request validator request Context_hash.encoding

  let init_test_chain validator chain_id forking_block =
    let forked_header = Store.Block.header forking_block in
    let context_hash = forked_header.shell.context in
    let request =
      External_validation.Fork_test_chain
        {chain_id; context_hash; forked_header}
    in
    send_request validator request Block_header.encoding

  let reconfigure_event_logging validator config =
    send_request
      validator
      (External_validation.Reconfigure_event_logging config)
      Data_encoding.unit

  let close vp =
    let open Lwt_syntax in
    let* () = Events.(emit close ()) in
    match vp.validator_process with
    | Running {process; input = process_input; canceler; _} ->
        let request = External_validation.Terminate in
        let* () = Events.(emit request_for request) in
        let* () =
          Lwt.catch
            (fun () ->
              vp.validator_process <- Exiting ;
              (* Try to trigger the clean shutdown of the validation
                 process. *)
              External_validation.send
                process_input
                External_validation.request_encoding
                request)
            (function
              | Unix.Unix_error (ECONNREFUSED, _, _)
              | Unix.Unix_error (EPIPE, _, _)
              | Unix.Unix_error (ENOTCONN, _, _) ->
                  (* It may fail if the validation process is not
                     responding (connection already closed) and is
                     killed afterward. No need to propagate the error. *)
                  let* () = Events.(emit cannot_close ()) in
                  Lwt.return_unit
              | e -> Lwt.reraise e)
        in
        let* () =
          Lwt.catch
            (fun () ->
              Lwt_unix.with_timeout shutdown_timeout (fun () ->
                  let* s = process#status in
                  match s with
                  | Unix.WEXITED 0 -> Events.(emit process_exited_normally ())
                  | status ->
                      let* () =
                        Events.(emit process_exited_abnormally status)
                      in
                      process#terminate ;
                      Lwt.return_unit))
            (function
              | Lwt_unix.Timeout -> Events.(emit unresponsive_validator) ()
              | err -> Lwt.reraise err)
        in
        let* () = Error_monad.cancel_with_exceptions canceler in
        Lwt.return_unit
    | Uninitialized | Exiting -> Lwt.return_unit
end

let init validator_environment validator_kind =
  let open Lwt_result_syntax in
  match validator_kind with
  | Internal chain_store ->
      let* (validator : 'a) =
        Internal_validator_process.init validator_environment chain_store
      in
      let validator_process : (module S with type t = 'a) =
        (module Internal_validator_process)
      in
      return (E {validator_process; validator})
  | External
      {
        genesis;
        readonly;
        data_dir;
        context_root;
        protocol_root;
        process_path;
        sandbox_parameters;
        dal_config;
        internal_events;
      } ->
      let* (validator : 'b) =
        External_validator_process.init
          validator_environment
          ~genesis
          ~data_dir
          ~readonly
          ~context_root
          ~protocol_root
          ~process_path
          ~sandbox_parameters
          ~dal_config
          ~internal_events
      in
      let validator_process : (module S with type t = 'b) =
        (module External_validator_process)
      in
      return (E {validator_process; validator})

let kind (E {validator_process; _}) =
  let (module M) = validator_process in
  M.kind

let close (E {validator_process = (module VP); validator}) = VP.close validator

let reconfigure_event_logging (E {validator_process = (module VP); validator})
    config =
  VP.reconfigure_event_logging validator config

let apply_block ?(simulate = false) ?(should_precheck = true)
    (E {validator_process = (module VP); validator}) chain_store ~predecessor
    header operations =
  let open Lwt_result_syntax in
  let block_hash = Block_header.hash header in
  let* () =
    when_ (not should_precheck) (fun () ->
        let*! is_validated =
          Store.Block.is_known_validated chain_store block_hash
        in
        fail_unless
          is_validated
          (Block_validator_errors.Applying_non_validated_block block_hash))
  in
  let* metadata = Store.Block.get_block_metadata chain_store predecessor in
  let max_operations_ttl = Store.Block.max_operations_ttl metadata in
  let* live_blocks, live_operations =
    Store.Chain.compute_live_blocks chain_store ~block:predecessor
  in
  let*? () =
    Block_validation.check_liveness
      ~live_operations
      ~live_blocks
      block_hash
      operations
  in
  VP.apply_block
    ~simulate
    ~should_precheck
    validator
    chain_store
    ~predecessor
    ~max_operations_ttl
    header
    operations

let precheck_block (E {validator_process = (module VP); validator}) chain_store
    ~predecessor header operations =
  VP.precheck_block validator chain_store ~predecessor header operations

let context_garbage_collection (E {validator_process = (module VP); validator})
    context_index context_hash ~gc_lockfile_path =
  VP.context_garbage_collection
    validator
    context_index
    context_hash
    ~gc_lockfile_path

let context_split (E {validator_process = (module VP); validator}) context_index
    =
  VP.context_split validator context_index

let commit_genesis (E {validator_process = (module VP); validator}) ~chain_id =
  VP.commit_genesis validator ~chain_id

let init_test_chain (E {validator_process = (module VP); validator}) chain_id
    forked_block =
  VP.init_test_chain validator chain_id forked_block

let preapply_block (E {validator_process = (module VP); validator} : t)
    chain_store ~predecessor ~protocol_data ~timestamp operations =
  let open Lwt_result_syntax in
  let chain_id = Store.Chain.chain_id chain_store in
  let* live_blocks, live_operations =
    Store.Chain.compute_live_blocks chain_store ~block:predecessor
  in
  let predecessor_shell_header = Store.Block.shell_header predecessor in
  let predecessor_hash = Store.Block.hash predecessor in
  let predecessor_block_metadata_hash =
    Store.Block.block_metadata_hash predecessor
  in
  let predecessor_ops_metadata_hash =
    Store.Block.all_operations_metadata_hash predecessor
  in
  let* metadata = Store.Block.get_block_metadata chain_store predecessor in
  let predecessor_max_operations_ttl =
    Store.Block.max_operations_ttl metadata
  in
  let* predecessor_resulting_context_hash =
    Store.Block.resulting_context_hash chain_store predecessor
  in
  VP.preapply_block
    validator
    ~chain_id
    ~timestamp
    ~protocol_data
    ~live_blocks
    ~live_operations
    ~predecessor_shell_header
    ~predecessor_hash
    ~predecessor_max_operations_ttl
    ~predecessor_block_metadata_hash
    ~predecessor_ops_metadata_hash
    ~predecessor_resulting_context_hash
    operations
