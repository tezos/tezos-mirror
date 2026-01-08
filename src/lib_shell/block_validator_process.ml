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

module Profiler = struct
  include (val Profiler.wrap Shell_profiling.block_validator_profiler)
end

type validator_environment = {
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
}

type validator_kind =
  | Internal : validator_environment * Store.Chain.chain_store -> validator_kind
  | External : {
      parameters : External_validation.parameters;
      process_path : string;
    }
      -> validator_kind

type simple_kind = External_process | Single_process

(* A common interface for the two type of validation *)
module type S = sig
  type t

  val kind : simple_kind

  val close : t -> unit Lwt.t

  val restart : t -> unit tzresult Lwt.t

  val apply_block :
    simulate:bool ->
    ?should_validate:bool ->
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

  val validate_block :
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

let[@warning "-32"] handle_reports = function
  | Some (report, profiler_report) -> (
      (match report with
      | None -> ()
      | Some report -> ( try Profiler.inc report with _ -> ())) ;
      match profiler_report with
      | None -> ()
      | Some report -> ( try Profiler.inc report with _ -> ()))
  | _ -> ()

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
        ~msg:
          "requesting validation and application of {block} for chain {chain}"
        ~pp1:Block_hash.pp
        ("block", Block_hash.encoding)
        ~pp2:Chain_id.pp
        ("chain", Chain_id.encoding)

    let application_request =
      declare_2
        ~section
        ~level:Debug
        ~name:"seq_application_request"
        ~msg:"requesting application of {block} for chain {chain}"
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

    let application_success =
      declare_2
        ~section
        ~level:Debug
        ~name:"seq_application_success"
        ~msg:"block {block} successfully applied in {timespan}"
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
    environment_headless : Tezos_profiler.Profiler.instance;
    context_headless : Tezos_profiler.Profiler.instance;
  }

  let[@warning "-32"] headless_reports validator =
    let report =
      Tezos_profiler.Profiler.report ~cpu:None validator.environment_headless
    in
    (match report with
    | None -> ()
    | Some report -> ( try Profiler.inc report with _ -> ())) ;
    let report =
      Tezos_profiler.Profiler.report ~cpu:None validator.context_headless
    in
    match report with
    | None -> ()
    | Some report -> ( try Profiler.inc report with _ -> ())

  let init
      ({
         user_activated_upgrades;
         user_activated_protocol_overrides;
         operation_metadata_size_limit;
       } :
        validator_environment) chain_store =
    let open Lwt_syntax in
    let* () = Events.(emit init ()) in
    let environment_headless =
      Tezos_profiler.Profiler.instance
        Tezos_profiler_backends.Simple_profiler.headless
        Profiler.Debug
    in
    let context_headless =
      Tezos_profiler.Profiler.instance
        Tezos_profiler_backends.Simple_profiler.headless
        Profiler.Debug
    in
    Tezos_profiler.Profiler.(plug main) environment_headless ;

    (* These profilers need to be plugged to a headless backend that can be
       shared with external processes.

       The reasoning behind this is the following:
       - The main process has profilers that are plugged to proper backends
       - When creating a child process to handle a request, its profilers are
       not plugged to any backend so any profiler call will be a no-op
       - Creating headless backends allows to share them between processes and
       gather the results when the child process has finished
       - The main process gathers the reports and writes them in the profilers
       that are plugged to backends.

       Another solution may be to plug the backends in the child processes.
    *)
    Tezos_protocol_environment.Environment_profiler.Environment_profiler.plug
      environment_headless ;
    Tezos_protocol_environment.Environment_profiler.Context_ops_profiler.plug
      context_headless ;
    return_ok
      {
        chain_store;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        cache = None;
        preapply_result = None;
        environment_headless;
        context_headless;
      }

  let kind = Single_process

  let close _ = Events.(emit close ())

  let restart _ =
    (* The single process validator cannot restarted. This is fine because the
       restart is necessary to mitigate Irmin/Brassia errors, errors that occur
       only when the validator is external. *)
    Lwt_result_syntax.return_unit

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

  let apply_block ~simulate ?(should_validate = true) validator chain_store
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
    let*! () =
      if should_validate then
        Events.(emit validation_request (block_hash, env.chain_id))
      else Events.(emit application_request (block_hash, env.chain_id))
    in
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
        ~should_validate
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
    let*! () =
      if should_validate then
        Events.(emit validation_success (block_hash, timespan))
      else Events.(emit application_success (block_hash, timespan))
    in
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
    let cache =
      match validator.cache with
      | None -> `Load
      | Some block_cache ->
          `Inherited (block_cache, predecessor_resulting_context_hash)
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
        ~cache
        operations
    in
    validator.preapply_result <- Some apply_result ;
    let () = (() [@profiler.overwrite headless_reports validator]) in
    return result

  let validate_block validator chain_store ~predecessor header _hash operations
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
    let* res =
      Block_validation.validate
        ~chain_id
        ~predecessor_block_header
        ~predecessor_block_hash
        ~predecessor_context
        ~predecessor_resulting_context_hash
        ~cache
        header
        operations
    in
    let () = (() [@profiler.overwrite headless_reports validator]) in
    return res

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
  include External_process.Make (External_validation)

  let restart = restart_hypervisee

  let kind = External_process

  let apply_block ~simulate ?(should_validate = true) validator chain_store
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
      External_validation.Apply
        {
          chain_id;
          block_header;
          predecessor_block_header;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          predecessor_resulting_context_hash;
          operations;
          max_operations_ttl;
          should_validate;
          simulate;
        }
    in
    let* res = send_request validator request in
    let () = (() [@profiler.overwrite handle_reports (snd res)]) in
    return (fst res)

  let preapply_block validator ~chain_id ~timestamp ~protocol_data ~live_blocks
      ~live_operations ~predecessor_shell_header ~predecessor_hash
      ~predecessor_max_operations_ttl ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~predecessor_resulting_context_hash
      operations =
    let open Lwt_result_syntax in
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
    let* res = send_request validator request in
    let () = (() [@profiler.overwrite handle_reports (snd res)]) in
    return (fst res)

  let validate_block validator chain_store ~predecessor header hash operations =
    let open Lwt_result_syntax in
    let chain_id = Store.Chain.chain_id chain_store in
    let predecessor_block_header = Store.Block.header predecessor in
    let predecessor_block_hash = Store.Block.hash predecessor in
    let* predecessor_resulting_context_hash =
      Store.Block.resulting_context_hash chain_store predecessor
    in
    let request =
      External_validation.Validate
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
    let* res = send_request validator request in
    let () = (() [@profiler.overwrite handle_reports (snd res)]) in
    return (fst res)

  let context_garbage_collection validator _index context_hash ~gc_lockfile_path
      =
    let open Lwt_result_syntax in
    let request =
      External_validation.Context_garbage_collection
        {context_hash; gc_lockfile_path}
    in
    let* (), _report = send_request validator request in
    return_unit

  let context_split validator _index =
    let open Lwt_result_syntax in
    let request = External_validation.Context_split in
    let* (), _report = send_request validator request in
    return_unit

  let commit_genesis validator ~chain_id =
    let open Lwt_result_syntax in
    let request = External_validation.Commit_genesis {chain_id} in
    let* res, _report = send_request validator request in
    return res

  let init_test_chain validator chain_id forking_block =
    let open Lwt_result_syntax in
    let forked_header = Store.Block.header forking_block in
    let context_hash = forked_header.shell.context in
    let request =
      External_validation.Fork_test_chain
        {chain_id; context_hash; forked_header}
    in
    let* res, _report = send_request validator request in
    return res

  let reconfigure_event_logging validator config =
    let open Lwt_result_syntax in
    let* (), _report =
      send_request
        validator
        (External_validation.Reconfigure_event_logging config)
    in
    return_unit
end

let init validator_kind =
  let open Lwt_result_syntax in
  match validator_kind with
  | Internal (validator_environment, chain_store) ->
      let* (validator : 'a) =
        Internal_validator_process.init validator_environment chain_store
      in
      let validator_process : (module S with type t = 'a) =
        (module Internal_validator_process)
      in
      return (E {validator_process; validator})
  | External {parameters; process_path} ->
      let* (validator : 'b) =
        External_validator_process.init parameters ~process_path
      in
      let validator_process : (module S with type t = 'b) =
        (module External_validator_process)
      in
      return (E {validator_process; validator})

let kind (E {validator_process; _}) =
  let (module M) = validator_process in
  M.kind

let close (E {validator_process = (module VP); validator}) = VP.close validator

let restart (E {validator_process = (module VP); validator}) =
  VP.restart validator

let reconfigure_event_logging (E {validator_process = (module VP); validator})
    config =
  VP.reconfigure_event_logging validator config

let apply_block ?(simulate = false) ?(should_validate = true)
    (E {validator_process = (module VP); validator}) chain_store ~predecessor
    header operations =
  let open Lwt_result_syntax in
  let block_hash = Block_header.hash header in
  let* () =
    when_ (not should_validate) (fun () ->
        let*! is_validated =
          Store.Block.is_known_validated chain_store block_hash
        in
        fail_unless
          is_validated
          (Block_validator_errors.Applying_non_validated_block block_hash))
  in
  let* metadata =
    (Store.Block.get_block_metadata
       chain_store
       predecessor
     [@profiler.record_s {verbosity = Notice} "get_predecessor_metadata"])
  in
  let max_operations_ttl = Store.Block.max_operations_ttl metadata in
  VP.apply_block
    ~simulate
    ~should_validate
    validator
    chain_store
    ~predecessor
    ~max_operations_ttl
    header
    operations

let validate_block (E {validator_process = (module VP); validator}) chain_store
    ~predecessor header block_hash operations =
  let open Lwt_result_syntax in
  let* live_blocks, live_operations =
    (Store.Chain.compute_live_blocks
       chain_store
       ~block:predecessor
     [@profiler.record_s {verbosity = Notice} "compute_live_blocks"])
  in
  let*? () =
    (Block_validation.check_liveness
       ~live_operations
       ~live_blocks
       block_hash
       operations [@profiler.record_f {verbosity = Notice} "check_liveness"])
  in
  VP.validate_block
    validator
    chain_store
    ~predecessor
    header
    block_hash
    operations

let context_garbage_collection (E {validator_process = (module VP); validator})
    context_index context_hash ~gc_lockfile_path =
  VP.context_garbage_collection
    validator
    context_index
    context_hash
    ~gc_lockfile_path
  [@profiler.record_s {verbosity = Notice} "context_garbage_collection"]

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
    (Store.Chain.compute_live_blocks
       chain_store
       ~block:predecessor
     [@profiler.record_s {verbosity = Notice} "compute_live_blocks"])
  in
  let predecessor_shell_header = Store.Block.shell_header predecessor in
  let predecessor_hash = Store.Block.hash predecessor in
  let predecessor_block_metadata_hash =
    Store.Block.block_metadata_hash predecessor
  in
  let predecessor_ops_metadata_hash =
    Store.Block.all_operations_metadata_hash predecessor
  in
  let* metadata =
    (Store.Block.get_block_metadata
       chain_store
       predecessor
     [@profiler.record_s {verbosity = Notice} "get_block_metadata"])
  in
  let predecessor_max_operations_ttl =
    Store.Block.max_operations_ttl metadata
  in
  let* predecessor_resulting_context_hash =
    (Store.Block.resulting_context_hash
       chain_store
       predecessor
     [@profiler.record_s {verbosity = Notice} "resulting_context_hash"])
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
