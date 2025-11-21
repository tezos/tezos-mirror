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

module Profiler =
  Tezos_protocol_environment.Environment_profiler.Environment_profiler

module Events = struct
  open Internal_event.Simple

  let section = ["external_validator"]

  let dynload_protocol =
    declare_1
      ~section
      ~level:Info
      ~name:"dynload_protocol"
      ~msg:"dynamic loading of protocol {protocol}"
      ~pp1:Protocol_hash.pp
      ("protocol", Protocol_hash.encoding)

  let emit = Internal_event.Simple.emit
end

module Processing = struct
  open Filename.Infix

  type state = {
    context_index : Context_ops.index;
    cache : Context_ops.block_cache option;
    cached_result : (Block_validation.apply_result * Context_ops.t) option;
    headless : Tezos_profiler.Profiler.instance;
    profiler_headless : Tezos_profiler.Profiler.instance;
  }

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

  let initial_state
      ({genesis; readonly; sandbox_parameters; data_dir; _} :
        External_validation.parameters) =
    let open Lwt_result_syntax in
    let sandbox_parameters =
      Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
    in
    let* context_index =
      Context_ops.init
        ~kind:`Disk
        ~patch_context:(fun ctxt ->
          Patch_context.patch_context genesis sandbox_parameters ctxt)
        ~readonly
        ~data_dir
        ()
    in
    let headless =
      Tezos_profiler.Profiler.instance
        Tezos_profiler_backends.Simple_profiler.headless
        Profiler.Info
    in
    let profiler_headless =
      Tezos_profiler.Profiler.instance
        Tezos_profiler_backends.Simple_profiler.headless
        Profiler.Info
    in

    Tezos_profiler.Profiler.(plug main) headless ;
    Tezos_protocol_environment.Environment_profiler.Environment_profiler.plug
      headless ;
    Tezos_protocol_environment.Environment_profiler.Context_ops_profiler.plug
      profiler_headless ;
    return
      {
        context_index;
        cache = None;
        cached_result = None;
        headless;
        profiler_headless;
      }

  let handle_request : type a.
      External_validation.parameters ->
      state ->
      a External_validation.request ->
      [ `Continue of
        (a
        * (Tezos_profiler.Profiler.report option
          * Tezos_profiler.Profiler.report option)
          option)
        tzresult
        * state
      | `Stop ]
      Lwt.t =
   fun {
         protocol_root;
         genesis;
         user_activated_upgrades;
         user_activated_protocol_overrides;
         operation_metadata_size_limit;
         _;
       }
       {context_index; cache; cached_result; headless; profiler_headless} ->
    let open Lwt_result_syntax in
    let continue res cache cached_result report =
      let res =
        match res with Error errs -> Error errs | Ok res -> Ok (res, report)
      in
      Lwt.return
        (`Continue
           ( res,
             {context_index; cache; cached_result; headless; profiler_headless}
           ))
    in
    function
    | Commit_genesis {chain_id} ->
        let*! commit =
          Error_monad.catch_es (fun () ->
              Context_ops.commit_genesis
                context_index
                ~chain_id
                ~time:genesis.time
                ~protocol:genesis.protocol)
        in
        continue commit cache None None
    | Apply
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
        } ->
        ()
        [@profiler.record
          {verbosity = Notice} "external_validator : apply_block"] ;
        let*! block_application_result =
          let* predecessor_context =
            Error_monad.catch_es (fun () ->
                let*! o =
                  Context_ops.checkout
                    context_index
                    predecessor_resulting_context_hash
                in
                match o with
                | Some c -> return c
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
            | Some cache ->
                `Inherited (cache, predecessor_resulting_context_hash)
          in
          with_retry_to_load_protocol protocol_root (fun () ->
              Block_validation.apply
                ~simulate
                ?cached_result
                ~should_validate
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
        () [@profiler.stop] ;
        let report = Tezos_profiler.Profiler.report ~cpu:None headless in
        let profiler_report =
          Tezos_profiler.Profiler.report ~cpu:None profiler_headless
        in
        continue
          block_application_result
          cache
          None
          (Some (report, profiler_report))
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
        ()
        [@profiler.record
          {verbosity = Notice} "external_validator : preapply_block"] ;
        let*! block_preapplication_result =
          let* predecessor_context =
            Error_monad.catch_es (fun () ->
                let*! context =
                  Context_ops.checkout
                    context_index
                    predecessor_resulting_context_hash
                in
                match context with
                | Some context -> return context
                | None ->
                    tzfail
                      (Block_validator_errors.Failed_to_checkout_context
                         predecessor_resulting_context_hash))
          in
          let*! protocol_hash = Context_ops.get_protocol predecessor_context in
          let* () = load_protocol protocol_hash protocol_root in
          let cache =
            match cache with
            | None -> `Load
            | Some cache ->
                `Inherited (cache, predecessor_resulting_context_hash)
          in
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
                ~cache
                operations)
        in
        let*! res, cachable_result =
          match block_preapplication_result with
          | Ok (res, last_preapplied_context) ->
              Lwt.return (Ok res, Some last_preapplied_context)
          | Error _ as err -> Lwt.return (err, None)
        in
        () [@profiler.stop] ;
        let report = Tezos_profiler.Profiler.report ~cpu:None headless in
        let profiler_report =
          Tezos_profiler.Profiler.report ~cpu:None profiler_headless
        in
        continue res cache cachable_result (Some (report, profiler_report))
    | External_validation.Validate
        {
          chain_id;
          predecessor_block_header;
          predecessor_block_hash;
          predecessor_resulting_context_hash;
          header;
          operations;
          _;
        } ->
        ()
        [@profiler.record
          {verbosity = Notice} "external_validator : validate_block"] ;
        let*! block_validate_result =
          let* predecessor_context =
            Error_monad.catch_es (fun () ->
                let*! o =
                  Context_ops.checkout
                    context_index
                    predecessor_resulting_context_hash
                in
                match o with
                | Some context -> return context
                | None ->
                    tzfail
                      (Block_validator_errors.Failed_to_checkout_context
                         predecessor_resulting_context_hash))
          in
          let cache =
            match cache with
            | None -> `Lazy
            | Some cache ->
                `Inherited (cache, predecessor_resulting_context_hash)
          in
          let*! protocol_hash = Context_ops.get_protocol predecessor_context in
          let* () = load_protocol protocol_hash protocol_root in
          with_retry_to_load_protocol protocol_root (fun () ->
              Block_validation.validate
                ~chain_id
                ~predecessor_block_header
                ~predecessor_block_hash
                ~predecessor_context
                ~predecessor_resulting_context_hash
                ~cache
                header
                operations)
        in
        () [@profiler.stop] ;
        let report = Tezos_profiler.Profiler.report ~cpu:None headless in
        let profiler_report =
          Tezos_profiler.Profiler.report ~cpu:None profiler_headless
        in
        continue
          block_validate_result
          cache
          cached_result
          (Some (report, profiler_report))
    | External_validation.Fork_test_chain
        {chain_id; context_hash; forked_header} ->
        let*! context_opt = Context_ops.checkout context_index context_hash in
        let*! res =
          match context_opt with
          | Some ctxt ->
              with_retry_to_load_protocol protocol_root (fun () ->
                  Block_validation.init_test_chain chain_id ctxt forked_header)
          | None ->
              tzfail
                (Block_validator_errors.Failed_to_checkout_context context_hash)
        in
        continue res cache cached_result None
    | External_validation.Context_garbage_collection
        {context_hash; gc_lockfile_path} ->
        let*! () = Context_ops.gc context_index context_hash in
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
              let*! () = Context_ops.wait_gc_completion context_index in
              let*! () = Lwt_unix.lockf lockfile Unix.F_ULOCK 0 in
              Lwt.return_unit)
            (fun () -> Lwt_unix.close lockfile)
        in
        let () = Lwt.dont_wait gc_waiter (fun _exn -> ()) in
        continue (Ok ()) cache cached_result None
    | External_validation.Context_split ->
        let*! () = Context_ops.split context_index in
        continue (Ok ()) cache cached_result None
    | External_validation.Terminate ->
        let*! () = Lwt_io.flush_all () in
        Lwt.return `Stop
    | External_validation.Reconfigure_event_logging config ->
        let*! res =
          Tezos_base_unix.Internal_event_unix.Configuration.reapply config
        in
        continue res cache cached_result None
end

include
  Tezos_base_unix.External_process_main.Make (External_validation) (Processing)
module Hypervisor =
  Tezos_base_unix.Hypervisor_process_main.Make (External_validation)
