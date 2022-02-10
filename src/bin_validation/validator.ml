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
      ~level:Debug
      ~name:"dynload_protocol"
      ~msg:"dynamic loading of protocol {protocol}"
      ~pp1:Protocol_hash.pp
      ("protocol", Protocol_hash.encoding)

  let validation_request =
    declare_1
      ~section
      ~level:Debug
      ~name:"validation_request"
      ~msg:"validating block {block}"
      ~pp1:(fun fmt header -> Block_hash.pp fmt (Block_header.hash header))
      ("block", Block_header.encoding)

  let precheck_request =
    declare_1
      ~section
      ~level:Debug
      ~name:"precheck_request"
      ~msg:"prechecking block {hash}"
      ~pp1:Block_hash.pp
      ("hash", Block_hash.encoding)

  let commit_genesis_request =
    declare_1
      ~section
      ~level:Debug
      ~name:"commit_genesis_request"
      ~msg:"committing genesis block {genesis}"
      ~pp1:Block_hash.pp
      ("genesis", Block_hash.encoding)

  let initialization_request =
    declare_0
      ~section
      ~level:Debug
      ~name:"initialization_request"
      ~msg:"initializing validator's environment"
      ()

  let fork_test_chain_request =
    declare_1
      ~section
      ~level:Debug
      ~name:"fork_testchain_request"
      ~msg:"forking test chain at block {block}"
      ~pp1:Block_header.pp
      ("block", Block_header.encoding)

  let termination_request =
    declare_0
      ~section
      ~level:Debug
      ~name:"termination_request"
      ~msg:"validator terminated"
      ()

  let emit = Internal_event.Simple.emit
end

open Filename.Infix

let load_protocol proto protocol_root =
  let open Lwt_tzresult_syntax in
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
            fail
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

let init input =
  let open Lwt_syntax in
  let* () = Events.(emit initialization_request ()) in
  let* {
         context_root;
         protocol_root;
         sandbox_parameters;
         genesis;
         user_activated_upgrades;
         user_activated_protocol_overrides;
       } =
    External_validation.recv input External_validation.parameters_encoding
  in
  let sandbox_parameters =
    Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
  in
  let* context_index =
    Context.init
      ~patch_context:(Patch_context.patch_context genesis sandbox_parameters)
      context_root
  in
  Lwt.return
    ( context_index,
      protocol_root,
      genesis,
      user_activated_upgrades,
      user_activated_protocol_overrides )

let run input output =
  let open Lwt_tzresult_syntax in
  let* () = handshake input output in
  let*! ( context_index,
          protocol_root,
          genesis,
          user_activated_upgrades,
          user_activated_protocol_overrides ) =
    init input
  in
  let rec loop (cache : Environment_context.Context.block_cache option)
      cached_result =
    let*! recved =
      External_validation.recv input External_validation.request_encoding
    in
    match recved with
    | External_validation.Init ->
        let init : unit Lwt.t =
          External_validation.send
            output
            (Error_monad.result_encoding Data_encoding.empty)
            (Ok ())
        in
        let*! () = init in
        loop cache None
    | External_validation.Commit_genesis {chain_id} ->
        let*! () = Events.(emit commit_genesis_request genesis.block) in
        let*! commit =
          Error_monad.catch_es (fun () ->
              Context.commit_genesis
                context_index
                ~chain_id
                ~time:genesis.time
                ~protocol:genesis.protocol)
        in
        let*! () =
          External_validation.send
            output
            (Error_monad.result_encoding Context_hash.encoding)
            commit
        in
        loop cache None
    | External_validation.Validate
        {
          chain_id;
          block_header;
          predecessor_block_header;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          operations;
          max_operations_ttl;
        } ->
        let*! () = Events.(emit validation_request block_header) in
        let*! block_application_result =
          let* predecessor_context =
            Error_monad.catch_es (fun () ->
                let pred_context_hash =
                  predecessor_block_header.shell.context
                in
                let*! o = Context.checkout context_index pred_context_hash in
                match o with
                | Some context -> return context
                | None ->
                    fail
                      (Block_validator_errors.Failed_to_checkout_context
                         pred_context_hash))
          in
          let*! protocol_hash = Context.get_protocol predecessor_context in
          let* () = load_protocol protocol_hash protocol_root in
          let env =
            {
              Block_validation.chain_id;
              user_activated_upgrades;
              user_activated_protocol_overrides;
              max_operations_ttl;
              predecessor_block_header;
              predecessor_block_metadata_hash;
              predecessor_ops_metadata_hash;
              predecessor_context;
            }
          in
          let predecessor_context = predecessor_block_header.shell.context in
          let cache =
            match cache with
            | None -> `Load
            | Some cache -> `Inherited (cache, predecessor_context)
          in
          with_retry_to_load_protocol protocol_root (fun () ->
              Block_validation.apply
                ?cached_result
                env
                block_header
                operations
                ~cache)
        in
        let (block_application_result, cache) =
          match block_application_result with
          | Error [Validation_errors.Inconsistent_hash _] as err ->
              (* This is a special case added for Hangzhou that could
                 be removed once the successor of Hangzhou will be
                 activated. This behavior is here to keep the
                 compatibility with the version Octez v11 which has a
                 buggy behavior with Hangzhou. *)
              (err, None)
          | Error _ as err -> (err, cache)
          | Ok {result; cache} ->
              ( Ok result,
                Some {context_hash = block_header.shell.context; cache} )
        in
        let*! () =
          External_validation.send
            output
            (Error_monad.result_encoding Block_validation.result_encoding)
            block_application_result
        in
        loop cache None
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
          operations;
        } ->
        let*! block_preapplication_result =
          let* predecessor_context =
            Error_monad.catch_es (fun () ->
                let pred_context_hash = predecessor_shell_header.context in
                let*! context =
                  Context.checkout context_index pred_context_hash
                in
                match context with
                | Some context -> return context
                | None ->
                    fail
                      (Block_validator_errors.Failed_to_checkout_context
                         pred_context_hash))
          in
          let*! protocol_hash = Context.get_protocol predecessor_context in
          let* () = load_protocol protocol_hash protocol_root in
          with_retry_to_load_protocol protocol_root (fun () ->
              Block_validation.preapply
                ~chain_id
                ~user_activated_upgrades
                ~user_activated_protocol_overrides
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
                operations)
        in
        let*! cachable_result =
          match block_preapplication_result with
          | Ok (res, last_preapplied_context) ->
              let*! () =
                External_validation.send
                  output
                  (Error_monad.result_encoding
                     Block_validation.preapply_result_encoding)
                  (Ok res)
              in
              Lwt.return_some last_preapplied_context
          | Error _ as err ->
              let*! () =
                External_validation.send
                  output
                  (Error_monad.result_encoding
                     Block_validation.preapply_result_encoding)
                  err
              in
              Lwt.return_none
        in
        loop cache cachable_result
    | External_validation.Precheck
        {
          chain_id;
          predecessor_block_header;
          predecessor_block_hash;
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
                    predecessor_block_header.shell.context
                in
                match o with
                | Some context -> return context
                | None ->
                    fail
                      (Block_validator_errors.Failed_to_checkout_context
                         predecessor_block_header.shell.context))
          in
          let cache =
            match cache with
            | None -> `Lazy
            | Some cache ->
                `Inherited (cache, predecessor_block_header.shell.context)
          in
          Block_validation.precheck
            ~chain_id
            ~predecessor_block_header
            ~predecessor_block_hash
            ~predecessor_context
            ~cache
            header
            operations
        in
        let*! () =
          External_validation.send
            output
            (Error_monad.result_encoding Data_encoding.unit)
            block_precheck_result
        in
        loop cache cached_result
    | External_validation.Fork_test_chain {context_hash; forked_header} ->
        let*! () = Events.(emit fork_test_chain_request forked_header) in
        let*! context_opt = Context.checkout context_index context_hash in
        let*! () =
          match context_opt with
          | Some ctxt ->
              let*! test_chain_init_result =
                with_retry_to_load_protocol protocol_root (fun () ->
                    Block_validation.init_test_chain ctxt forked_header)
              in
              External_validation.send
                output
                (Error_monad.result_encoding Block_header.encoding)
                test_chain_init_result
          | None ->
              External_validation.send
                output
                (Error_monad.result_encoding Data_encoding.empty)
                (error
                   (Block_validator_errors.Failed_to_checkout_context
                      context_hash))
        in
        loop cache None
    | External_validation.Terminate ->
        let*! () = Lwt_io.flush_all () in
        Events.(emit termination_request ())
    | External_validation.Reconfigure_event_logging config ->
        let*! res =
          Tezos_base_unix.Internal_event_unix.Configuration.reapply config
        in
        let*! () =
          External_validation.send
            output
            (Error_monad.result_encoding Data_encoding.empty)
            res
        in
        loop cache None
  in
  let*! () = loop None None in
  return_unit

let main ?socket_dir () =
  let open Lwt_result_syntax in
  let canceler = Lwt_canceler.create () in
  let*! (in_channel, out_channel) =
    match socket_dir with
    | Some socket_dir ->
        let*! () = Tezos_base_unix.Internal_event_unix.init () in
        let pid = Unix.getpid () in
        let socket_path = External_validation.socket_path ~socket_dir ~pid in
        let*! socket_process =
          External_validation.create_socket_connect ~canceler ~socket_path
        in
        let socket_in = Lwt_io.of_fd ~mode:Input socket_process in
        let socket_out = Lwt_io.of_fd ~mode:Output socket_process in
        Lwt.return (socket_in, socket_out)
    | None -> Lwt.return (Lwt_io.stdin, Lwt_io.stdout)
  in
  let*! () = Events.(emit initialized ()) in
  let*! r =
    Error_monad.catch_es (fun () ->
        let* () = run in_channel out_channel in
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
