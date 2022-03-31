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
  if Registered_protocol.mem proto then return_unit
  else
    let cmxs_file =
      protocol_root
      // Protocol_hash.to_short_b58check proto
      // Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp proto
    in
    Events.(emit dynload_protocol proto) >|= fun () ->
    try
      Dynlink.loadfile_private cmxs_file ;
      Result.return_unit
    with Dynlink.Error err ->
      Format.ksprintf
        (fun msg ->
          error
            Block_validator_errors.(
              Validation_process_failed (Protocol_dynlink_failure msg)))
        "Cannot load file: %s. (Expected location: %s.)"
        (Dynlink.error_message err)
        cmxs_file

let inconsistent_handshake msg =
  Block_validator_errors.(
    Validation_process_failed (Inconsistent_handshake msg))

let handshake input output =
  External_validation.send
    output
    Data_encoding.Variable.bytes
    External_validation.magic
  >>= fun () ->
  External_validation.recv input Data_encoding.Variable.bytes >>= fun magic ->
  fail_when
    (not (Bytes.equal magic External_validation.magic))
    (inconsistent_handshake "bad magic")

let init input =
  Events.(emit initialization_request ()) >>= fun () ->
  External_validation.recv input External_validation.parameters_encoding
  >>= fun {
            context_root;
            protocol_root;
            sandbox_parameters;
            genesis;
            user_activated_upgrades;
            user_activated_protocol_overrides;
            operation_metadata_size_limit;
          } ->
  let sandbox_param =
    Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
  in
  Context.init
    ~patch_context:(Patch_context.patch_context genesis sandbox_param)
    context_root
  >>= fun context_index ->
  Lwt.return
    ( context_index,
      protocol_root,
      genesis,
      user_activated_upgrades,
      user_activated_protocol_overrides,
      operation_metadata_size_limit )

let run input output =
  handshake input output >>=? fun () ->
  init input
  >>= fun ( context_index,
            protocol_root,
            genesis,
            user_activated_upgrades,
            user_activated_protocol_overrides,
            operation_metadata_size_limit ) ->
  let rec loop (cache : Environment_context.Context.block_cache option)
      cached_result =
    External_validation.recv input External_validation.request_encoding
    >>= function
    | External_validation.Init ->
        let init : unit Lwt.t =
          External_validation.send
            output
            (Error_monad.result_encoding Data_encoding.empty)
            (Ok ())
        in
        init >>= fun () -> loop cache None
    | External_validation.Commit_genesis {chain_id} ->
        let commit_genesis : unit Lwt.t =
          Events.(emit commit_genesis_request genesis.block) >>= fun () ->
          Error_monad.protect (fun () ->
              Context.commit_genesis
                context_index
                ~chain_id
                ~time:genesis.time
                ~protocol:genesis.protocol)
          >>= fun commit ->
          External_validation.send
            output
            (Error_monad.result_encoding Context_hash.encoding)
            commit
        in
        commit_genesis >>= fun () -> loop cache None
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
        let validate =
          Events.(emit validation_request block_header) >>= fun () ->
          ( Error_monad.protect (fun () ->
                let pred_context_hash =
                  predecessor_block_header.shell.context
                in
                Context.checkout context_index pred_context_hash >>= function
                | Some context -> return context
                | None ->
                    fail
                      (Block_validator_errors.Failed_to_checkout_context
                         pred_context_hash))
          >>=? fun predecessor_context ->
            Context.get_protocol predecessor_context >>= fun protocol_hash ->
            load_protocol protocol_hash protocol_root >>=? fun () ->
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
              }
            in
            let predecessor_context = predecessor_block_header.shell.context in
            let cache =
              match cache with
              | None -> `Load
              | Some cache -> `Inherited (cache, predecessor_context)
            in
            Block_validation.apply
              ?cached_result
              env
              block_header
              operations
              ~cache
            >>= function
            | Error [Block_validator_errors.Unavailable_protocol {protocol; _}]
              as err -> (
                (* If `next_protocol` is missing, try to load it *)
                load_protocol protocol protocol_root
                >>= function
                | Error _ -> Lwt.return err
                | Ok () ->
                    Block_validation.apply env block_header operations ~cache)
            | result -> Lwt.return result )
          >>= fun res ->
          let (res, cache) =
            match res with
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
          External_validation.send
            output
            (Error_monad.result_encoding Block_validation.result_encoding)
            res
          >>= fun () -> Lwt.return cache
        in
        validate >>= fun cache -> loop cache None
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
        let preapply =
          (* TODO event preapply *)
          (( Error_monad.protect (fun () ->
                 let pred_context_hash = predecessor_shell_header.context in
                 Context.checkout context_index pred_context_hash >>= function
                 | Some context -> return context
                 | None ->
                     fail
                       (Block_validator_errors.Failed_to_checkout_context
                          pred_context_hash))
           >>=? fun predecessor_context ->
             Context.get_protocol predecessor_context >>= fun protocol_hash ->
             load_protocol protocol_hash protocol_root >>=? fun () ->
             let preapply () =
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
                 operations
             in
             preapply () >>= function
             | Error [Block_validator_errors.Unavailable_protocol {protocol; _}]
               as err -> (
                 (* If `next_protocol` is missing, try to load it *)
                 load_protocol protocol protocol_root
                 >>= function
                 | Error _ -> Lwt.return err
                 | Ok () -> preapply ())
             | result -> Lwt.return result )
           >>= function
           | Ok (res, last_preapplied_context) ->
               External_validation.send
                 output
                 (Error_monad.result_encoding
                    Block_validation.preapply_result_encoding)
                 (Ok res)
               >>= fun () -> Lwt.return_some last_preapplied_context
           | Error _ as err ->
               External_validation.send
                 output
                 (Error_monad.result_encoding
                    Block_validation.preapply_result_encoding)
                 err
               >>= fun () -> Lwt.return_none)
          >>= fun cachable_result -> Lwt.return cachable_result
        in
        preapply >>= fun cachable_result -> loop cache cachable_result
    | External_validation.Precheck
        {
          chain_id;
          predecessor_block_header;
          predecessor_block_hash;
          header;
          operations;
          hash;
        } ->
        let validate =
          Events.(emit precheck_request hash) >>= fun () ->
          ( Error_monad.protect (fun () ->
                Context.checkout
                  context_index
                  predecessor_block_header.shell.context
                >>= function
                | Some context -> return context
                | None ->
                    fail
                      (Block_validator_errors.Failed_to_checkout_context
                         predecessor_block_header.shell.context))
          >>=? fun predecessor_context ->
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
              operations )
          >>= fun res ->
          External_validation.send
            output
            (Error_monad.result_encoding Data_encoding.unit)
            res
        in
        validate >>= fun () -> loop cache cached_result
    | External_validation.Fork_test_chain {context_hash; forked_header} ->
        let fork_test_chain : unit Lwt.t =
          Events.(emit fork_test_chain_request forked_header) >>= fun () ->
          Context.checkout context_index context_hash >>= function
          | Some ctxt ->
              (Block_validation.init_test_chain ctxt forked_header >>= function
               | Error [Block_validator_errors.Missing_test_protocol protocol]
                 ->
                   load_protocol protocol protocol_root >>=? fun () ->
                   Block_validation.init_test_chain ctxt forked_header
               | result -> Lwt.return result)
              >>= fun result ->
              External_validation.send
                output
                (Error_monad.result_encoding Block_header.encoding)
                result
          | None ->
              External_validation.send
                output
                (Error_monad.result_encoding Data_encoding.empty)
                (error
                   (Block_validator_errors.Failed_to_checkout_context
                      context_hash))
        in
        fork_test_chain >>= fun () -> loop cache None
    | External_validation.Terminate ->
        Lwt_io.flush_all () >>= fun () -> Events.(emit termination_request ())
    | External_validation.Reconfigure_event_logging config ->
        Internal_event_unix.Configuration.reapply config >>= fun res ->
        External_validation.send
          output
          (Error_monad.result_encoding Data_encoding.empty)
          res
        >>= fun () -> loop cache None
  in
  loop None None >>= fun () -> return_unit

let main ?socket_dir () =
  let canceler = Lwt_canceler.create () in
  (match socket_dir with
  | Some socket_dir ->
      Internal_event_unix.init () >>= fun () ->
      let pid = Unix.getpid () in
      let socket_path = External_validation.socket_path ~socket_dir ~pid in
      External_validation.create_socket_connect ~canceler ~socket_path
      >>= fun socket_process ->
      let socket_in = Lwt_io.of_fd ~mode:Input socket_process in
      let socket_out = Lwt_io.of_fd ~mode:Output socket_process in
      Lwt.return (socket_in, socket_out)
  | None -> Lwt.return (Lwt_io.stdin, Lwt_io.stdout))
  >>= fun (in_channel, out_channel) ->
  Events.(emit initialized ()) >>= fun () ->
  Lwt.catch
    (fun () ->
      run in_channel out_channel >>=? fun () ->
      Lwt_canceler.cancel canceler >>= function
      | Ok () | Error [] -> return_unit
      | Error (exc :: excs) ->
          let texc = TzTrace.make (Error_monad.Exn exc) in
          let texcs =
            List.map (fun exc -> TzTrace.make (Error_monad.Exn exc)) excs
          in
          let t = List.fold_left TzTrace.conp texc texcs in
          Lwt.return (Error t))
    fail_with_exn
  >>= function
  | Ok () -> Events.(emit terminated ()) >>= fun () -> return_unit
  | Error _ as errs ->
      External_validation.send
        out_channel
        (Error_monad.result_encoding Data_encoding.unit)
        errs
      >>= fun () -> Lwt.return errs
