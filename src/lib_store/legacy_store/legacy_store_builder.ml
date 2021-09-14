(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Filename.Infix

(* Inlined from Tezos_shell.Patch_context to avoid cyclic dependencies *)
let patch_context (genesis : Genesis.t) key_json ctxt =
  (match key_json with
  | None -> Lwt.return ctxt
  | Some (key, json) ->
      Tezos_context.Context.add
        ctxt
        [key]
        (Data_encoding.Binary.to_bytes_exn Data_encoding.json json))
  >>= fun ctxt ->
  Registered_protocol.get_result genesis.protocol >>=? fun proto ->
  let module Proto = (val proto) in
  let ctxt = Tezos_shell_context.Shell_context.wrap_disk_context ctxt in
  Proto.init
    ctxt
    {
      level = 0l;
      proto_level = 0;
      predecessor = genesis.block;
      timestamp = genesis.time;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.empty;
      fitness = [];
      context = Context_hash.zero;
    }
  >>=? fun {context; _} ->
  return (Tezos_shell_context.Shell_context.unwrap_disk_context context)

let load_protocol proto protocol_root =
  if Registered_protocol.mem proto then return_unit
  else
    let cmxs_file =
      protocol_root
      // Protocol_hash.to_short_b58check proto
      // Format.asprintf "protocol_%a" Protocol_hash.pp proto
    in
    try
      Dynlink.loadfile_private (cmxs_file ^ ".cmxs") ;
      return_unit
    with Dynlink.Error err ->
      Format.ksprintf
        (fun msg ->
          fail
            Block_validator_errors.(
              Validation_process_failed (Protocol_dynlink_failure msg)))
        "Cannot load file: %s. (Expected location: %s.)"
        (Dynlink.error_message err)
        cmxs_file

(* From "legacy chain_validator"*)
let may_update_checkpoint chain_state new_head =
  Legacy_state.Chain.checkpoint chain_state >>= fun checkpoint ->
  Legacy_state.Block.last_allowed_fork_level new_head >>=? fun new_level ->
  if new_level <= checkpoint.shell.level then return_unit
  else
    let state = Legacy_state.Chain.global_state chain_state in
    Legacy_state.history_mode state >>= fun history_mode ->
    let head_level = Legacy_state.Block.level new_head in
    Legacy_state.Block.predecessor_n
      new_head
      (Int32.to_int (Int32.sub head_level new_level))
    >>= function
    | None -> assert false (* should not happen *)
    | Some new_checkpoint -> (
        Legacy_state.Block.read_opt chain_state new_checkpoint >>= function
        | None -> assert false (* should not happen *)
        | Some new_checkpoint -> (
            let new_checkpoint = Legacy_state.Block.header new_checkpoint in
            match history_mode with
            | History_mode.Legacy.Archive ->
                Legacy_state.Chain.set_checkpoint chain_state new_checkpoint
                >>= fun () -> return_unit
            | Full ->
                Legacy_state.Chain.set_checkpoint_then_purge_full
                  chain_state
                  new_checkpoint
            | Rolling ->
                Legacy_state.Chain.set_checkpoint_then_purge_rolling
                  chain_state
                  new_checkpoint))

let generate identity_file pow =
  let target = Crypto_box.make_pow_target pow in
  Format.eprintf "Generating a new identity... (level: %.2f) " pow ;
  P2p_identity.generate_with_bound target >>= fun id ->
  Lwt_utils_unix.Json.write_file
    identity_file
    (Data_encoding.Json.construct P2p_identity.encoding id)
  >>=? fun () -> return_unit

let dump_config data_dir =
  (* version *)
  let data_version = "0.0.4" in
  let version_file_name = "version.json" in
  let version_file data_dir = data_dir // version_file_name in
  let write_version_file data_dir =
    let version_file = version_file data_dir in
    let version_encoding = Data_encoding.(obj1 (req "version" string)) in
    Lwt_utils_unix.Json.write_file
      version_file
      (Data_encoding.Json.construct version_encoding data_version)
  in
  write_version_file data_dir >>=? fun () ->
  (* identity *)
  let identity_file_name = "identity.json" in
  let identity_file = data_dir // identity_file_name in
  generate identity_file 0. >>=? fun () ->
  (* config *)
  (* TODO write a config ? *)
  let _config_file_name = "config.json" in
  (* TODO write some peers ? *)
  let _peers_file_name = "peers.json" in
  (* TODO protocols ? *)
  let _protocol_dir data_dir = data_dir // "protocol" in
  return_unit

let run () =
  let ok msg =
    External_validation.send Lwt_io.stdout Data_encoding.string (msg ^ "\n")
    >>= fun () -> Lwt_io.flush_all () >>= return
  in
  Error_monad.protect
    (fun () ->
      let usg_msg = Format.sprintf "Usage: %s <data_dir>" Sys.argv.(0) in
      let output_dir = ref None in
      Arg.parse
        []
        (fun dir ->
          if !output_dir <> None then raise (Arg.Bad usg_msg)
          else output_dir := Some dir)
        usg_msg ;
      let output_dir =
        WithExceptions.Option.to_exn
          ~none:(Invalid_argument usg_msg)
          !output_dir
      in
      if Sys.file_exists output_dir then
        Format.ksprintf Stdlib.failwith "%s already exists" output_dir ;
      Lwt_utils_unix.create_dir output_dir >>= fun () ->
      dump_config output_dir >>=? fun () ->
      let store_root = Filename.concat output_dir "store" in
      let context_root = Filename.concat output_dir "context" in
      (* Start listening for messages on stdin *)
      External_validation.recv
        Lwt_io.stdin
        Data_encoding.(
          tup2
            History_mode.Legacy.encoding
            External_validation.parameters_encoding)
      >>= fun ( history_mode,
                {
                  External_validation.context_root = _;
                  protocol_root;
                  sandbox_parameters;
                  genesis;
                  user_activated_upgrades;
                  user_activated_protocol_overrides;
                } ) ->
      let sandbox_param =
        Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
      in
      let patch_context ctxt = patch_context genesis sandbox_param ctxt in
      (* TODO parametrize this *)
      Legacy_state.init
        ~patch_context
        ~store_root
        ~context_root
        ~history_mode
        ~readonly:false
        genesis
      >>=? fun (state, chain, context_index, _history_mode) ->
      (* Storing protocols *)
      Seq.iter_es
        (fun (module P : Registered_protocol.T) ->
          let proto =
            Registered_protocol.get_embedded_sources P.hash
            |> WithExceptions.Option.get ~loc:__LOC__
          in
          Legacy_state.Protocol.store state proto >>= function
          | None ->
              Format.kasprintf
                ok
                "could not store protocol %a"
                Protocol_hash.pp
                P.hash
          | Some p ->
              Format.kasprintf ok "stored protocol %a" Protocol_hash.pp p)
        (Registered_protocol.seq ())
      >>=? fun () ->
      let rec loop () =
        External_validation.recv
          Lwt_io.stdin
          External_validation.request_encoding
        >>= function
        | External_validation.Fork_test_chain _ (* TODO *)
        | External_validation.Init | External_validation.Commit_genesis _
        (* commit_genesis is done by [Legacy_state.init] *)
        | External_validation.Restore_context_integrity ->
            (* noop *) ok "noop"
        | External_validation.Terminate -> ok "exiting" >>=? fun () -> exit 0
        | External_validation.Validate
            {
              chain_id;
              block_header;
              predecessor_block_header;
              operations;
              max_operations_ttl;
              predecessor_block_metadata_hash;
              predecessor_ops_metadata_hash;
            } ->
            let pred_context_hash = predecessor_block_header.shell.context in
            ( (Context.checkout context_index pred_context_hash >>= function
               | Some context -> return context
               | None ->
                   fail
                     (Block_validator_errors.Failed_to_checkout_context
                        pred_context_hash))
            >>=? fun predecessor_context ->
              Context.get_protocol predecessor_context >>= fun protocol_hash ->
              load_protocol protocol_hash protocol_root >>=? fun () ->
              (* This call commits in the context *)
              let apply_environment =
                {
                  Block_validation.max_operations_ttl;
                  chain_id;
                  predecessor_block_header;
                  predecessor_context;
                  predecessor_block_metadata_hash;
                  predecessor_ops_metadata_hash;
                  user_activated_upgrades;
                  user_activated_protocol_overrides;
                }
              in
              Block_validation.apply
                apply_environment
                block_header
                operations
                ~cache:`Lazy
              >>= function
              | Error
                  [Block_validator_errors.Unavailable_protocol {protocol; _}] as
                err -> (
                  (* If `next_protocol` is missing, try to load it *)
                  load_protocol protocol protocol_root
                  >>= function
                  | Error _ -> Lwt.return err
                  | Ok () ->
                      Block_validation.apply
                        apply_environment
                        block_header
                        operations
                        ~cache:`Lazy)
              | result -> Lwt.return result )
            >>=? fun ( ({
                          validation_store;
                          block_metadata;
                          ops_metadata;
                          block_metadata_hash;
                          ops_metadata_hashes;
                        } as res),
                       _ ) ->
            (Context.checkout context_index validation_store.context_hash
             >>= function
             | Some context -> return context
             | None ->
                 fail
                   (Block_validator_errors.Failed_to_checkout_context
                      validation_store.context_hash))
            >>=? fun commited_context ->
            Context.get_protocol commited_context >>= fun protocol_hash ->
            Legacy_state.Chain.update_level_indexed_protocol_store
              chain
              chain_id
              block_header.shell.proto_level
              protocol_hash
              block_header
            >>= fun () ->
            Legacy_state.Block.store
              chain
              block_header
              block_metadata
              operations
              ops_metadata
              block_metadata_hash
              ops_metadata_hashes
              validation_store
              ~forking_testchain:false
            >>=? fun block ->
            let block =
              WithExceptions.Option.to_exn
                ~none:(Failure "failed to store")
                block
            in
            Legacy_chain.set_head chain block >>=? fun _prev_head ->
            may_update_checkpoint chain block >>=? fun () ->
            let msg =
              Data_encoding.Json.(
                construct Block_validation.result_encoding res |> to_string)
            in
            let block_hash = Block_header.hash block_header in
            Format.kasprintf
              ok
              "validated and stored %a: %s"
              Block_hash.pp
              block_hash
              msg
            >>=? fun () -> loop ()
      in
      loop ())
    ~on_error:(fun err ->
      Format.kasprintf ok "error: %a" pp_print_trace err >>=? fun () -> exit 1)

let () =
  match Lwt_main.run (run ()) with
  | Ok () -> ()
  | Error err ->
      Format.eprintf "%a@." pp_print_trace err ;
      exit 1
