(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let ( // ) = Filename.concat

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

let inconsistent_handshake msg =
  Block_validator_errors.(
    Validation_process_failed (Inconsistent_handshake msg))

let run stdin stdout =
  External_validation.recv stdin Data_encoding.Variable.bytes
  >>= fun magic ->
  fail_when
    (not (Bytes.equal magic External_validation.magic))
    (inconsistent_handshake "bad magic")
  >>=? fun () ->
  External_validation.recv stdin External_validation.parameters_encoding
  >>= fun { context_root;
            protocol_root;
            sandbox_parameters;
            genesis;
            user_activated_upgrades;
            user_activated_protocol_overrides } ->
  let sandbox_param =
    Option.map ~f:(fun p -> ("sandbox_parameter", p)) sandbox_parameters
  in
  Context.init
    ~patch_context:(Patch_context.patch_context genesis sandbox_param)
    context_root
  >>= fun context_index ->
  let rec loop () =
    External_validation.recv stdin External_validation.request_encoding
    >>= (function
          | External_validation.Validate
              { chain_id;
                block_header;
                predecessor_block_header;
                operations;
                max_operations_ttl } ->
              Error_monad.protect (fun () ->
                  let pred_context_hash =
                    predecessor_block_header.shell.context
                  in
                  Context.checkout context_index pred_context_hash
                  >>= function
                  | Some context ->
                      return context
                  | None ->
                      fail
                        (Block_validator_errors.Failed_to_checkout_context
                           pred_context_hash))
              >>=? (fun predecessor_context ->
                     Context.get_protocol predecessor_context
                     >>= fun protocol_hash ->
                     load_protocol protocol_hash protocol_root
                     >>=? fun () ->
                     Block_validation.apply
                       chain_id
                       ~user_activated_upgrades
                       ~user_activated_protocol_overrides
                       ~max_operations_ttl
                       ~predecessor_block_header
                       ~predecessor_context
                       ~block_header
                       operations
                     >>= function
                     | Error
                         [ Block_validator_errors.Unavailable_protocol
                             {protocol; _} ] as err -> (
                         (* If `next_protocol` is missing, try to load it *)
                         load_protocol protocol protocol_root
                         >>= function
                         | Error _ ->
                             Lwt.return err
                         | Ok () ->
                             Block_validation.apply
                               chain_id
                               ~user_activated_upgrades
                               ~user_activated_protocol_overrides
                               ~max_operations_ttl
                               ~predecessor_block_header
                               ~predecessor_context
                               ~block_header
                               operations )
                     | result ->
                         Lwt.return result)
              >>= fun res ->
              External_validation.send
                stdout
                (Error_monad.result_encoding Block_validation.result_encoding)
                res
              >>= return
          | External_validation.Commit_genesis {chain_id} ->
              Error_monad.protect (fun () ->
                  Context.commit_genesis
                    context_index
                    ~chain_id
                    ~time:genesis.time
                    ~protocol:genesis.protocol
                  >>= fun commit -> return commit)
              >>=? fun commit ->
              External_validation.send
                stdout
                (Error_monad.result_encoding Context_hash.encoding)
                commit
              >>= return
          | External_validation.Init ->
              External_validation.send
                stdout
                (Error_monad.result_encoding Data_encoding.empty)
                (Ok ())
              >>= return
          | External_validation.Fork_test_chain {context_hash; forked_header}
            ->
              Context.checkout context_index context_hash
              >>= (function
                    | Some ctxt ->
                        Block_validation.init_test_chain ctxt forked_header
                        >>= (function
                              | Error
                                  [ Block_validator_errors.Missing_test_protocol
                                      protocol ] ->
                                  load_protocol protocol protocol_root
                                  >>=? fun () ->
                                  Block_validation.init_test_chain
                                    ctxt
                                    forked_header
                              | result ->
                                  Lwt.return result)
                        >>= fun result ->
                        External_validation.send
                          stdout
                          (Error_monad.result_encoding Block_header.encoding)
                          result
                    | None ->
                        External_validation.send
                          stdout
                          (Error_monad.result_encoding Data_encoding.empty)
                          (error
                             (Block_validator_errors.Failed_to_checkout_context
                                context_hash)))
              >>= return
          | External_validation.Terminate ->
              Lwt_io.flush_all () >>= fun () -> exit 0
          | External_validation.Restore_context_integrity ->
              let res = Context.restore_integrity context_index in
              External_validation.send
                stdout
                (Error_monad.result_encoding Data_encoding.(option int31))
                res
              >>= return)
    >>=? loop
  in
  loop ()

let main () =
  let stdin = Lwt_io.of_fd ~mode:Input Lwt_unix.stdin in
  let stdout = Lwt_io.of_fd ~mode:Output Lwt_unix.stdout in
  Lwt.catch
    (fun () -> run stdin stdout >>=? fun () -> return 0)
    (fun e -> Lwt.return (error_exn e))
  >>= function
  | Ok v ->
      Lwt.return v
  | Error _ as errs ->
      External_validation.send
        stdout
        (Error_monad.result_encoding Data_encoding.unit)
        errs
      >>= fun () -> Lwt.return 1
