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

let get_context index hash =
  Context.checkout index hash
  >>= function
  | None ->
      fail (Block_validator_errors.Failed_to_checkout_context hash)
  | Some ctx ->
      return ctx

type proto_status = Embeded | Dynlinked

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
  Fork_validation.recv stdin Data_encoding.Variable.bytes
  >>= fun magic ->
  fail_when
    (not (Bytes.equal magic Fork_validation.magic))
    (inconsistent_handshake "bad magic") >>=? fun () ->
  Fork_validation.recv stdin Fork_validation.parameters_encoding
  >>= fun {context_root; protocol_root} ->
  let genesis_block = ref Block_hash.zero in
  let genesis_time = ref Time.Protocol.epoch in
  let genesis_protocol = ref Protocol_hash.zero in
  let patch_context ctxt =
    let module Proto = (val Registered_protocol.get_exn !genesis_protocol) in
    let ctxt = Shell_context.wrap_disk_context ctxt in
    Proto.init
      ctxt
      {
        level = 0l;
        proto_level = 0;
        predecessor = !genesis_block;
        timestamp = !genesis_time;
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        fitness = [];
        context = Context_hash.zero;
      }
    >>= function
    | Error _ ->
        assert false (* FIXME error *)
    | Ok {context; _} ->
        let context = Shell_context.unwrap_disk_context context in
        Lwt.return context
  in
  Context.init ~patch_context context_root
  >>= fun context_index ->
  let rec loop () =
    Fork_validation.recv stdin Fork_validation.request_encoding
    >>= (function
          | Fork_validation.Validate
              { chain_id;
                block_header;
                predecessor_block_header;
                operations;
                max_operations_ttl } ->
              Error_monad.protect (fun () ->
                  get_context
                    context_index
                    predecessor_block_header.shell.context
                  >>=? fun predecessor_context ->
                  Context.get_protocol predecessor_context
                  >>= fun protocol_hash ->
                  load_protocol protocol_hash protocol_root
                  >>=? fun () ->
                  Block_validation.apply
                    chain_id
                    ~max_operations_ttl
                    ~predecessor_block_header
                    ~predecessor_context
                    ~block_header
                    operations)
              >>= fun result ->
              Fork_validation.send
                stdout
                (Error_monad.result_encoding Block_validation.result_encoding)
                result
          | Fork_validation.Commit_genesis
              {chain_id; time; genesis_hash; protocol} ->
              genesis_time := time ;
              genesis_block := genesis_hash ;
              genesis_protocol := protocol ;
              Error_monad.protect (fun () ->
                  Context.commit_genesis
                    context_index
                    ~chain_id
                    ~time
                    ~protocol
                  >>= fun commit -> return commit)
              >>= fun commit ->
              Fork_validation.send
                stdout
                (Error_monad.result_encoding Context_hash.encoding)
                commit
          | Fork_validation.Init ->
              Fork_validation.send
                stdout
                (Error_monad.result_encoding Data_encoding.empty)
                (Ok ())
          | Fork_validation.Terminate ->
              Lwt_io.flush_all () >>= fun () -> exit 0)
    >>= fun () -> loop ()
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
      Fork_validation.send
        stdout
        (Error_monad.result_encoding Data_encoding.unit)
        errs
      >>= fun () -> Lwt.return 1
