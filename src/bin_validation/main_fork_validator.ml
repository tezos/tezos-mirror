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

let (//) = Filename.concat

let get_context index hash =
  Context.checkout index hash >>= function
  | None ->
      fail (Block_validator_errors.Failed_to_checkout_context hash)
  | Some ctx ->
      return ctx

type proto_status =
  | Embeded
  | Dynlinked

let load_protocol proto protocol_root =
  let cmxs_file = protocol_root // Protocol_hash.to_short_b58check proto //
                  Format.asprintf "protocol_%a" Protocol_hash.pp proto in
  begin
    try Dynlink.loadfile_private (cmxs_file^".cmxs") ; return_unit with
      Dynlink.Error err ->
        Format.ksprintf
          (fun msg -> fail
              Block_validator_errors.(Validation_process_failed
                                        (Protocol_dynlink_failure msg))
          )
          "Cannot load file: %s. (Expected location: %s.)"
          (Dynlink.error_message err)
          cmxs_file
  end

(* List and try to dynlink unknown protocols *)
let update_known_protocols state kp_htbl protocol_root =
  let registered_protos = Registered_protocol.list_embedded () in
  List.iter
    (fun ph ->
       if not (Hashtbl.mem kp_htbl ph) then
         Hashtbl.add kp_htbl ph Embeded)
    registered_protos;
  State.Protocol.list state >>= fun other_protos ->
  let other_protos = Protocol_hash.Set.elements other_protos in
  Error_monad.iter_s
    (fun ph ->
       if not (Hashtbl.mem kp_htbl ph) then
         begin
           load_protocol ph protocol_root >>=? fun () ->
           return (Hashtbl.add kp_htbl ph Dynlinked)
         end
       else return_unit
    )
    other_protos

let run stdin stdout =
  Fork_validation.recv stdin >>= fun data ->
  let ({ store_root ; context_root ; protocol_root ; chain_id ;
         block_header ; predecessor_block_header ; operations ;
         max_operations_ttl } : Fork_validation.fork_parameter) =
    Data_encoding.Binary.of_bytes_exn
      Fork_validation.fork_parameters_encoding
      (MBytes.of_string data) in
  begin
    try
      return (Array.get Sys.argv 1)
    with
    | Invalid_argument _ ->
        fail
          (Block_validator_errors.Validation_process_failed (Missing_handshake))
  end >>=? fun genesis_hanshake ->
  begin
    match Data_encoding.Json.from_string genesis_hanshake with
    | Ok v ->
        return (Data_encoding.Json.destruct State.Chain.genesis_encoding v)
    | Error msg ->
        fail
          Block_validator_errors.(Validation_process_failed (Inconsistent_handshake msg))
  end >>=? fun genesis ->
  State.init
    ~store_root
    ~context_root
    genesis >>=? fun (state, _chain, context_index, _history_mode) ->
  let known_protocols = Hashtbl.create 10 in
  update_known_protocols state known_protocols protocol_root >>=? fun () ->
  get_context context_index
    predecessor_block_header.shell.context >>=? fun predecessor_context ->
  Block_validation.apply
    chain_id
    ~max_operations_ttl
    ~predecessor_block_header
    ~predecessor_context
    ~block_header
    operations >>= fun result ->
  let to_send =
    Data_encoding.Binary.to_bytes_exn
      (Error_monad.result_encoding Block_validation.result_encoding)
      result in
  Fork_validation.send stdout (MBytes.to_string to_send) >>= fun () ->
  return_unit

let main () =
  let stdin = Lwt_io.of_fd ~mode:Input Lwt_unix.stdin in
  let stdout = Lwt_io.of_fd ~mode:Output Lwt_unix.stdout in
  Lwt.catch
    (fun () ->
       run stdin stdout >>=? fun () ->
       return 0)
    (fun e ->
       Lwt.return (Error_monad.error_exn e) >>= fun err ->
       let to_send = Data_encoding.Binary.to_bytes_exn
           (Error_monad.result_encoding Data_encoding.unit) err in
       Fork_validation.send stdout (MBytes.to_string to_send) >>= fun () ->
       return 1
    ) >>=
  function
  | Ok v -> Lwt.return v
  | Error _ as errs ->
      let to_send = Data_encoding.Binary.to_bytes_exn
          (Error_monad.result_encoding Data_encoding.unit) errs in
      Fork_validation.send stdout (MBytes.to_string to_send) >>= fun () ->
      Lwt.return 1

let () =
  Pervasives.exit (Lwt_main.run @@ main ())
