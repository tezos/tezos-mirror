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

type persisted_mockup_environment = {
  protocol_hash : Protocol_hash.t;
  rpc_context : Tezos_protocol_environment.rpc_context;
}

let rpc_context_encoding :
    Tezos_protocol_environment.rpc_context Data_encoding.t =
  let open Data_encoding in
  let open Tezos_protocol_environment in
  conv
    (fun {block_hash; block_header; context} ->
      (block_hash, block_header, context))
    (fun (block_hash, block_header, context) ->
      {block_hash; block_header; context})
    (obj3
       (req "block_hash" Block_hash.encoding)
       (req "shell_header" Block_header.shell_header_encoding)
       (req "context" Memory_context.encoding))

let persisted_mockup_environment_encoding :
    persisted_mockup_environment Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {protocol_hash; rpc_context} -> (protocol_hash, rpc_context))
    (fun (protocol_hash, rpc_context) -> {protocol_hash; rpc_context})
    (obj2
       (req "protocol_hash" Protocol_hash.encoding)
       (req "context" rpc_context_encoding))

let get_mockup_by_hash :
    Protocol_hash.t -> Registration.mockup_environment tzresult Lwt.t =
 fun protocol_hash ->
  let available = Registration.get_registered_contexts () in
  let mockup_opt =
    List.find_opt
      (fun (module Mockup : Tezos_mockup_registration.Registration.Mockup_sig) ->
        Protocol_hash.equal protocol_hash Mockup.protocol_hash)
      available
  in
  match mockup_opt with
  | Some mockup ->
      return mockup
  | None ->
      failwith "requested protocol not found in available mockup environments"

let default_mockup_context :
    unit ->
    (Registration.mockup_environment * Tezos_protocol_environment.rpc_context)
    tzresult
    Lwt.t =
 fun () ->
  match Registration.get_registered_contexts () with
  | [] ->
      failwith "default_mockup_context: no registered mockup environment"
  | mockup :: _ ->
      let (module Mockup) = mockup in
      Mockup.init Mockup.default_parameters
      >>=? fun rpc_context -> return (mockup, rpc_context)

let init_mockup_context_by_protocol_hash :
    Protocol_hash.t ->
    (Registration.mockup_environment * Tezos_protocol_environment.rpc_context)
    tzresult
    Lwt.t =
 fun proto_hash ->
  get_mockup_by_hash proto_hash
  >>=? fun mockup ->
  let (module Mockup) = mockup in
  Mockup.init Mockup.default_parameters
  >>=? fun rpc_context -> return (mockup, rpc_context)

let mockup_dirname = "mockup"

let context_file = "context.json"

let mockup_context_from_persisted {protocol_hash; rpc_context} =
  get_mockup_by_hash protocol_hash
  >>=? fun mockup -> return (mockup, rpc_context)

let get_mockup_context_from_disk :
    base_dir:string ->
    (Registration.mockup_environment * Tezos_protocol_environment.rpc_context)
    tzresult
    Lwt.t =
 fun ~base_dir ->
  let file =
    Filename.concat base_dir (Filename.concat mockup_dirname context_file)
  in
  if not (Sys.file_exists file) then
    failwith "get_mockup_context_from_disk: file %s not found" file
  else
    Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file
    >>=? fun context_json ->
    match
      Data_encoding.Json.destruct
        persisted_mockup_environment_encoding
        context_json
    with
    | persisted_mockup ->
        mockup_context_from_persisted persisted_mockup
    | exception _e ->
        failwith "get_mockup_context_from_disk: could not read %s" file

let overwrite_mockup ~protocol_hash ~rpc_context ~base_dir =
  let mockup_dir = Filename.concat base_dir mockup_dirname in
  let context_file = Filename.concat mockup_dir context_file in
  if not (Sys.file_exists context_file) then
    failwith "create_mockup: file %s does not exist" context_file
  else
    let json =
      Data_encoding.Json.construct
        persisted_mockup_environment_encoding
        {protocol_hash; rpc_context}
    in
    Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file context_file json

type base_dir_class =
  | Base_dir_does_not_exist
  | Base_dir_is_file
  | Base_dir_is_mockup
  | Base_dir_is_nonempty
  | Base_dir_is_empty

let is_directory_empty dir = Array.length (Sys.readdir dir) = 0

let classify_base_dir base_dir =
  if not (Sys.file_exists base_dir) then Base_dir_does_not_exist
  else if not (Sys.is_directory base_dir) then Base_dir_is_file
  else if is_directory_empty base_dir then Base_dir_is_empty
  else
    let mockup_dir = Filename.concat base_dir mockup_dirname in
    let context_file = Filename.concat mockup_dir context_file in
    if
      Sys.file_exists mockup_dir
      && Sys.is_directory mockup_dir
      && Sys.file_exists context_file
    then Base_dir_is_mockup
    else Base_dir_is_nonempty

let create_mockup ~(cctxt : Tezos_client_base.Client_context.full)
    ~protocol_hash =
  let base_dir = cctxt#get_base_dir in
  let mockup_dir = Filename.concat base_dir mockup_dirname in
  let context_file = Filename.concat mockup_dir context_file in
  let create_base_dir () =
    cctxt#message "created mockup client base dir in %s" base_dir
    >>= fun () ->
    Tezos_stdlib_unix.Lwt_utils_unix.create_dir base_dir
    >>= fun () -> return_unit
  in
  ( match classify_base_dir base_dir with
  | Base_dir_does_not_exist ->
      create_base_dir ()
  | Base_dir_is_file ->
      failwith "%s is a file" base_dir
  | Base_dir_is_mockup ->
      failwith "%s is already initialized as a mockup directory" base_dir
  | Base_dir_is_nonempty ->
      failwith
        "%s is not empty, please specify a fresh base directory"
        base_dir
  | Base_dir_is_empty ->
      create_base_dir () )
  >>=? fun () ->
  Tezos_stdlib_unix.Lwt_utils_unix.create_dir mockup_dir
  >>= fun () ->
  init_mockup_context_by_protocol_hash protocol_hash
  >>=? fun (_mockup_env, rpc_context) ->
  let json =
    Data_encoding.Json.construct
      persisted_mockup_environment_encoding
      {protocol_hash; rpc_context}
  in
  Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file context_file json
