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

include Persistence_intf

(** The regexp that tests if a protocol hash is the one of Alpha.

      This inelegant design is the best we could think of that is resilient
      in the face of protocol changes: we don't want to have to change code in
      each protocol every time the Alpha protocol changes.
  *)
let is_proto_alpha_regexp = Re.Str.regexp_case_fold ".*ProtoAlpha.*"

module Make (Registration : Registration.S) = struct
  include Persistence_intf

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

  module Persistent_mockup_environment = struct
    type t = {
      protocol_hash : Protocol_hash.t;
      chain_id : Chain_id.t;
      rpc_context : Tezos_protocol_environment.rpc_context;
      protocol_data : bytes;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun {protocol_hash; chain_id; rpc_context; protocol_data} ->
          (protocol_hash, chain_id, rpc_context, protocol_data))
        (fun (protocol_hash, chain_id, rpc_context, protocol_data) ->
          {protocol_hash; chain_id; rpc_context; protocol_data})
        (obj4
           (req "protocol_hash" Protocol_hash.encoding)
           (req "chain_id" Chain_id.encoding)
           (req "context" rpc_context_encoding)
           (req "protocol_data" Variable.bytes))

    let to_json = Data_encoding.Json.construct encoding

    let of_json = Data_encoding.Json.destruct encoding
  end

  let get_registered_mockup (protocol_hash_opt : Protocol_hash.t option)
      (printer : #Tezos_client_base.Client_context.printer) :
      Registration.mockup_environment tzresult Lwt.t =
    let mockup_environments = Registration.get_registered_environments () in
    (match protocol_hash_opt with
    | Some protocol_hash ->
        Lwt.return (fun (module Mockup : Registration.MOCKUP) ->
            Protocol_hash.equal protocol_hash Mockup.protocol_hash)
    | None ->
        printer#warning
          "No protocol specified: using Alpha as default protocol."
        >|= fun () (module Mockup : Registration.MOCKUP) ->
        Re.Str.string_match
          is_proto_alpha_regexp
          (Protocol_hash.to_b58check Mockup.protocol_hash)
          0)
    >>= fun criterion ->
    match List.find criterion mockup_environments with
    | Some mockup -> return mockup
    | None ->
        let requested_protocol =
          match protocol_hash_opt with
          | Some requested ->
              Format.asprintf
                "Requested protocol with hash %a"
                Protocol_hash.pp
                requested
          | None -> "Default protocol Alpha (no requested protocol)"
        in
        let protocol_hashes =
          List.map
            (fun (module Mockup : Registration.MOCKUP) -> Mockup.protocol_hash)
            mockup_environments
        in
        failwith
          "%s not found in available mockup environments. Available protocol \
           hashes: [%a]"
          requested_protocol
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ", ")
              Protocol_hash.pp)
          protocol_hashes

  let default_mockup_context :
      Tezos_client_base.Client_context.printer ->
      (Registration.mockup_environment * Registration.mockup_context) tzresult
      Lwt.t =
   fun cctxt ->
    get_registered_mockup None cctxt >>=? fun mockup ->
    let (module Mockup) = mockup in
    Mockup.init
      ~cctxt
      ~parameters:Mockup.default_parameters
      ~constants_overrides_json:None
      ~bootstrap_accounts_json:None
    >>=? fun rpc_context -> return (mockup, rpc_context)

  let init_mockup_context_by_protocol_hash :
      cctxt:Tezos_client_base.Client_context.printer ->
      protocol_hash:Protocol_hash.t ->
      constants_overrides_json:Data_encoding.json option ->
      bootstrap_accounts_json:Data_encoding.json option ->
      (Registration.mockup_environment * Registration.mockup_context) tzresult
      Lwt.t =
   fun ~cctxt ~protocol_hash ~constants_overrides_json ~bootstrap_accounts_json ->
    get_registered_mockup (Some protocol_hash) cctxt >>=? fun mockup ->
    let (module Mockup) = mockup in
    Mockup.init
      ~cctxt
      ~parameters:Mockup.default_parameters
      ~constants_overrides_json
      ~bootstrap_accounts_json
    >>=? fun menv -> return (mockup, menv)

  let mockup_context_from_persisted
      ({protocol_hash; chain_id; rpc_context; protocol_data} :
        Persistent_mockup_environment.t)
      (printer : #Tezos_client_base.Client_context.printer) =
    get_registered_mockup (Some protocol_hash) printer >>=? fun mockup ->
    return
      ( mockup,
        Tezos_mockup_registration.Registration_intf.
          {chain = chain_id; rpc_context; protocol_data} )

  let get_mockup_context_from_disk ~base_dir ~protocol_hash
      (printer : #Tezos_client_base.Client_context.printer) =
    let file = (Files.Context.get ~dirname:base_dir :> string) in
    if not (Sys.file_exists file) then
      failwith "get_mockup_context_from_disk: file %s not found" file
    else
      Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file file
      >>=? fun context_json ->
      match Persistent_mockup_environment.of_json context_json with
      | persisted_mockup ->
          let ({
                 timestamp = predecessor_timestamp;
                 level = predecessor_level;
                 fitness = predecessor_fitness;
                 _;
               }
                : Block_header.shell_header) =
            persisted_mockup.rpc_context.block_header
          in
          let timestamp =
            Time.System.to_protocol (Tezos_stdlib_unix.Systime_os.now ())
          in
          let predecessor = persisted_mockup.rpc_context.block_hash in
          get_registered_mockup (Some persisted_mockup.protocol_hash) printer
          >>=? fun (module Mockup) ->
          (*
             In the mockup mode, reactivity is important and there are
             no constraints to be consistent with other nodes. For this
             reason, the mockup mode loads the cache lazily.
             See {!Environment_context.source_of_cache}.
          *)
          Mockup.Protocol.value_of_key
            ~chain_id:persisted_mockup.chain_id
            ~predecessor_context:persisted_mockup.rpc_context.context
            ~predecessor_timestamp
            ~predecessor_level
            ~predecessor_fitness
            ~predecessor
            ~timestamp
          >>=? fun value_of_key ->
          Tezos_protocol_environment.Context.load_cache
            predecessor
            persisted_mockup.rpc_context.context
            `Lazy
            value_of_key
          >>=? fun context ->
          let persisted_mockup =
            {
              persisted_mockup with
              rpc_context = {persisted_mockup.rpc_context with context};
            }
          in
          mockup_context_from_persisted persisted_mockup printer
          >>=? fun (((module Mockup_environment), _) as res) ->
          (match protocol_hash with
          | None -> return_unit
          | Some desired_protocol
            when Protocol_hash.equal
                   Mockup_environment.protocol_hash
                   desired_protocol ->
              return_unit
          | Some desired_protocol ->
              failwith
                "Protocol %a was requested via --protocol\n\
                 yet the mockup at %s was initialized with %a"
                Protocol_hash.pp_short
                desired_protocol
                base_dir
                Protocol_hash.pp_short
                Mockup_environment.protocol_hash)
          >>=? fun () -> return res
      | exception _e ->
          failwith "get_mockup_context_from_disk: could not read %s" file

  let overwrite_mockup ~protocol_hash ~chain_id ~rpc_context ~protocol_data
      ~base_dir =
    let context_file = (Files.Context.get ~dirname:base_dir :> string) in
    if not (Sys.file_exists context_file) then
      failwith "overwrite_mockup: file %s does not exist" context_file
    else
      Persistent_mockup_environment.(
        to_json {protocol_hash; chain_id; rpc_context; protocol_data})
      |> Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file context_file

  type base_dir_class =
    | Base_dir_does_not_exist
    | Base_dir_is_file
    | Base_dir_is_mockup
    | Base_dir_is_nonempty
    | Base_dir_is_empty

  let pp_base_dir_class ppf bclass =
    Format.fprintf
      ppf
      "base_dir_%s"
      ((function
         | Base_dir_does_not_exist -> "does_not_exist"
         | Base_dir_is_file -> "is_file"
         | Base_dir_is_mockup -> "is_mockup"
         | Base_dir_is_empty -> "is_empty"
         | Base_dir_is_nonempty -> "is_non_empty")
         bclass)

  let is_directory_empty dir = Array.length (Sys.readdir dir) = 0

  let classify_base_dir base_dir =
    if not (Sys.file_exists base_dir) then return Base_dir_does_not_exist
    else if not (Sys.is_directory base_dir) then return Base_dir_is_file
    else if is_directory_empty base_dir then return Base_dir_is_empty
    else
      let dirname = base_dir in
      Files.exists_mockup_directory ~dirname >>= function
      | true -> (
          Files.Context.exists ~dirname >>= function
          | true -> return Base_dir_is_mockup
          | false -> return Base_dir_is_nonempty)
      | false -> return Base_dir_is_nonempty

  let create_mockup ~(cctxt : Tezos_client_base.Client_context.full)
      ~protocol_hash ~constants_overrides_json ~bootstrap_accounts_json
      ~asynchronous =
    let base_dir = cctxt#get_base_dir in
    let create_base_dir () =
      Tezos_stdlib_unix.Lwt_utils_unix.create_dir base_dir >>= fun () ->
      cctxt#message "Created mockup client base dir in %s" base_dir
      >>= fun () -> return_unit
    in
    (classify_base_dir base_dir >>=? function
     | Base_dir_does_not_exist | Base_dir_is_empty -> create_base_dir ()
     | Base_dir_is_file -> failwith "%s is a file" base_dir
     | Base_dir_is_mockup ->
         failwith "%s is already initialized as a mockup directory" base_dir
     | Base_dir_is_nonempty ->
         failwith
           "%s is not empty, please specify a fresh base directory"
           base_dir)
    >>=? fun () ->
    init_mockup_context_by_protocol_hash
      ~cctxt:(cctxt :> Tezos_client_base.Client_context.printer)
      ~protocol_hash
      ~constants_overrides_json
      ~bootstrap_accounts_json
    >>=? fun (_mockup_env, {chain = chain_id; rpc_context; protocol_data}) ->
    let mockup_dir = (Files.get_mockup_directory ~dirname:base_dir :> string) in
    Tezos_stdlib_unix.Lwt_utils_unix.create_dir mockup_dir >>= fun () ->
    let context_file = (Files.Context.get ~dirname:base_dir :> string) in
    Persistent_mockup_environment.(
      to_json {protocol_hash; chain_id; rpc_context; protocol_data})
    |> Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file context_file
    >>=? fun () ->
    if asynchronous then
      (* Setup a local persistent mempool *)
      let mempool_file = (Files.Mempool.get ~dirname:base_dir :> string) in
      cctxt#message "creating persistent mempool file at %s" mempool_file
      >>= fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Output mempool_file (fun oc ->
          Lwt_io.write oc "[]" >|= ok)
    else return_unit
end

module Internal_for_tests = struct
  module Make = Make
end

include Make (Registration)
