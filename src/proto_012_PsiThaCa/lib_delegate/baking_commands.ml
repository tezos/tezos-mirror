(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Client_proto_args

let pidfile_arg =
  Clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Clic.parameter (fun _ s -> return s))

let may_lock_pidfile pidfile_opt f =
  match pidfile_opt with
  | None -> f ()
  | Some pidfile ->
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () ->
          failwith "Failed to create the pidfile: %s" pidfile)
        ~filename:pidfile
        f

let http_headers =
  match Sys.getenv_opt "TEZOS_REMOTE_MEMPOOL_HTTP_HEADERS" with
  | None -> None
  | Some contents ->
      let lines = String.split_on_char '\n' contents in
      Some
        (List.fold_left
           (fun acc line ->
             match String.index_opt line ':' with
             | None ->
                 Stdlib.failwith
                   "Http headers: invalid TEZOS_REMOTE_MEMPOOL_HTTP_HEADERS \
                    environment variable, missing colon"
             | Some pos ->
                 let header = String.trim (String.sub line 0 pos) in
                 let header = String.lowercase_ascii header in
                 if
                   header <> "host"
                   && (String.length header < 2 || String.sub header 0 2 <> "x-")
                 then
                   Stdlib.failwith
                     "Http headers: invalid TEZOS_REMOTE_MEMPOOL_HTTP_HEADERS \
                      environment variable, only 'host' or 'x-' headers are \
                      supported" ;
                 let value =
                   String.trim
                     (String.sub line (pos + 1) (String.length line - pos - 1))
                 in
                 (header, value) :: acc)
           []
           lines)

let check_endpoint_validity uri =
  match Uri.scheme uri with
  | Some "http" | Some "https" -> ()
  | None ->
      Stdlib.failwith "no scheme detected, http and https scheme are required"
  | Some x ->
      Printf.ksprintf
        Stdlib.failwith
        "invalid scheme '%s' only http and https endpoints are supported"
        x

let mempool_arg =
  Clic.arg
    ~long:"mempool"
    ~placeholder:"file"
    ~doc:
      "When specified, the baker will try to fetch a mempool from this file \
       (or uri) and will try to include the retrieved operations in the block. \
       The expected format of the content is of the form of the \
       '/chains/<chain_id>/mempool/pending_operations' RPC. Environment \
       variable 'TEZOS_REMOTE_MEMPOOL_HTTP_HEADERS' may also be specified to \
       add headers to the requests (only 'host' and custom 'x-...' headers are \
       supported)."
    (Clic.map_parameter
       ~f:(fun uri ->
         let open Baking_configuration in
         let path = Uri.to_string uri in
         if Sys.file_exists path then Mempool.(Local {filename = path})
         else (
           check_endpoint_validity uri ;
           Mempool.(Remote {uri; http_headers})))
       uri_parameter)

let context_path_arg =
  Clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:
      "When specified, the client will read in the local context at the \
       provided path in order to build the block, instead of relying on the \
       'preapply' RPC."
    string_parameter

let endorsement_force_switch_arg =
  Clic.switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "Disable consistency, injection and double signature checks for \
       (pre)endorsements."
    ()

let do_not_monitor_node_mempool_arg =
  Clic.switch
    ~long:"ignore-node-mempool"
    ~doc:
      "Ignore mempool operations from the node and do not subsequently monitor \
       them. Use in conjunction with --mempool option to restrict the observed \
       operations to those of the mempool file."
    ()

let keep_alive_arg =
  Clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let liquidity_baking_escape_vote_switch =
  Clic.switch
    ~doc:"Vote to end the liquidity baking subsidy."
    ~long:"liquidity-baking-escape-vote"
    ()

let get_delegates (cctxt : Protocol_client_context.full)
    (pkhs : Signature.public_key_hash list) =
  let proj_delegate (alias, public_key_hash, public_key, secret_key_uri) =
    {
      Baking_state.alias = Some alias;
      public_key_hash;
      public_key;
      secret_key_uri;
    }
  in
  (if pkhs = [] then
   Client_keys.get_keys cctxt >>=? fun keys ->
   List.map proj_delegate keys |> return
  else
    List.map_es
      (fun pkh ->
        Client_keys.get_key cctxt pkh >>=? function
        | (alias, pk, sk_uri) -> return (proj_delegate (alias, pkh, pk, sk_uri)))
      pkhs)
  >>=? fun delegates ->
  Tezos_signer_backends.Encrypted.decrypt_list
    cctxt
    (List.filter_map
       (function
         | {Baking_state.alias = Some alias; _} -> Some alias | _ -> None)
       delegates)
  >>=? fun () ->
  let delegates_no_duplicates = List.sort_uniq compare delegates in
  (if Compare.List_lengths.(delegates <> delegates_no_duplicates) then
   cctxt#warning
     "Warning: the list of public key hash aliases contains duplicate hashes, \
      which are ignored"
  else Lwt.return ())
  >>= fun () -> return delegates_no_duplicates

let sources_param =
  Clic.seq_of_param
    (Client_keys.Public_key_hash.source_param
       ~name:"baker"
       ~desc:"name of the delegate owning the endorsement right")

let delegate_commands () : Protocol_client_context.full Clic.command list =
  let open Clic in
  let group =
    {name = "delegate.client"; title = "Tenderbake client commands"}
  in
  [
    command
      ~group
      ~desc:"Forge and inject block using the delegates' rights."
      (args8
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_switch
         mempool_arg
         context_path_arg
         do_not_monitor_node_mempool_arg)
      (prefixes ["bake"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force,
             mempool,
             context_path,
             do_not_monitor_node_mempool )
           pkhs
           cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.bake
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ~force
          ~monitor_node_mempool:(not do_not_monitor_node_mempool)
          ?mempool
          ?context_path
          delegates);
    command
      ~group
      ~desc:"Forge and inject an endorsement operation."
      (args1 endorsement_force_switch_arg)
      (prefixes ["endorse"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.endorse ~force cctxt delegates);
    command
      ~group
      ~desc:"Forge and inject a preendorsement operation."
      (args1 endorsement_force_switch_arg)
      (prefixes ["preendorse"; "for"] @@ sources_param)
      (fun force pkhs cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.preendorse ~force cctxt delegates);
    command
      ~group
      ~desc:"Send a Tenderbake proposal"
      (args7
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_switch
         mempool_arg
         context_path_arg)
      (prefixes ["propose"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force,
             mempool,
             context_path )
           sources
           cctxt ->
        get_delegates cctxt sources >>=? fun delegates ->
        Baking_lib.propose
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ~force
          ?mempool
          ?context_path
          delegates);
  ]

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let per_block_vote_file_arg =
  Clic.arg
    ~doc:"read per block votes as json file"
    ~short:'V'
    ~long:"votefile"
    ~placeholder:"filename"
    (Clic.parameter (fun _ s -> return s))

let baker_commands () : Protocol_client_context.full Clic.command list =
  let open Clic in
  let group =
    {
      Clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      (args7
         pidfile_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         keep_alive_arg
         liquidity_baking_escape_vote_switch
         per_block_vote_file_arg)
      (prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"node_data_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ sources_param)
      (fun ( pidfile,
             minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             keep_alive,
             liquidity_baking_escape_vote,
             per_block_vote_file )
           node_data_path
           sources
           cctxt ->
        may_lock_pidfile pidfile @@ fun () ->
        get_delegates cctxt sources >>=? fun delegates ->
        let context_path = Filename.Infix.(node_data_path // "context") in
        Client_daemon.Baker.run
          cctxt
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ~liquidity_baking_escape_vote
          ?per_block_vote_file
          ~chain:cctxt#chain
          ~context_path
          ~keep_alive
          delegates);
  ]

let accuser_commands () =
  let open Clic in
  let group =
    {
      Clic.name = "delegate.accuser";
      title = "Commands related to the accuser daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg Client_proto_args.preserved_levels_arg keep_alive_arg)
      (prefixes ["run"] @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        let preserved_levels = Option.value ~default:200 preserved_levels in
        may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.Accuser.run
          cctxt
          ~chain:cctxt#chain
          ~preserved_levels
          ~keep_alive);
  ]
