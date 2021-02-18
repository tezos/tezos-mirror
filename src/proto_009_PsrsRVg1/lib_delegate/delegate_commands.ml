(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Client_proto_args
open Client_proto_contracts
open Client_baking_lib

let group =
  {Clic.name = "delegate"; title = "Commands related to delegate operations."}

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else return p)

let mempool_arg =
  Clic.arg
    ~long:"mempool"
    ~placeholder:"file"
    ~doc:
      "When used the client will read the mempool in the provided file \
       instead of querying the node through an RPC (useful for debugging \
       only)."
    string_parameter

let context_path_arg =
  Clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:
      "When use the client will read in the local context at the provided \
       path in order to build the block, instead of relying on the 'preapply' \
       RPC."
    string_parameter

let pidfile_arg =
  Clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Clic.parameter (fun _ s -> return s))

let may_lock_pidfile = function
  | None ->
      return_unit
  | Some pidfile ->
      trace (failure "Failed to create the pidfile: %s" pidfile)
      @@ Lwt_lock_file.create ~unlink_on_exit:true pidfile

let block_param t =
  Clic.param
    ~name:"block"
    ~desc:"commitment blocks whose nonce should be revealed"
    (Clic.parameter (fun _ str -> Lwt.return (Block_hash.of_b58check str)))
    t

let keep_alive_arg =
  Clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let preload_baker_keys_arg =
  Clic.switch
    ~doc:
      "When at least one baker contract is specified that is using an \
       encrypted consensus key, prompt for password at start-up, rather than \
       when baking a block."
    ~short:'P'
    ~long:"preload-keys"
    ()

let delegate_commands () =
  let open Clic in
  [ command
      ~group
      ~desc:"Forge and inject block using the delegate rights."
      (args8
         max_priority_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         force_switch
         minimal_timestamp_switch
         mempool_arg
         context_path_arg)
      ( prefixes ["bake"; "for"]
      @@ Baker_or_pkh_alias.source_param
           ~name:"baker"
           ~desc:"name of the baker owning the baking right"
      @@ stop )
      (fun ( max_priority,
             minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             force,
             minimal_timestamp,
             mempool,
             context_path )
           baker_contract
           cctxt ->
        baker_of_contract cctxt baker_contract
        >>=? fun baker_hash ->
        bake_block
          cctxt
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ~force
          ?max_priority
          ~minimal_timestamp
          ?mempool
          ?context_path
          ~chain:cctxt#chain
          ~head:cctxt#block
          baker_hash);
    command
      ~group
      ~desc:"Forge and inject a seed-nonce revelation operation."
      no_options
      (prefixes ["reveal"; "nonce"; "for"] @@ seq_of_param block_param)
      (fun () block_hashes cctxt ->
        reveal_block_nonces
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          block_hashes);
    command
      ~group
      ~desc:
        "Forge and inject all the possible seed-nonce revelation operations."
      no_options
      (prefixes ["reveal"; "nonces"] @@ stop)
      (fun () cctxt ->
        reveal_nonces ~chain:cctxt#chain ~block:cctxt#block cctxt ());
    command
      ~group
      ~desc:"Forge and inject an endorsement operation."
      no_options
      ( prefixes ["endorse"; "for"]
      @@ Baker_or_pkh_alias.source_param
           ~name:"baker"
           ~desc:"name of the baker owning the endorsement right"
      @@ stop )
      (fun () baker_contract cctxt ->
        baker_of_contract cctxt baker_contract
        >>=? fun baker_hash ->
        endorse_block cctxt ~chain:cctxt#chain baker_hash);
    command
      ~group
      ~desc:
        "Clear the nonces file by removing the nonces which blocks cannot be \
         found on the chain."
      no_options
      (prefixes ["filter"; "orphan"; "nonces"] @@ stop)
      (fun () (cctxt : #Protocol_client_context.full) ->
        cctxt#with_lock (fun () ->
            let chain = cctxt#chain in
            Client_baking_files.resolve_location cctxt ~chain `Nonce
            >>=? fun nonces_location ->
            let open Client_baking_nonces in
            (* Filtering orphan nonces *)
            load cctxt nonces_location
            >>=? fun nonces ->
            Block_hash.Map.fold
              (fun block nonce acc ->
                acc
                >>= fun acc ->
                Shell_services.Blocks.Header.shell_header
                  cctxt
                  ~chain
                  ~block:(`Hash (block, 0))
                  ()
                >>= function
                | Ok _ ->
                    Lwt.return acc
                | Error _ ->
                    Lwt.return (Block_hash.Map.add block nonce acc))
              nonces
              (Lwt.return empty)
            >>= fun orphans ->
            if Block_hash.Map.cardinal orphans = 0 then
              cctxt#message "No orphan nonces found." >>= fun () -> return_unit
            else
              (* "Backup-ing" orphan nonces *)
              let orphan_nonces_file = "orphan_nonce" in
              cctxt#load orphan_nonces_file ~default:empty encoding
              >>=? fun orphan_nonces ->
              let orphan_nonces = add_all orphan_nonces orphans in
              cctxt#write orphan_nonces_file orphan_nonces encoding
              >>=? fun () ->
              (* Don't forget the 's'. *)
              let orphan_nonces_file = orphan_nonces_file ^ "s" in
              cctxt#message
                "Successfully filtered %d orphan nonces and moved them to \
                 '$TEZOS_CLIENT/%s'."
                (Block_hash.Map.cardinal orphans)
                orphan_nonces_file
              >>= fun () ->
              let filtered_nonces =
                Client_baking_nonces.remove_all nonces orphans
              in
              save cctxt nonces_location filtered_nonces
              >>=? fun () -> return_unit));
    command
      ~group
      ~desc:"List orphan nonces."
      no_options
      (prefixes ["list"; "orphan"; "nonces"] @@ stop)
      (fun () (cctxt : #Protocol_client_context.full) ->
        cctxt#with_lock (fun () ->
            let open Client_baking_nonces in
            let orphan_nonces_file = "orphan_nonce" in
            cctxt#load orphan_nonces_file ~default:empty encoding
            >>=? fun orphan_nonces ->
            let block_hashes =
              List.map fst (Block_hash.Map.bindings orphan_nonces)
            in
            cctxt#message
              "@[<v 2>Found %d orphan nonces associated to the potentially \
               unknown following blocks:@ %a@]"
              (Block_hash.Map.cardinal orphan_nonces)
              (Format.pp_print_list ~pp_sep:Format.pp_print_cut Block_hash.pp)
              block_hashes
            >>= fun () -> return_unit));
    command
      ~group
      ~desc:
        "Find baker with the given hash of consensus key. Note that the \
         consensus key has to be active."
      no_options
      ( prefixes ["find"; "baker"; "with"; "consensus"; "key"]
      @@ Client_keys.Public_key_hash.source_param @@ stop )
      (fun () baker_pkh (cctxt : #Protocol_client_context.full) ->
        baker_of_contract cctxt (Contract.implicit_contract baker_pkh)
        >>=? fun baker_hash ->
        cctxt#message "Found baker: %a" Baker_hash.pp baker_hash
        >>= fun () -> return_unit) ]

let remove_duplicates :
    #Protocol_client_context.full ->
    Contract.t list ->
    Contract.t list tzresult Lwt.t =
 fun cctxt bakers ->
  let module Contract_set = Set.Make (Contract) in
  let unique_bakers = Contract_set.(bakers |> of_list |> elements) in
  ( if List.length bakers <> List.length unique_bakers then
    cctxt#message
      "Warning: the list of bakers contains duplicates, which are ignored"
  else Lwt.return () )
  >>= fun () -> return unique_bakers

let preload_keys :
    #Protocol_client_context.full -> Contract.t list -> unit tzresult Lwt.t =
 fun cctxt contracts ->
  List.fold_left_es
    (fun acc contract ->
      match Contract.is_baker contract with
      | Some baker -> (
          Alpha_services.Baker.consensus_key cctxt (cctxt#chain, `Head 0) baker
          >>=? fun consensus_pk ->
          Client_keys.get_key cctxt (Signature.Public_key.hash consensus_pk)
          >>=? fun (consensus_key_name, _pk, _sk_uri) ->
          Alpha_services.Baker.pending_consensus_key
            cctxt
            (cctxt#chain, `Head 0)
            baker
          >>=? function
          | None ->
              return @@ (consensus_key_name :: acc)
          | Some (pending_pk, _cycle) ->
              Client_keys.get_key cctxt (Signature.Public_key.hash pending_pk)
              >|=? fun (pending_key_name, _pk, _sk_uri) ->
              consensus_key_name :: pending_key_name :: acc )
      | None -> (
        match Contract.is_implicit contract with
        | Some pkh ->
            Client_keys.Public_key_hash.name cctxt pkh
            >|=? fun name -> name :: acc
        | None ->
            return acc ))
    []
    contracts
  >>=? fun key_names ->
  Tezos_signer_backends.Encrypted.decrypt_list cctxt key_names

let baker_commands () =
  let open Clic in
  let group =
    {
      Clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [ command
      ~group
      ~desc:"Launch the baker daemon."
      (args7
         pidfile_arg
         max_priority_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         keep_alive_arg
         preload_baker_keys_arg)
      ( prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"context_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ seq_of_param Baker_or_pkh_alias.source_param )
      (fun ( pidfile,
             max_priority,
             minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             keep_alive,
             preload_baker_keys )
           node_path
           bakers
           cctxt ->
        may_lock_pidfile pidfile
        >>=? fun () ->
        remove_duplicates cctxt bakers
        >>=? fun bakers ->
        (if preload_baker_keys then preload_keys cctxt bakers else return_unit)
        >>=? fun () ->
        Client_daemon.Baker.run
          cctxt
          ~chain:cctxt#chain
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ?max_priority
          ~context_path:(Filename.concat node_path "context")
          ~keep_alive
          bakers) ]

let endorser_commands () =
  let open Clic in
  let group =
    {
      Clic.name = "delegate.endorser";
      title = "Commands related to endorser daemon.";
    }
  in
  [ command
      ~group
      ~desc:"Launch the endorser daemon"
      (args4
         pidfile_arg
         endorsement_delay_arg
         keep_alive_arg
         preload_baker_keys_arg)
      (prefixes ["run"] @@ seq_of_param Baker_or_pkh_alias.source_param)
      (fun (pidfile, endorsement_delay, keep_alive, preload_baker_keys)
           bakers
           cctxt ->
        may_lock_pidfile pidfile
        >>=? fun () ->
        remove_duplicates cctxt bakers
        >>=? fun bakers ->
        (if preload_baker_keys then preload_keys cctxt bakers else return_unit)
        >>=? fun () ->
        Client_daemon.Endorser.run
          cctxt
          ~chain:cctxt#chain
          ~delay:endorsement_delay
          ~keep_alive
          bakers) ]

let accuser_commands () =
  let open Clic in
  let group =
    {
      Clic.name = "delegate.accuser";
      title = "Commands related to the accuser daemon.";
    }
  in
  [ command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
      (prefixes ["run"] @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        may_lock_pidfile pidfile
        >>=? fun () ->
        Client_daemon.Accuser.run
          cctxt
          ~chain:cctxt#chain
          ~preserved_levels
          ~keep_alive) ]
