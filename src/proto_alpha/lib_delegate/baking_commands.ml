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
  Tezos_clic.arg
    ~doc:"write process id in file"
    ~short:'P'
    ~long:"pidfile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun _ s -> return s))

let may_lock_pidfile pidfile_opt f =
  match pidfile_opt with
  | None -> f ()
  | Some pidfile ->
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () ->
          failwith "Failed to create the pidfile: %s" pidfile)
        ~filename:pidfile
        f

let http_headers_env_variable =
  "TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS"

let http_headers =
  match Sys.getenv_opt http_headers_env_variable with
  | None -> None
  | Some contents ->
      let lines = String.split_on_char '\n' contents in
      Some
        (List.fold_left
           (fun acc line ->
             match String.index_opt line ':' with
             | None ->
                 invalid_arg
                   (Printf.sprintf
                      "Http headers: invalid %s environment variable, missing \
                       colon"
                      http_headers_env_variable)
             | Some pos ->
                 let header = String.trim (String.sub line 0 pos) in
                 let header = String.lowercase_ascii header in
                 if header <> "host" then
                   invalid_arg
                     (Printf.sprintf
                        "Http headers: invalid %s environment variable, only \
                         'host' headers are supported"
                        http_headers_env_variable) ;
                 let value =
                   String.trim
                     (String.sub line (pos + 1) (String.length line - pos - 1))
                 in
                 (header, value) :: acc)
           []
           lines)

let operations_arg =
  Tezos_clic.arg
    ~long:"operations-pool"
    ~placeholder:"file|uri"
    ~doc:
      (Printf.sprintf
         "When specified, the baker will try to fetch operations from this \
          file (or uri) and to include retrieved operations in the block. The \
          expected format of the contents is a list of operations [ \
          alpha.operation ].  Environment variable '%s' may also be specified \
          to add headers to the requests (only 'host' headers are supported). \
          If the resource cannot be retrieved, e.g., if the file is absent, \
          unreadable, or the web service returns a 404 error, the resource is \
          simply ignored."
         http_headers_env_variable)
    (Tezos_clic.map_parameter
       ~f:(fun uri ->
         let open Baking_configuration in
         match Uri.scheme uri with
         | Some "http" | Some "https" ->
             Operations_source.(Remote {uri; http_headers})
         | None | Some _ ->
             (* acts as if it were file even though it might no be *)
             Operations_source.(Local {filename = Uri.to_string uri}))
       uri_parameter)

let context_path_arg =
  Tezos_clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:
      "When specified, the client will read in the local context at the \
       provided path in order to build the block, instead of relying on the \
       'preapply' RPC."
    string_parameter

let force_apply_switch_arg =
  Tezos_clic.switch
    ~long:"force-apply"
    ~doc:"Force the baker to not only validate but also apply operations."
    ()

let endorsement_force_switch_arg =
  Tezos_clic.switch
    ~long:"force"
    ~short:'f'
    ~doc:
      "Disable consistency, injection and double signature checks for \
       (pre)endorsements."
    ()

let do_not_monitor_node_mempool_arg =
  Tezos_clic.switch
    ~long:"ignore-node-mempool"
    ~doc:
      "Ignore mempool operations from the node and do not subsequently monitor \
       them. Use in conjunction with --operations option to restrict the \
       observed operations to those of the mempool file."
    ()

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the daemon process alive: when the connection with the node is \
       lost, the daemon periodically tries to reach it."
    ~short:'K'
    ~long:"keep-alive"
    ()

let liquidity_baking_toggle_vote_parameter =
  Tezos_clic.parameter
    ~autocomplete:(fun _ctxt -> return ["on"; "off"; "pass"])
    (let open Protocol.Alpha_context.Liquidity_baking in
    fun _ctxt -> function
      | "on" -> return LB_on
      | "off" -> return LB_off
      | "pass" -> return LB_pass
      | s ->
          failwith
            "unexpected vote: %s, expected either \"on\", \"off\", or \"pass\"."
            s)

let liquidity_baking_toggle_vote_arg =
  Tezos_clic.arg
    ~doc:
      "Vote to continue or end the liquidity baking subsidy. The possible \
       values for this option are: \"off\" to request ending the subsidy, \
       \"on\" to request continuing or restarting the subsidy, and \"pass\" to \
       abstain. Note that this \"option\" is mandatory!"
    ~long:"liquidity-baking-toggle-vote"
    ~placeholder:"vote"
    liquidity_baking_toggle_vote_parameter

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
        | alias, pk, sk_uri -> return (proj_delegate (alias, pkh, pk, sk_uri)))
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
  (if List.compare_lengths delegates delegates_no_duplicates <> 0 then
   cctxt#warning
     "Warning: the list of public key hash aliases contains duplicate hashes, \
      which are ignored"
  else Lwt.return ())
  >>= fun () -> return delegates_no_duplicates

let sources_param =
  Tezos_clic.seq_of_param
    (Client_keys.Public_key_hash.source_param
       ~name:"baker"
       ~desc:
         "name of the delegate owning the endorsement/baking right or name of \
          the consensus key signing on the delegate's behalf")

let endpoint_arg =
  Tezos_clic.arg
    ~long:"dal-node"
    ~placeholder:"uri"
    ~doc:"endpoint of the DAL node, e.g. 'http://localhost:8933'"
    (Tezos_clic.parameter (fun _ s -> return @@ Uri.of_string s))

let delegate_commands () : Protocol_client_context.full Tezos_clic.command list
    =
  let open Tezos_clic in
  let group =
    {name = "delegate.client"; title = "Tenderbake client commands"}
  in
  [
    command
      ~group
      ~desc:"Benchmark the proof of work challenge resolution"
      (args2
         (default_arg
            ~doc:"Proof of work threshold"
            ~long:"threshold"
            ~placeholder:"int"
            ~default:
              (Int64.to_string
                 Default_parameters.constants_mainnet.proof_of_work_threshold)
            (parameter (fun (cctxt : Protocol_client_context.full) x ->
                 try return (Int64.of_string x)
                 with _ -> cctxt#error "Expect an integer")))
         (arg
            ~doc:"Random seed"
            ~long:"seed"
            ~placeholder:"int"
            (parameter (fun (cctxt : Protocol_client_context.full) x ->
                 try return (int_of_string x)
                 with _ -> cctxt#error "Expect an integer"))))
      (prefix "bench"
      @@ param
           ~name:"nb_draw"
           ~desc:"number of draws"
           (parameter (fun (cctxt : Protocol_client_context.full) x ->
                match int_of_string x with
                | x when x >= 1 -> return x
                | _ | (exception _) ->
                    cctxt#error "Expect a strictly positive integer"))
      @@ fixed ["baking"; "PoW"; "challenges"])
      (fun (proof_of_work_threshold, seed) nb_draw cctxt ->
        let open Lwt_result_syntax in
        let*! () =
          cctxt#message
            "Running %d iterations of proof-of-work challenge..."
            nb_draw
        in
        let rstate =
          match seed with
          | None -> Random.State.make_self_init ()
          | Some s -> Random.State.make [|s|]
        in
        let* all =
          List.map_es
            (fun i ->
              let level = Int32.of_int (Random.State.int rstate (1 lsl 29)) in
              let shell_header =
                Tezos_base.Block_header.
                  {
                    level;
                    proto_level = 1;
                    (* uint8 *)
                    predecessor = Tezos_crypto.Hashed.Block_hash.zero;
                    timestamp = Time.Protocol.epoch;
                    validation_passes = 3;
                    (* uint8 *)
                    operations_hash =
                      Tezos_crypto.Hashed.Operation_list_list_hash.zero;
                    fitness = [];
                    context = Tezos_crypto.Hashed.Context_hash.zero;
                  }
              in
              let now = Time.System.now () in
              let* _ =
                Baking_pow.mine
                  ~proof_of_work_threshold
                  shell_header
                  (fun proof_of_work_nonce ->
                    Protocol.Alpha_context.
                      {
                        Block_header.payload_hash =
                          Protocol.Block_payload_hash.zero;
                        payload_round = Round.zero;
                        seed_nonce_hash = None;
                        proof_of_work_nonce;
                        liquidity_baking_toggle_vote = LB_pass;
                      })
              in
              let _then = Time.System.now () in
              let x = Ptime.diff _then now in
              let*! () = cctxt#message "%d/%d: %a" i nb_draw Ptime.Span.pp x in
              return x)
            (1 -- nb_draw)
        in
        let sum = List.fold_left Ptime.Span.add Ptime.Span.zero all in
        let base, tail = Stdlib.List.(hd all, tl all) in
        let max =
          List.fold_left
            (fun x y -> if Ptime.Span.compare x y > 0 then x else y)
            base
            tail
        in
        let min =
          List.fold_left
            (fun x y -> if Ptime.Span.compare x y <= 0 then x else y)
            base
            tail
        in
        let div = Ptime.Span.to_float_s sum /. float (List.length all) in
        let*! () =
          cctxt#message
            "%d runs: min: %a, max: %a, average: %a"
            nb_draw
            Ptime.Span.pp
            min
            Ptime.Span.pp
            max
            (Format.pp_print_option Ptime.Span.pp)
            (Ptime.Span.of_float_s div)
        in
        return_unit);
    command
      ~group
      ~desc:"Forge and inject block using the delegates' rights."
      (args10
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_apply_switch_arg
         force_switch
         operations_arg
         context_path_arg
         do_not_monitor_node_mempool_arg
         endpoint_arg)
      (prefixes ["bake"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force_apply,
             force,
             extra_operations,
             context_path,
             do_not_monitor_node_mempool,
             dal_node_endpoint )
           pkhs
           cctxt ->
        get_delegates cctxt pkhs >>=? fun delegates ->
        Baking_lib.bake
          cctxt
          ~minimal_nanotez_per_gas_unit
          ~minimal_timestamp
          ~minimal_nanotez_per_byte
          ~minimal_fees
          ~force_apply
          ~force
          ~monitor_node_mempool:(not do_not_monitor_node_mempool)
          ?extra_operations
          ?context_path
          ?dal_node_endpoint
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
      (args8
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         minimal_timestamp_switch
         force_apply_switch_arg
         force_switch
         operations_arg
         context_path_arg)
      (prefixes ["propose"; "for"] @@ sources_param)
      (fun ( minimal_fees,
             minimal_nanotez_per_gas_unit,
             minimal_nanotez_per_byte,
             minimal_timestamp,
             force_apply,
             force,
             extra_operations,
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
          ~force_apply
          ~force
          ?extra_operations
          ?context_path
          delegates);
  ]

let directory_parameter =
  Tezos_clic.parameter (fun _ p ->
      Lwt_utils_unix.dir_exists p >>= fun exists ->
      if not exists then failwith "Directory doesn't exist: '%s'" p
      else return p)

let per_block_vote_file_arg =
  Tezos_clic.arg
    ~doc:"read per block votes as json file"
    ~short:'V'
    ~long:"votefile"
    ~placeholder:"filename"
    (Tezos_clic.parameter (fun (_cctxt : Protocol_client_context.full) file ->
         let open Lwt_result_syntax in
         let* file_exists =
           protect
             ~on_error:(fun _ ->
               tzfail (Liquidity_baking_vote.Block_vote_file_not_found file))
             (fun () ->
               let*! b = Lwt_unix.file_exists file in
               return b)
         in
         if file_exists then return file
         else tzfail (Liquidity_baking_vote.Block_vote_file_not_found file)))

let lookup_default_vote_file_path (cctxt : Protocol_client_context.full) =
  let open Lwt_syntax in
  let default_filename = Liquidity_baking_vote.default_vote_json_filename in
  let file_exists path =
    Lwt.catch (fun () -> Lwt_unix.file_exists path) (fun _ -> return_false)
  in
  let when_s pred x g =
    let* b = pred x in
    if b then return_some x else g ()
  in
  (* Check in current working directory *)
  when_s file_exists default_filename @@ fun () ->
  (* Check in the baker directory *)
  let base_dir_file = Filename.Infix.(cctxt#get_base_dir // default_filename) in
  when_s file_exists base_dir_file @@ fun () -> return_none

type baking_mode = Local of {local_data_dir_path : string} | Remote

let baker_args =
  Tezos_clic.args10
    pidfile_arg
    minimal_fees_arg
    minimal_nanotez_per_gas_unit_arg
    minimal_nanotez_per_byte_arg
    force_apply_switch_arg
    keep_alive_arg
    liquidity_baking_toggle_vote_arg
    per_block_vote_file_arg
    operations_arg
    endpoint_arg

let run_baker
    ( pidfile,
      minimal_fees,
      minimal_nanotez_per_gas_unit,
      minimal_nanotez_per_byte,
      force_apply,
      keep_alive,
      liquidity_baking_toggle_vote,
      per_block_vote_file,
      extra_operations,
      dal_node_endpoint ) baking_mode sources cctxt =
  may_lock_pidfile pidfile @@ fun () ->
  (if per_block_vote_file = None then
   (* If the liquidity baking file was not explicitly given, we
      look into default locations. *)
   lookup_default_vote_file_path cctxt
  else Lwt.return per_block_vote_file)
  >>= fun per_block_vote_file ->
  (* We don't let the user run the baker without providing some
     option (CLI, file path, or file in default location) for
     the toggle vote. *)
  Liquidity_baking_vote.load_liquidity_baking_config
    ~per_block_vote_file_arg:per_block_vote_file
    ~toggle_vote_arg:liquidity_baking_toggle_vote
  >>=? fun liquidity_baking ->
  get_delegates cctxt sources >>=? fun delegates ->
  let context_path =
    match baking_mode with
    | Local {local_data_dir_path} ->
        Some Filename.Infix.(local_data_dir_path // "context")
    | Remote -> None
  in
  Client_daemon.Baker.run
    cctxt
    ~minimal_fees
    ~minimal_nanotez_per_gas_unit
    ~minimal_nanotez_per_byte
    ~liquidity_baking
    ?extra_operations
    ?dal_node_endpoint
    ~force_apply
    ~chain:cctxt#chain
    ?context_path
    ~keep_alive
    delegates

let baker_commands () : Protocol_client_context.full Tezos_clic.command list =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      baker_args
      (prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"node_data_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ sources_param)
      (fun args local_data_dir_path sources cctxt ->
        let baking_mode = Local {local_data_dir_path} in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let baking_mode = Remote in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (* no_options *)
      (args1 keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun keep_alive cctxt ->
        Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive);
  ]

let accuser_commands () =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.accuser";
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
        may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.Accuser.run
          cctxt
          ~chain:cctxt#chain
          ~preserved_levels
          ~keep_alive);
  ]
