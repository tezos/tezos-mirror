(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

let data_dir_doc =
  Format.sprintf "The directory path to the transaction rollup node data."

let rpc_addr_doc =
  Format.asprintf
    "The address and port where the node listens to RPCs. The default is %s"

let reconnection_delay_doc =
  Format.asprintf
    "The reconnection delay when the connection is lost. The default delay is \
     %f"

let data_dir_arg =
  let doc = data_dir_doc in
  Clic.arg
    ~long:"data-dir"
    ~placeholder:"data_dir"
    ~doc
    Client_proto_args.string_parameter

let operator_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"operator"
    ~placeholder:"operator"
    ~doc:"The operator of the rollup"
    ()

let batch_signer_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"batch-signer"
    ~placeholder:"batch-signer"
    ~doc:"The signer for submission of batches"
    ()

let finalize_commitment_signer_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"finalize-commitment-signer"
    ~placeholder:"finalize-commitment-signer"
    ~doc:"The signer for finalization of commitments"
    ()

let remove_commitment_signer_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"remove-commitment-signer"
    ~placeholder:"remove-commitment-signer"
    ~doc:"The signer for removals of commitments"
    ()

let rejection_signer_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"rejection-signer"
    ~placeholder:"rejection-signer"
    ~doc:"The signer for rejections"
    ()

let dispatch_withdrawals_signer_arg =
  Client_keys.Public_key_hash.source_arg
    ~long:"dispatch-withdrawals-signer"
    ~placeholder:"dispatch-withdrawals-signer"
    ~doc:"The signer for dispatch withdrawals"
    ()

let rollup_id_param =
  let open Client_proto_rollups in
  Clic.parameter ~autocomplete:TxRollupAlias.autocomplete (fun cctxt s ->
      let open Lwt_result_syntax in
      let from_alias s = TxRollupAlias.find cctxt s in
      let from_key s =
        match Protocol.Alpha_context.Tx_rollup.of_b58check s with
        | Ok x -> return x
        | Error _ ->
            failwith "Cannot parse %s as a transaction rollup address" s
      in
      Client_aliases.parse_alternatives
        [("alias", from_alias); ("key", from_key)]
        s)

let rollup_id_arg =
  Clic.arg
    ~long:"rollup-id"
    ~placeholder:"rollup-id"
    ~doc:"The rollup id of the rollup to target"
    rollup_id_param

let origination_level_arg =
  Clic.arg
    ~long:"origination-level"
    ~placeholder:"origination_level"
    ~doc:"The level of the block where the rollup was originated"
    (Clic.parameter (fun _ str ->
         match Int32.of_string_opt str with
         | None -> failwith "Invalid origination level"
         | Some l -> return l))

let rpc_addr_arg =
  let default = P2p_point.Id.to_string Node_config.default_rpc_addr in
  let doc = rpc_addr_doc default in
  Clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"address:port"
    ~doc
    ~default
    (Clic.parameter (fun _ s ->
         P2p_point.Id.of_string s
         |> Result.map_error (fun e -> [Exn (Failure e)])
         |> Lwt.return))

let rpc_addr_opt_arg =
  let default = P2p_point.Id.to_string Node_config.default_rpc_addr in
  let doc = rpc_addr_doc default in
  Clic.arg
    ~long:"rpc-addr"
    ~placeholder:"address:port"
    ~doc
    (Clic.parameter (fun _ s ->
         P2p_point.Id.of_string s
         |> Result.map_error (fun e -> [Exn (Failure e)])
         |> Lwt.return))

let reconnection_delay_arg =
  let default = Node_config.default_reconnection_delay in
  let doc = reconnection_delay_doc default in
  Clic.default_arg
    ~long:"reconnection-delay"
    ~placeholder:"delay"
    ~doc
    ~default:(string_of_float default)
    (Clic.parameter (fun _ p ->
         try return (float_of_string p) with _ -> failwith "Cannot read float"))

let reconnection_delay_opt_arg =
  let default = Node_config.default_reconnection_delay in
  let doc = reconnection_delay_doc default in
  Clic.arg
    ~long:"reconnection-delay"
    ~placeholder:"delay"
    ~doc
    (Clic.parameter (fun _ p ->
         try return (float_of_string p) with _ -> failwith "Cannot read float"))

let possible_modes = List.map Node_config.string_of_mode Node_config.modes

let mode_parameter =
  Clic.parameter
    ~autocomplete:(fun _ -> return possible_modes)
    (fun _ m -> Lwt.return (Node_config.mode_of_string m))

let mode_param =
  Clic.param
    ~name:"mode"
    ~desc:
      (Printf.sprintf
         "The mode for the rollup node (%s)"
         (String.concat ", " possible_modes))
    mode_parameter

let allow_deposit_arg =
  Clic.switch
    ~doc:"Allow the operator to make a first deposit for commitments"
    ~long:"allow-deposit"
    ()

let group =
  Clic.
    {
      name = "tx_rollup.node";
      title = "Commands related to the transaction rollup node";
    }

let to_tzresult msg = function Some x -> return x | None -> failwith msg

let config_from_args data_dir rollup_id mode operator batch_signer
    finalize_commitment_signer remove_commitment_signer rejection_signer
    dispatch_withdrawals_signer origination_level rpc_addr allow_deposit
    reconnection_delay =
  let data_dir =
    match data_dir with
    | Some d -> d
    | None -> Node_config.default_data_dir rollup_id
  in
  Node_config.
    {
      data_dir;
      mode;
      signers =
        {
          operator;
          submit_batch = batch_signer;
          finalize_commitment = finalize_commitment_signer;
          remove_commitment = remove_commitment_signer;
          rejection = rejection_signer;
          dispatch_withdrawals = dispatch_withdrawals_signer;
        };
      rollup_id;
      origination_level;
      rpc_addr;
      reconnection_delay;
      allow_deposit;
      l2_blocks_cache_size = default_l2_blocks_cache_size;
      caps = default_caps;
      batch_burn_limit = None;
    }

let patch_config_from_args config rollup_id mode operator batch_signer
    finalize_commitment_signer remove_commitment_signer rejection_signer
    dispatch_withdrawals_signer origination_level rpc_addr allow_deposit
    reconnection_delay =
  if
    Protocol.Alpha_context.Tx_rollup.(rollup_id <> config.Node_config.rollup_id)
  then
    error_with
      "Rollup node is configured for rollup %a but asked to run for rollup %a"
      Protocol.Alpha_context.Tx_rollup.pp
      config.Node_config.rollup_id
      Protocol.Alpha_context.Tx_rollup.pp
      rollup_id
  else
    let operator = Option.either operator config.signers.operator in
    let submit_batch = Option.either batch_signer config.signers.submit_batch in
    let finalize_commitment =
      Option.either
        finalize_commitment_signer
        config.signers.finalize_commitment
    in
    let remove_commitment =
      Option.either remove_commitment_signer config.signers.remove_commitment
    in
    let dispatch_withdrawals =
      Option.either
        dispatch_withdrawals_signer
        config.signers.dispatch_withdrawals
    in
    let rejection = Option.either rejection_signer config.signers.rejection in
    let signers =
      {
        Node_config.operator;
        submit_batch;
        finalize_commitment;
        remove_commitment;
        dispatch_withdrawals;
        rejection;
      }
    in
    let origination_level =
      Option.either origination_level config.origination_level
    in
    let rpc_addr = Option.value rpc_addr ~default:config.rpc_addr in
    let reconnection_delay =
      Option.value reconnection_delay ~default:config.reconnection_delay
    in
    let allow_deposit = allow_deposit || config.allow_deposit in
    let config =
      {
        config with
        mode;
        signers;
        origination_level;
        rpc_addr;
        reconnection_delay;
        allow_deposit;
      }
    in
    ok config

let configuration_init_command =
  let open Clic in
  command
    ~group
    ~desc:"Configure the transaction rollup daemon."
    (args11
       data_dir_arg
       operator_arg
       batch_signer_arg
       finalize_commitment_signer_arg
       remove_commitment_signer_arg
       rejection_signer_arg
       dispatch_withdrawals_signer_arg
       origination_level_arg
       rpc_addr_arg
       allow_deposit_arg
       reconnection_delay_arg)
    (prefix "init" @@ mode_param
    @@ prefixes ["config"; "for"]
    @@ Clic.param
         ~name:"rollup-id"
         ~desc:"address of the rollup"
         rollup_id_param
    @@ stop)
    (fun ( data_dir,
           operator,
           batch_signer,
           finalize_commitment_signer,
           remove_commitment_signer,
           rejection_signer,
           dispatch_withdrawals_signer,
           origination_level,
           rpc_addr,
           allow_deposit,
           reconnection_delay )
         mode
         rollup_id
         cctxt ->
      let open Lwt_result_syntax in
      let*! () = Event.(emit preamble_warning) () in
      let config =
        config_from_args
          data_dir
          rollup_id
          mode
          operator
          batch_signer
          finalize_commitment_signer
          remove_commitment_signer
          rejection_signer
          dispatch_withdrawals_signer
          origination_level
          rpc_addr
          allow_deposit
          reconnection_delay
      in
      let*? config = Node_config.check_mode config in
      let* file = Node_config.save config in
      (* This is necessary because the node has not yet been launched, so event
         listening can't be used. *)
      let*! () = cctxt#message "Configuration written in %s" file in
      let*! () = Event.(emit configuration_was_written) (file, config) in
      return_unit)

let run_command =
  let open Lwt_result_syntax in
  let open Clic in
  command
    ~group
    ~desc:"Run the transaction rollup daemon."
    (args11
       data_dir_arg
       operator_arg
       batch_signer_arg
       finalize_commitment_signer_arg
       remove_commitment_signer_arg
       rejection_signer_arg
       dispatch_withdrawals_signer_arg
       origination_level_arg
       rpc_addr_opt_arg
       allow_deposit_arg
       reconnection_delay_opt_arg)
    (prefix "run" @@ mode_param @@ prefix "for"
    @@ Clic.param
         ~name:"rollup-id"
         ~desc:"address of the rollup"
         rollup_id_param
    @@ stop)
    (fun ( data_dir,
           operator,
           batch_signer,
           finalize_commitment_signer,
           remove_commitment_signer,
           rejection_signer,
           dispatch_withdrawals_signer,
           origination_level,
           rpc_addr,
           allow_deposit,
           reconnection_delay )
         mode
         rollup_id
         cctxt ->
      let*! () = Event.(emit preamble_warning) () in
      let* config =
        match data_dir with
        | None ->
            let rpc_addr =
              Option.value rpc_addr ~default:Node_config.default_rpc_addr
            in
            let reconnection_delay =
              Option.value
                reconnection_delay
                ~default:Node_config.default_reconnection_delay
            in
            return
              (config_from_args
                 data_dir
                 rollup_id
                 mode
                 operator
                 batch_signer
                 finalize_commitment_signer
                 remove_commitment_signer
                 rejection_signer
                 dispatch_withdrawals_signer
                 origination_level
                 rpc_addr
                 allow_deposit
                 reconnection_delay)
        | Some data_dir ->
            let* config = Node_config.load ~data_dir in
            let*? config =
              patch_config_from_args
                config
                rollup_id
                mode
                operator
                batch_signer
                finalize_commitment_signer
                remove_commitment_signer
                rejection_signer
                dispatch_withdrawals_signer
                origination_level
                rpc_addr
                allow_deposit
                reconnection_delay
            in
            return config
      in
      let*? config = Node_config.check_mode config in
      Daemon.run config cctxt)

let tx_rollup_commands () =
  List.map
    (Clic.map_command (new Protocol_client_context.wrap_full))
    [configuration_init_command; run_command]

let select_commands _ _ = return (tx_rollup_commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
