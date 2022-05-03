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

let rollup_id_arg =
  Clic.arg
    ~long:"rollup-id"
    ~placeholder:"rollup-id"
    ~doc:"The rollup id of the rollup to target"
    (Clic.parameter (fun _ s ->
         match Protocol.Alpha_context.Tx_rollup.of_b58check s with
         | Ok x -> return x
         | Error _ -> failwith "Invalid Rollup Id"))

let rollup_genesis_arg =
  Clic.arg
    ~long:"rollup-genesis"
    ~placeholder:"rollup_genesis"
    ~doc:"The hash of the block where the rollup was created"
    (Clic.parameter (fun _ str ->
         Option.fold_f
           ~none:(fun () -> failwith "Invalid Block Hash")
           ~some:return
         @@ Block_hash.of_b58check_opt str))

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

let group =
  Clic.
    {
      name = "tx_rollup.node";
      title = "Commands related to the transaction rollup node";
    }

let to_tzresult msg = function Some x -> return x | None -> failwith msg

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
       rollup_id_arg
       rollup_genesis_arg
       rpc_addr_arg
       reconnection_delay_arg)
    (prefixes ["config"; "init"; "on"] @@ stop)
    (fun ( data_dir,
           operator,
           batch_signer,
           finalize_commitment_signer,
           remove_commitment_signer,
           rejection_signer,
           dispatch_withdrawals_signer,
           rollup_id,
           rollup_genesis,
           rpc_addr,
           reconnection_delay )
         cctxt ->
      let open Lwt_result_syntax in
      let*! () = Event.(emit preamble_warning) () in
      let* rollup_id = to_tzresult "Missing arg --rollup-id" rollup_id in
      let data_dir =
        match data_dir with
        | Some d -> d
        | None -> Node_config.default_data_dir rollup_id
      in
      let config =
        Node_config.
          {
            data_dir;
            operator;
            signers =
              {
                submit_batch = batch_signer;
                finalize_commitment = finalize_commitment_signer;
                remove_commitment = remove_commitment_signer;
                rejection = rejection_signer;
                dispatch_withdrawals = dispatch_withdrawals_signer;
              };
            rollup_id;
            rollup_genesis;
            rpc_addr;
            reconnection_delay;
            l2_blocks_cache_size = default_l2_blocks_cache_size;
          }
      in
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
    (args2 data_dir_arg rollup_id_arg)
    (prefixes ["run"] @@ stop)
    (fun (data_dir, rollup_id) cctxt ->
      let*! () = Event.(emit preamble_warning) () in
      let* data_dir =
        match data_dir with
        | Some d -> return d
        | None ->
            let* rollup_id =
              to_tzresult "Provide at least --rollup-id or --data-dir" rollup_id
            in
            return (Node_config.default_data_dir rollup_id)
      in
      let* config = Node_config.load ~data_dir in
      Daemon.run config cctxt)

let tx_rollup_commands () =
  List.map
    (Clic.map_command (new Protocol_client_context.wrap_full))
    [configuration_init_command; run_command]

let select_commands _ _ = return (tx_rollup_commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
