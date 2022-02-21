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
  Format.sprintf
    "The directory path to the transaction rollup node data. The default path \
     is %s"

let rpc_addr_doc =
  Format.asprintf
    "The address where the node listens. The default address is %s"

let rpc_port_doc =
  Format.asprintf "The port where the node listens. The default port is %d"

let reconnection_delay_doc =
  Format.asprintf
    "The reconnection delay when the connection is lost. The default delay is \
     %f"

let data_dir_arg =
  let default = Configuration.default_data_dir in
  let doc = data_dir_doc default in
  Clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data_dir"
    ~doc
    ~default
    Client_proto_args.string_parameter

let operator_arg =
  let doc = "The public key of the node operator" in
  Clic.arg
    ~long:"operator"
    ~placeholder:"public_key"
    ~doc
    (Clic.parameter (fun _ -> Client_keys.Public_key_hash.of_source))

let rollup_id_arg =
  Clic.arg
    ~long:"rollup-id"
    ~placeholder:"rollup_id"
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
  let default = Configuration.default_rpc_addr in
  let doc = rpc_addr_doc default in
  Clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"address|ip"
    ~doc
    ~default
    Client_proto_args.string_parameter

let rpc_port_arg =
  let default = Configuration.default_rpc_port in
  let doc = rpc_port_doc default in
  Clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"port"
    ~doc
    ~default:(string_of_int default)
    Client_proto_args.int_parameter

let reconnection_delay_arg =
  let default = Configuration.default_reconnection_delay in
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

let to_tzresult msg = function
  | Some x -> Error_monad.return x
  | None -> Error_monad.failwith msg

let configuration_init_command =
  let open Clic in
  command
    ~group
    ~desc:"Configure the transaction rollup daemon."
    (args7
       data_dir_arg
       operator_arg
       rollup_id_arg
       rollup_genesis_arg
       rpc_addr_arg
       rpc_port_arg
       reconnection_delay_arg)
    (prefixes ["config"; "init"; "on"] @@ stop)
    (fun ( data_dir,
           client_keys,
           rollup_id,
           block_hash,
           rpc_addr,
           rpc_port,
           reconnection_delay )
         cctxt ->
      let open Lwt_result_syntax in
      let*! () = Event.(emit preamble_warning) () in
      let* client_keys = to_tzresult "Missing arg --operator" client_keys in
      let* rollup_id = to_tzresult "Missing arg --rollup_id" rollup_id in
      let* rollup_genesis =
        to_tzresult "Missing arg --rollup_genesis" block_hash
      in
      let config =
        Configuration.
          {
            data_dir;
            client_keys;
            rollup_id;
            rollup_genesis;
            rpc_addr;
            rpc_port;
            reconnection_delay;
          }
      in
      let file = Configuration.get_configuration_filename data_dir in
      let* () = Configuration.save config in
      (* This is necessary because the node has not yet been launched, so event
         listening can't be used. *)
      cctxt#message "Configuration written in %s" file >>= fun _ ->
      let*! () = Event.(emit configuration_was_written) (file, config) in
      return_unit)

let run_command =
  let open Lwt_syntax in
  let open Clic in
  command
    ~group
    ~desc:"Run the transaction rollup daemon."
    (args1 data_dir_arg)
    (prefixes ["run"] @@ stop)
    (fun data_dir cctxt ->
      let* () = Event.(emit preamble_warning) () in
      Daemon.run ~data_dir cctxt)

let tx_rollup_commands () =
  List.map
    (Clic.map_command (new Protocol_client_context.wrap_full))
    [configuration_init_command; run_command]

let select_commands _ _ =
  return (tx_rollup_commands () @ Client_helpers_commands.commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
