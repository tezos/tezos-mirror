(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

let sc_rollup_address_param =
  Clic.param
    ~name:"sc-rollup-address"
    ~desc:"The smart-contract rollup address"
    (Clic.parameter (fun _ s ->
         match Protocol.Alpha_context.Sc_rollup.Address.of_b58check_opt s with
         | None -> failwith "Invalid smart-contract rollup address"
         | Some addr -> return addr))

let sc_rollup_node_operator_param =
  let open Lwt_result_syntax in
  Clic.param
    ~name:"operator"
    ~desc:
      (Printf.sprintf
         "Public key hash, or alias, of a smart-contract rollup node operator. \
          An operator can be specialized to a particular purpose by prefixing \
          its key or alias by said purpose, e.g. publish:alias_of_my_operator. \
          The possible purposes are: %s."
         (String.concat ", "
         @@ Configuration.(List.map string_of_purpose purposes)))
  @@ Clic.parameter
  @@ fun cctxt s ->
  let parse_pkh s =
    let from_alias s = Client_keys.Public_key_hash.find cctxt s in
    let from_key s =
      match Signature.Public_key_hash.of_b58check_opt s with
      | None ->
          failwith "Could not read public key hash for rollup node operator"
      | Some pkh -> return pkh
    in
    Client_aliases.parse_alternatives
      [("alias", from_alias); ("key", from_key)]
      s
  in
  match String.split ~limit:1 ':' s with
  | [_] ->
      let+ pkh = parse_pkh s in
      `Default pkh
  | [purpose; operator_s] -> (
      match Configuration.purpose_of_string purpose with
      | Some purpose ->
          let+ pkh = parse_pkh operator_s in
          `Purpose (purpose, pkh)
      | None ->
          let+ pkh = parse_pkh s in
          `Default pkh)
  | _ ->
      (* cannot happen due to String.split's implementation. *)
      assert false

let possible_modes = List.map Configuration.string_of_mode Configuration.modes

let mode_parameter =
  Clic.parameter
    ~autocomplete:(fun _ -> return possible_modes)
    (fun _ m -> Lwt.return (Configuration.mode_of_string m))

let mode_param =
  Clic.param
    ~name:"mode"
    ~desc:
      (Format.asprintf
         "@[<v 2>The mode for the rollup node (%s)@,%a@]"
         (String.concat ", " possible_modes)
         (Format.pp_print_list (fun fmt mode ->
              Format.fprintf
                fmt
                "- %s: %s"
                (Configuration.string_of_mode mode)
                (Configuration.description_of_mode mode)))
         Configuration.modes)
    mode_parameter

let rpc_addr_arg =
  let default = Configuration.default_rpc_addr in
  Clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"rpc-address|ip"
    ~doc:
      (Format.sprintf
         "The address the smart-contract rollup node listens to. Default value \
          is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let dal_node_addr_arg =
  let default = Configuration.default_dal_node_addr in
  Clic.default_arg
    ~long:"dal-node-addr"
    ~placeholder:"dal-node-address|ip"
    ~doc:
      (Format.sprintf
         "The address of the dal node from which the smart-contract rollup \
          node downloads slots. Default value is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"rpc-port"
    ~doc:
      (Format.sprintf
         "The port the smart-contract rollup node listens to. Default value is \
          %s"
         default)
    ~default
    Client_proto_args.int_parameter

let dal_node_port_arg =
  let default = Configuration.default_dal_node_port |> string_of_int in
  Clic.default_arg
    ~long:"dal-node-port"
    ~placeholder:"dal-node-port"
    ~doc:
      (Format.sprintf
         "The port of the dal node from which the smart-contract rollup node \
          downloads slots from. Default value is %s"
         default)
    ~default
    Client_proto_args.int_parameter

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the smart-contract rollup node data directory. Default \
          value is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let loser_mode =
  Clic.default_arg
    ~long:"loser-mode"
    ~placeholder:"mode"
    ~default:""
    ~doc:"Set the rollup node failure points (for test only!)."
    (Clic.parameter (fun _ s ->
         match Loser_mode.make s with
         | Some t -> return t
         | None -> failwith "Invalid syntax for failure points"))

let reconnection_delay_arg =
  let default =
    Format.sprintf "%.1f" Configuration.default_reconnection_delay
  in
  let doc =
    Format.asprintf
      "The first reconnection delay, in seconds, to wait before reconnecting \
       to the Tezos node. The default delay is %s.\n\
       The actual delay varies to follow a randomized exponential backoff \
       (capped to 1.5h): [1.5^reconnection_attempt * delay ± 50%%]."
      default
  in
  Clic.default_arg
    ~long:"reconnection-delay"
    ~placeholder:"delay"
    ~doc
    ~default
    (Clic.parameter (fun _ p ->
         try return (float_of_string p) with _ -> failwith "Cannot read float"))

let filename_arg =
  Clic.default_arg
    ~long:"filename"
    ~placeholder:"filename"
    ~doc:"The path to the file to import."
    ~default:"import.in"
    Client_proto_args.string_parameter

let pvm_name_arg =
  Clic.default_arg
    ~long:"pvm-name"
    ~placeholder:"pvm_name"
    ~doc:"The name of the PVM."
    ~default:"arith"
    Client_proto_args.string_parameter

let group =
  {
    Clic.name = "sc_rollup.node";
    title = "Commands related to the smart-contract rollup node.";
  }

let config_init_command =
  let open Lwt_result_syntax in
  let open Clic in
  command
    ~group
    ~desc:"Configure the smart-contract rollup node."
    (args7
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       loser_mode
       reconnection_delay_arg
       dal_node_addr_arg
       dal_node_port_arg)
    (prefix "init" @@ mode_param
    @@ prefixes ["config"; "for"]
    @@ sc_rollup_address_param
    @@ prefixes ["with"; "operators"]
    @@ seq_of_param @@ sc_rollup_node_operator_param)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           loser_mode,
           reconnection_delay,
           dal_node_addr,
           dal_node_port )
         mode
         sc_rollup_address
         sc_rollup_node_operators
         cctxt ->
      let open Configuration in
      let purposed_operators, default_operators =
        List.partition_map
          (function
            | `Purpose p_operator -> Left p_operator
            | `Default operator -> Right operator)
          sc_rollup_node_operators
      in
      let default_operator =
        match default_operators with
        | [] -> None
        | [default_operator] -> Some default_operator
        | _ -> Stdlib.failwith "Multiple default operators"
      in
      let sc_rollup_node_operators =
        Configuration.make_purpose_map
          purposed_operators
          ~default:default_operator
      in
      let config =
        {
          data_dir;
          sc_rollup_address;
          sc_rollup_node_operators;
          rpc_addr;
          rpc_port;
          reconnection_delay;
          dal_node_addr;
          dal_node_port;
          fee_parameters = Operator_purpose_map.empty;
          mode;
          loser_mode;
        }
      in
      let*? config = check_mode config in
      save config >>=? fun () ->
      cctxt#message
        "Smart-contract rollup node configuration written in %s"
        (filename config)
      >>= fun _ -> return ())

let run_command =
  let open Clic in
  command
    ~group
    ~desc:"Run the rollup daemon."
    (args1 data_dir_arg)
    (prefixes ["run"] @@ stop)
    (fun data_dir cctxt -> Daemon.run ~data_dir cctxt >>=? fun () -> return ())

let import_command =
  let open Clic in
  command
    ~group
    ~desc:"Run the rollup daemon."
    (args3 data_dir_arg filename_arg pvm_name_arg)
    (prefixes ["import"] @@ stop)
    (fun (data_dir, filename, pvm_name) cctxt ->
      let hash = Reveals.import ~data_dir ~filename ~pvm_name in
      cctxt#message "%a" Protocol.Alpha_context.Sc_rollup.Reveal_hash.pp hash
      >>= return)

let sc_rollup_commands () =
  List.map
    (Clic.map_command (new Protocol_client_context.wrap_full))
    [config_init_command; run_command; import_command]

let select_commands _ _ =
  return (sc_rollup_commands () @ Client_helpers_commands.commands ())

let () = Client_main_run.run (module Daemon_config) ~select_commands
