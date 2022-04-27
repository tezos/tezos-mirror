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

type error += Bad_minimal_fees of string

let () =
  register_error_kind
    `Permanent
    ~id:"badMinimalFeesArg"
    ~title:"Bad -minimal-fees arg"
    ~description:"invalid fee threshold in -fee-threshold"
    ~pp:(fun ppf literal ->
      Format.fprintf ppf "invalid minimal fees '%s'" literal)
    Data_encoding.(obj1 (req "parameter" string))
    (function Bad_minimal_fees parameter -> Some parameter | _ -> None)
    (fun parameter -> Bad_minimal_fees parameter)

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
    ~name:"node-operator"
    ~desc:"Public key hash of the the smart-contract rollup node operator"
    (Clic.parameter (fun _ s ->
         match Signature.Public_key_hash.of_b58check_opt s with
         | None ->
             failwith "Could not read public key hash for rollup node operator"
         | Some pkh -> return pkh))

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

let minimal_fees_arg =
  let open Protocol.Alpha_context in
  let default =
    Configuration.default_fee_parameter.minimal_fees |> Tez.to_string
  in
  Clic.default_arg
    ~long:"minimal-fees"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees lower than this threshold (in tez) when \
       injecting."
    ~default
    (Clic.parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> failwith "Bad minimal fees"))

let minimal_nanotez_per_gas_unit_arg =
  let default =
    Configuration.default_fee_parameter.minimal_nanotez_per_gas_unit
    |> Q.to_string
  in
  Clic.default_arg
    ~long:"minimal-nanotez-per-gas-unit"
    ~placeholder:"amount"
    ~doc:
      "exclude operations with fees per gas lower than this threshold (in \
       nanotez) when injecting."
    ~default
    (Clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

let minimal_nanotez_per_byte_arg =
  let default =
    Configuration.default_fee_parameter.minimal_nanotez_per_byte |> Q.to_string
  in
  Clic.default_arg
    ~long:"minimal-nanotez-per-byte"
    ~placeholder:"amount"
    ~default
    ~doc:
      "exclude operations with fees per byte lower than this threshold (in \
       nanotez) when injecting."
    (Clic.parameter (fun _ s ->
         try return (Q.of_string s) with _ -> fail (Bad_minimal_fees s)))

let force_low_fee_arg =
  Clic.switch
    ~long:"force-low-fee"
    ~doc:
      "Don't check that the fee is lower than the estimated default value when \
       injecting."
    ()

let fee_cap_arg =
  let open Protocol.Alpha_context in
  let default = Configuration.default_fee_parameter.fee_cap |> Tez.to_string in
  Clic.default_arg
    ~long:"fee-cap"
    ~placeholder:"amount"
    ~default
    ~doc:"Set the fee cap when injecting."
    (Clic.parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> failwith "Bad fee cap"))

let burn_cap_arg =
  let open Protocol.Alpha_context in
  let default = Configuration.default_fee_parameter.burn_cap |> Tez.to_string in
  Clic.default_arg
    ~long:"burn-cap"
    ~placeholder:"amount"
    ~default
    ~doc:"Set the burn cap when injecting."
    (Clic.parameter (fun _ s ->
         match Tez.of_string s with
         | Some t -> return t
         | None -> failwith "Bad burn cap"))

let group =
  {
    Clic.name = "sc_rollup.node";
    title = "Commands related to the smart-contract rollup node.";
  }

let config_init_command =
  let open Clic in
  command
    ~group
    ~desc:"Configure the smart-contract rollup node."
    (args9
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       minimal_fees_arg
       minimal_nanotez_per_byte_arg
       minimal_nanotez_per_gas_unit_arg
       force_low_fee_arg
       fee_cap_arg
       burn_cap_arg)
    (prefixes ["config"; "init"; "on"]
    @@ sc_rollup_address_param
    @@ prefixes ["with"; "operator"]
    @@ sc_rollup_node_operator_param stop)
    (fun ( data_dir,
           rpc_addr,
           rpc_port,
           minimal_fees,
           minimal_nanotez_per_byte,
           minimal_nanotez_per_gas_unit,
           force_low_fee,
           fee_cap,
           burn_cap )
         sc_rollup_address
         sc_rollup_node_operator
         cctxt ->
      let open Configuration in
      let config =
        {
          data_dir;
          sc_rollup_address;
          sc_rollup_node_operator;
          rpc_addr;
          rpc_port;
          minimal_fees;
          minimal_nanotez_per_byte;
          minimal_nanotez_per_gas_unit;
          force_low_fee;
          fee_cap;
          burn_cap;
        }
      in
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

let sc_rollup_commands () =
  List.map
    (Clic.map_command (new Protocol_client_context.wrap_full))
    [config_init_command; run_command]

let select_commands _ _ =
  return (sc_rollup_commands () @ Client_helpers_commands.commands ())

let main () = ()

let () = Client_main_run.run (module Client_config) ~select_commands
