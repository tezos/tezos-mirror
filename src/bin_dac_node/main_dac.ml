(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

let group =
  {Tezos_clic.name = "dac-node"; title = "Commands related to the DAC node"}

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the DAC node data directory. Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let rpc_address_arg =
  let default = Configuration.default_rpc_address in
  Tezos_clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"rpc-address|ip"
    ~doc:
      (Format.sprintf
         "The address the DAC node listens to. Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let int_parameter =
  let open Tezos_clic in
  parameter (fun _ p ->
      let open Lwt_result_syntax in
      let* i =
        try Lwt.return_ok (int_of_string p)
        with _ -> failwith "Cannot read int"
      in
      if i < 0 then failwith "Parameter must be non-negative" else return i)

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Tezos_clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"rpc-port"
    ~doc:
      (Format.sprintf
         "The port the DAC node listens to. Default value is %s"
         default)
    ~default
    int_parameter

let config_init_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  command
    ~group
    ~desc:"Configure DAC node."
    (args3 data_dir_arg rpc_address_arg rpc_port_arg)
    (prefixes ["init-config"] stop)
    (fun (data_dir, rpc_address, rpc_port) cctxt ->
      let open Configuration in
      let config =
        {
          data_dir;
          rpc_address;
          rpc_port;
          mode = Legacy {threshold = 0; dac_members_addresses = []};
          reveal_data_dir = default_reveal_data_dir;
        }
      in
      let* () = save config in
      let*! _ =
        cctxt#message "DAC node configuration written in %s" (filename config)
      in
      return ())

(* DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4125
   Move the following commands to a dac node once we have one. *)
module Dac_client = struct
  let reveal_data_dir_arg =
    Tezos_clic.arg
      ~long:"reveal-data-dir"
      ~placeholder:"reveal-data-dir"
      ~doc:"The directory where reveal preimage pages are saved."
      (Client_config.string_parameter ())

  let threshold_arg =
    Tezos_clic.arg
      ~long:"threshold"
      ~placeholder:"threshold"
      ~doc:
        "The number of signatures needed from the Data Availability Committee \
         members to validate reveal data.)"
      int_parameter

  let tz4_address_parameter () =
    Tezos_clic.parameter (fun _cctxt s ->
        let open Lwt_result_syntax in
        let*? bls_pkh = Signature.Bls.Public_key_hash.of_b58check s in
        let pkh : Tezos_crypto.Aggregate_signature.public_key_hash =
          Tezos_crypto.Aggregate_signature.Bls12_381 bls_pkh
        in
        return pkh)

  let tz4_address_param ?(name = "public key hash")
      ?(desc = "bls public key hash to use") =
    let desc = String.concat "\n" [desc; "A tz4 address"] in
    Tezos_clic.param ~name ~desc (tz4_address_parameter ())

  (** Add an account alias as a member of the Data availability Committee in the
    configuration of the Dac node. *)
  let add_dac_alias_command =
    let open Lwt_result_syntax in
    let open Tezos_clic in
    command
      ~group
      ~desc:"Add an account alias as Data Availability Committee member"
      (args1 data_dir_arg)
      (prefixes ["add"; "data"; "availability"; "committee"; "member"]
      @@ tz4_address_param @@ stop)
      (fun data_dir dac_member_address cctxt ->
        let open Configuration in
        let* config = load ~data_dir in
        let* ({dac_members_addresses = old_dac_members_addresses; _} as
             legacy_config) =
          match config.mode with
          | Legacy config -> return config
          | _ -> failwith "Configuration is not in legacy mode"
        in
        if
          List.mem
            ~equal:Tezos_crypto.Aggregate_signature.Public_key_hash.equal
            dac_member_address
            old_dac_members_addresses
        then
          let*! _ =
            cctxt#message
              "Alias is already listed as a DAC member %s"
              (filename config)
          in
          return_unit
        else
          let dac_members_addresses =
            old_dac_members_addresses @ [dac_member_address]
          in
          let mode = Legacy {legacy_config with dac_members_addresses} in
          let* () = save {config with mode} in
          let*! _ =
            cctxt#message
              "DAC address added to configuration in %s"
              (filename config)
          in
          return_unit)

  (* DAC/TODO: https://gitlab.com/tezos/tezos/-/issues/4136
     Add option to specify a list of addresses from a file. *)
  let set_parameters_command =
    let open Lwt_result_syntax in
    let open Tezos_clic in
    command
      ~group
      ~desc:"Configure DAC parameters."
      (args3 data_dir_arg threshold_arg reveal_data_dir_arg)
      (prefixes ["set"; "dac"; "parameters"] stop)
      (fun (data_dir, threshold, reveal_data_dir) cctxt ->
        let open Configuration in
        let* config = load ~data_dir in
        let* legacy_config =
          match config.mode with
          | Legacy config -> return config
          | _ -> failwith "Only legacy mode supported."
        in
        let threshold =
          Option.value threshold ~default:legacy_config.threshold
        in
        let reveal_data_dir =
          Option.value reveal_data_dir ~default:config.reveal_data_dir
        in
        let mode = Legacy {legacy_config with threshold} in
        let config = {config with reveal_data_dir; mode} in
        let* () = save config in
        let*! _ =
          cctxt#message
            "DAC parameters set for configuration in %s"
            (filename config)
        in
        return ())

  let commands = [add_dac_alias_command; set_parameters_command]
end

let run_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Run the DAC node."
    (args1 data_dir_arg)
    (prefixes ["run"] @@ stop)
    (fun data_dir cctxt -> Daemon.run ~data_dir cctxt)

let commands () = [run_command; config_init_command] @ Dac_client.commands

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
