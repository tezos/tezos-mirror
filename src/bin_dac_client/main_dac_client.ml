(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

type error += Invalid_positive_int_parameter of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.dac.invalid_positive_int_parameter"
    ~title:"Argument is not a positive integer"
    ~description:"Argument must be a positive integer"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Expected a valid positive integer, provided %s instead"
        reveal_data_path)
    Data_encoding.(obj1 (req "arg" string))
    (function Invalid_positive_int_parameter s -> Some s | _ -> None)
    (fun s -> Invalid_positive_int_parameter s)

let tz4_address_parameter =
  Tezos_clic.parameter (fun _cctxt s ->
      let open Lwt_result_syntax in
      let*? bls_pkh = Signature.Bls.Public_key_hash.of_b58check s in
      let pkh : Tezos_crypto.Aggregate_signature.public_key_hash =
        Tezos_crypto.Aggregate_signature.Bls12_381 bls_pkh
      in
      return pkh)

let _tz4_address_param ?(name = "public key hash")
    ?(desc = "bls public key hash to use") =
  let desc = String.concat "\n" [desc; "A tz4 address"] in
  Tezos_clic.param ~name ~desc tz4_address_parameter

let positive_int_parameter =
  Tezos_clic.parameter (fun _cctxt p ->
      let open Lwt_result_syntax in
      let* i =
        try Lwt.return_ok (int_of_string p)
        with _ -> tzfail @@ Invalid_positive_int_parameter p
      in
      if i < 0 then tzfail @@ Invalid_positive_int_parameter p else return i)

let wait_for_threshold_arg =
  Tezos_clic.arg
    ~long:"wait-for-threshold"
    ~placeholder:"wait-for-threshold"
    ~doc:
      (Format.sprintf
         "The amount of signatures required from a certificate before the \
          client returns")
    positive_int_parameter

let coordinator_rpc_parameter =
  Tezos_clic.parameter (fun _cctxt h ->
      match String.split ':' h with
      | [host; port] ->
          Lwt.catch
            (fun () ->
              Lwt_result.return
              @@ Dac_node_client.make_unix_cctxt
                   ~scheme:"http"
                   ~host
                   ~port:(int_of_string port))
            (fun _ -> failwith "Address not in format <rpc_address>:<rpc_port>")
      | _ -> failwith "Address not in format <rpc_address>:<rpc_port>")

let hex_parameter =
  Tezos_clic.parameter (fun _cctxt h ->
      let open Lwt_result_syntax in
      match Hex.to_bytes (`Hex h) with
      | None -> failwith "Parameter is not a valid hex-encoded string"
      | Some b -> return b)

let coordinator_rpc_param ?(name = "DAC coordinator rpc address parameter")
    ?(desc = "The address of the DAC coordinator") =
  let desc =
    String.concat "\n" [desc; "An address of the form <rpc_address>:<rpc_port>"]
  in
  Tezos_clic.param ~name ~desc coordinator_rpc_parameter

let hex_payload_param ?(name = "hex payload") ?(desc = "A hex encoded payload")
    =
  let desc = String.concat "\n" [desc; "A hex encoded string"] in
  Tezos_clic.param ~name ~desc hex_parameter

let hex_root_hash_param ?(name = "hex root hash")
    ?(desc = "A hex encoded root hash") =
  Tezos_clic.param
    ~name
    ~desc
    (Tezos_clic.map_parameter ~f:Dac_plugin.raw_hash_of_bytes hex_parameter)

let content_filename_param ?(name = "payload filename")
    ?(desc = "A filename containing a payload") =
  let desc = String.concat "\n" [desc; "Payload must be in binary format"] in
  Tezos_clic.param ~name ~desc (Client_config.string_parameter ())

let group =
  {
    Tezos_clic.name = "dac-client";
    title = "Dac client commands for interacting with a Dac node";
  }

let send_raw_payload threshold coordinator_cctxt payload =
  let open Lwt_result_syntax in
  let* root_hash = Command_handlers.send_preimage coordinator_cctxt payload in
  match threshold with
  | None ->
      return
      @@ Format.printf
           "Payload stored under root hash: %a"
           Dac_plugin.pp_raw_hash
           root_hash
  | Some threshold -> (
      let* certificate_opt =
        Command_handlers.wait_for_certificate
          coordinator_cctxt
          root_hash
          threshold
      in
      match certificate_opt with
      | None ->
          return
          @@ Format.printf
               "No certificate could be obtained.\n\
                Payload stored under root hash: %a\n"
               Dac_plugin.pp_raw_hash
               root_hash
      | Some certificate ->
          return
          @@ Format.printf "Certificate received: %a\n" Hex.pp certificate)

let send_dac_payload_input_from_command_line =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Send a list of strings to the DAC coordinator"
    (args1 wait_for_threshold_arg)
    (prefixes ["send"; "payload"; "to"; "coordinator"]
    @@ coordinator_rpc_param
    @@ prefixes ["with"; "content"]
    @@ hex_payload_param @@ stop)
    (fun threshold coordinator_cctxt payload _cctxt ->
      send_raw_payload threshold coordinator_cctxt payload)

let send_dac_payload_input_from_file =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Send the contents of a file as payload to the DAC coordinator."
    (args1 wait_for_threshold_arg)
    (prefixes ["send"; "payload"; "to"; "coordinator"]
    @@ coordinator_rpc_param
    @@ prefixes ["from"; "file"]
    @@ content_filename_param @@ stop)
    (fun threshold coordinator_cctxt filename _cctxt ->
      let open Lwt_result_syntax in
      let* payload =
        Lwt.catch
          (fun () ->
            let*! payload = Lwt_utils_unix.read_file filename in
            return @@ String.to_bytes payload)
          (fun exn ->
            Stdlib.failwith
              (Format.sprintf "Cannot read from file %s: %s" filename
              @@ Printexc.to_string exn))
      in
      send_raw_payload threshold coordinator_cctxt payload)

let get_dac_certificate =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Get a certificate from the coordinator"
    (args1 @@ constant ())
    (prefixes ["get"; "certificate"; "from"; "coordinator"]
    @@ coordinator_rpc_param
    @@ prefixes ["for"; "root"; "hash"]
    @@ hex_root_hash_param @@ stop)
    (fun () coordinator_cctxt root_hash _cctxt ->
      let open Lwt_result_syntax in
      let* certificate_opt =
        Command_handlers.get_certificate coordinator_cctxt root_hash
      in
      match certificate_opt with
      | None ->
          return
          @@ Format.printf
               "No certificate known for %a\n"
               Dac_plugin.pp_raw_hash
               root_hash
      | Some certificate ->
          return
          @@ Format.printf "Certificate received: %a\n" Hex.pp certificate)

let commands () =
  [
    send_dac_payload_input_from_command_line;
    send_dac_payload_input_from_file;
    get_dac_certificate;
  ]

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
