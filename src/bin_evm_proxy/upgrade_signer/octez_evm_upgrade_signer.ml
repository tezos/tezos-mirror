(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

open Evm_proxy_lib

let from_hex s =
  let open Lwt_result_syntax in
  let hex = `Hex s in
  match Hex.to_string hex with
  | Some s -> return s
  | None -> failwith "Parameter %s is an invalid hexadecimal value." s

let secret_key_parameter =
  let open Tezos_clic in
  param ~name:"secret-key" ~desc:"Dictator's secret key"
  @@ parameter (fun _ s ->
         let open Lwt_result_syntax in
         let* sk = from_hex s in
         match
           Data_encoding.Binary.of_string_opt
             Tezos_crypto.Signature.Secp256k1.Secret_key.encoding
             sk
         with
         | Some sk -> return sk
         | None -> failwith "Parameter '%s' is an invalid secret key." sk)

let nonce_parameter =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  param ~name:"nonce" ~desc:"Dictator's upgrade nonce"
  @@ parameter (fun _ nonce ->
         return (Ethereum_types.u16_to_bytes @@ int_of_string nonce))

let root_hash_parameter =
  let open Tezos_clic in
  param ~name:"root-hash" ~desc:"Preimage root hash to upgrade to"
  @@ parameter (fun _ root_hash -> from_hex root_hash)

let rollup_parameter =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  param ~name:"rollup" ~desc:"EVM rollup address to upgrade to"
  @@ parameter (fun _ address ->
         let address_opt =
           Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_opt address
         in
         match address_opt with
         | Some address ->
             return
             @@ Data_encoding.Binary.to_string_exn
                  Tezos_crypto.Hashed.Smart_rollup_address.encoding
                  address
         | None ->
             failwith
               "Parameter '%s' is invalid: it is expected to be a smart rollup \
                address encoded in a base58 string."
               address)

let main_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Sign the upgrade kernel message"
    Tezos_clic.no_options
    (prefixes ["sign"; "upgrade"; "for"]
    @@ rollup_parameter
    @@ prefixes ["with"; "nonce"]
    @@ nonce_parameter
    @@ prefixes ["and"; "root_hash"]
    @@ root_hash_parameter
    @@ prefixes ["and"; "secret"; "key"]
    @@ secret_key_parameter @@ stop)
    (fun () rollup_address_bytes nonce_bytes root_hash_bytes secret_key () ->
      let message_to_sign =
        rollup_address_bytes ^ nonce_bytes ^ root_hash_bytes
      in
      let signature =
        Tezos_crypto.Signature.Secp256k1.sign_keccak256
          secret_key
          (String.to_bytes message_to_sign)
      in
      let sig_bytes =
        Data_encoding.Binary.to_string_exn
          Tezos_crypto.Signature.Secp256k1.encoding
          signature
      in
      let upgrade_tag_bytes = "\003" in
      let full_external_message =
        Hex.show @@ Hex.of_string @@ rollup_address_bytes ^ upgrade_tag_bytes
        ^ nonce_bytes ^ root_hash_bytes ^ sig_bytes
      in
      Printf.printf "Signed message:\n%s\n" (Hex.show @@ Hex.of_string sig_bytes) ;
      Printf.printf "Full external message:\n%s\n" full_external_message ;
      return_unit)

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let dispatch args =
  let open Lwt_result_syntax in
  let* ctx, args =
    Tezos_clic.parse_global_options Tezos_clic.no_options () args
  in
  Tezos_clic.dispatch [main_command] ctx args

let executable_name = Filename.basename Sys.executable_name

let handle_error = function
  | Ok _ -> ()
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options:Tezos_clic.no_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let () = Lwt_main.run (dispatch (argv ())) |> handle_error
