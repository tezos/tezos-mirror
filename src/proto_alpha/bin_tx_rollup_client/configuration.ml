(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Clic
open Lwt_result_syntax
module Base = Tezos_client_base

type t = {base_dir : string; wallet_dir : string; endpoint : Uri.t}

let default_base_dir = Filename.concat (Sys.getenv "HOME") ".tezos-client"

let default_tezos_base_dir =
  Tezos_client_base_unix.Client_config.default_base_dir

let default_endpoint = "http://localhost:9999"

let default =
  {
    base_dir = default_base_dir;
    wallet_dir = default_tezos_base_dir;
    endpoint = Uri.of_string default_endpoint;
  }

let valid_endpoint _configuration s =
  let endpoint = Uri.of_string s in
  match (Uri.scheme endpoint, Uri.query endpoint, Uri.fragment endpoint) with
  | Some ("http" | "https"), [], None -> return endpoint
  | _ -> failwith "Endpoint should be of the form http[s]://address:port"

let endpoint_arg () =
  arg
    ~long:"endpoint"
    ~short:'E'
    ~placeholder:"uri"
    ~doc:
      (Printf.sprintf
         "endpoint of the tx rollup node; e.g. '%s'"
         default_endpoint)
  @@ parameter valid_endpoint

let valid_dir _configuration base_dir =
  match Sys.is_directory base_dir with
  | true -> return base_dir
  | false | (exception Sys_error _) ->
      failwith "%s does not seem to be an existing directory" base_dir

let base_dir_arg () =
  arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:
      (Format.asprintf
         "@[<v>@[<2>Tezos transaction rollup client data directory@,\
          The directory where the Tezos transaction rollup client stores its \
          data.@,\
          If absent, its value defaults to %s@]@]@."
         default_base_dir)
    (parameter valid_dir)

let wallet_dir_arg () =
  arg
    ~long:"wallet-dir"
    ~short:'w'
    ~placeholder:"path"
    ~doc:
      (Format.asprintf
         "@[<v>@[<2>Wallet directory@,\
          The directory where to look for known keys.@,\
          If absent, its value defaults to %s@]@]@."
         default_tezos_base_dir)
    (parameter valid_dir)

let global_options () =
  Clic.args3 (base_dir_arg ()) (wallet_dir_arg ()) (endpoint_arg ())

let make (base_dir, wallet_dir, endpoint) =
  {
    base_dir = Option.value base_dir ~default:default_base_dir;
    wallet_dir = Option.value wallet_dir ~default:default_tezos_base_dir;
    endpoint = Option.value endpoint ~default:(Uri.of_string default_endpoint);
  }

let parse argv =
  let* opts, argv =
    Clic.parse_global_options (global_options ()) default argv
  in
  return (make opts, argv)

class type tx_client_context =
  object
    inherit Base.Client_context.io_wallet

    inherit RPC_context.generic
  end

class unix_tx_client_context ~wallet_dir ~password_filename ~rpc_config :
  tx_client_context =
  object
    inherit
      Client_context_unix.unix_io_wallet ~base_dir:wallet_dir ~password_filename

    inherit
      Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
        rpc_config
        (Tezos_rpc_http.Media_type.Command_line.of_command_line
           rpc_config.media_type)
  end

(* for the moment the base_dir is not used but it's going to be soon when for
    example we save the config *)
let make_unix_client_context {base_dir = _; wallet_dir; endpoint} =
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in

  new unix_tx_client_context ~wallet_dir ~rpc_config ~password_filename:None
