(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type prometheus = {metrics_addr : string; metrics_port : int}

type contracts = {
  sequencer_governance : string;
  kernel_governance : string;
  security_kernel_governance : string;
}

type configuration = {
  prometheus : prometheus;
  endpoint : Uri.t;
  contracts : contracts;
}

let mainnet_contracts =
  {
    sequencer_governance = "KT1NcZQ3y9Wv32BGiUfD2ZciSUz9cY1DBDGF";
    kernel_governance = "KT1H5pCmFuhAwRExzNNrPQFKpunJx1yEVa6J";
    security_kernel_governance = "KT1N5MHQW5fkqXkW9GPjRYfn5KwbuYrvsY1g";
  }

let ghostnet_contracts =
  {
    sequencer_governance = "KT1FRzozuzFMWLimpFeSdADHTMxzU8KtgCr9";
    kernel_governance = "KT1HfJb718fGszcgYguA4bfTjAqe1BEmFHkv";
    security_kernel_governance = "KT1QDgF5pBkXEizj5RnmagEyxLxMTwVRpmYk";
  }

let default_metrics_addr = "127.0.0.1"

let default_metrics_port = "9191"

let default_endpoint = "http://127.0.0.1:8732"

module Params = struct
  let string : (string, unit) Tezos_clic.parameter =
    Tezos_clic.parameter (fun _ s -> Lwt.return_ok s)

  let int : (int, unit) Tezos_clic.parameter =
    Tezos_clic.parameter (fun _ s -> Lwt.return_ok (int_of_string s))
end

type network_argument_not_found_error = Both_args | No_args

let network_argument_not_found ~error =
  match error with
  | Both_args ->
      Stdlib.failwith
        "You can specify only one network, use only --etherlink-mainnet or \
         --etherlink-ghostnet.\n"
  | No_args ->
      Stdlib.failwith
        "You must provide an argument to specify the network, use \
         --etherlink-mainnet or --etherlink-ghostnet.\n"

module Args = struct
  let metrics_addr =
    Tezos_clic.default_arg
      ~long:"metrics-addr"
      ~doc:"Metrics address of the prometheus server."
      ~placeholder:"<metrics-addr>"
      ~default:default_metrics_addr
      Params.string

  let metrics_port =
    Tezos_clic.default_arg
      ~long:"metrics-port"
      ~doc:"Metrics port of the prometheus server."
      ~placeholder:"<metrics-port>"
      ~default:default_metrics_port
      Params.int

  let endpoint =
    Tezos_clic.default_arg
      ~long:"endpoint"
      ~doc:"RPC endpoint (L1 octez node)."
      ~placeholder:"<endpoint>"
      ~default:default_endpoint
      Params.string

  let etherlink_mainnet : (bool, unit) Tezos_clic.arg =
    Tezos_clic.switch
      ~long:"etherlink-mainnet"
      ~doc:"Enable configuration for mainnet's governance contracts."
      ()

  let etherlink_ghostnet : (bool, unit) Tezos_clic.arg =
    Tezos_clic.switch
      ~long:"etherlink-ghostnet"
      ~doc:"Enable configuration for ghostnet's governance contracts."
      ()
end
