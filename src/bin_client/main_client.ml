(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Client_config

let disable_disclaimer =
  match Sys.getenv_opt "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER" with
  | Some ("yes" | "y" | "YES" | "Y") -> true
  | _ -> false

let timeout_seconds () =
  let default_value = 10 in
  let varname = "TEZOS_CLIENT_RPC_TIMEOUT_SECONDS" in
  match Sys.getenv_opt varname with
  | None -> default_value
  | Some s -> (
      match int_of_string_opt s with
      | Some i when i > 0 -> i
      | Some invalid ->
          Format.eprintf
            "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
             The value read from environment variable@ @[<h 0>%s := %d@]@ is \
             invalid as a timeout interval:@ It must be a strictly positive \
             integral number@ indicating the timeout period (in seconds).@,\
             @\n\
            \ Using the default value (:= %d seconds per RPC call).@]@\n\
             @."
            varname
            invalid
            default_value ;
          default_value
      | None ->
          Format.eprintf
            "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
             The value read from environment variable@ @[<h 0>%s := %s@]@ is \
             malformed as a timeout interval:@ It must be a strictly positive \
             integral number@ indicating the timeout period (in seconds).@,\
             @\n\
            \ Using the default value (:= %d seconds per RPC call).@]@\n\
             @."
            varname
            s
            default_value ;
          default_value)

let testnet_disclaimer () =
  if not disable_disclaimer then
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
       @,\
      \               This is @{<warning>NOT@} the Tezos Mainnet.@,\
       @,\
      \         Do @{<warning>NOT@} use your fundraiser keys on this network.\n\
       @."

let mainnet_disclaimer () =
  if not disable_disclaimer then
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Disclaimer@}@}@,\
       The  Tezos  network  is  a  new  blockchain technology.@,\
       Users are  solely responsible  for any risks associated@,\
       with usage of the Tezos network.  Users should do their@,\
       own  research to determine  if Tezos is the appropriate@,\
       platform for their needs and should apply judgement and@,\
       care in their network interactions.@]@\n\
       @."

(** [rpc_timeout ~timeout ?canceler f ctxt] returns an Lwt.t promise that
    attempts to execute [f] (a closure of type ['a -> 'b tzresult Lwt.t],
    which typically performs an RPC call) over input [ctxt] (of type ['a])
    within a timeout window of [timeout] seconds, using [Error_monad.with_timeout].
    If the promise returned by [f ctxt] resolves before [timeout] seconds have
    elapsed, then the promise returned by [rpc_timeout] will resolve to the same value
    as [f ctxt].

    In the case that the promise returned by [f ctxt] is not resolved within [timeout] seconds,
    the call to [f ctxt] is instead cancelled through the use of an [Error_monad.protect] wrapper,
    which is parametrized over the passed-in value of the optional parameter [canceler],
    or a newly created [Lwt_canceler.t] if said parameter is omitted.
    The promise returned by [rpc_timeout] is then determined by the implemented behavior of [Error_monad.with_timeout].
*)
let rpc_timeout ~timeout ?canceler f ctxt =
  let canceler = Option.value_f ~default:Lwt_canceler.create canceler in
  let request = Error_monad.protect ~canceler (fun () -> f ctxt) in
  let alarm =
    Tezos_stdlib_unix.Systime_os.sleep (Ptime.Span.of_int_s timeout)
  in
  Error_monad.with_timeout ~canceler alarm (fun _ -> request)

let check_network ~timeout ctxt =
  let open Lwt_syntax in
  let* r = rpc_timeout ~timeout Version_services.version ctxt in
  match r with
  | Error _ -> Lwt.return_none
  | Ok {network_version; _} ->
      let has_prefix prefix =
        String.has_prefix ~prefix (network_version.chain_name :> string)
      in
      if List.exists has_prefix ["TEZOS_BETANET"; "TEZOS_MAINNET"] then (
        mainnet_disclaimer () ;
        Lwt.return_some `Mainnet)
      else (
        testnet_disclaimer () ;
        Lwt.return_some `Testnet)

let get_commands_for_version ~timeout ctxt network chain block protocol =
  let open Lwt_syntax in
  let* r =
    rpc_timeout
      ~timeout
      (fun ctxt -> Shell_services.Blocks.protocols ctxt ~chain ~block ())
      ctxt
  in
  match r with
  | Ok {next_protocol = version; _} -> (
      match protocol with
      | None ->
          return
            (Some version, Client_commands.commands_for_version version network)
      | Some given_version ->
          if not (Protocol_hash.equal version given_version) then
            Format.eprintf
              "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
               The protocol provided via `--protocol` (%a)@,\
               is not the one retrieved from the node (%a).@]@\n\
               @."
              Protocol_hash.pp_short
              given_version
              Protocol_hash.pp_short
              version ;
          return
            ( Some version,
              Client_commands.commands_for_version given_version network ))
  | Error errs -> (
      match protocol with
      | None ->
          Format.eprintf
            "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
             Failed to acquire the protocol version from the node@,\
             %a@]@\n\
             @."
            (Format.pp_print_list pp)
            errs ;
          return (None, [])
      | Some version ->
          return
            (Some version, Client_commands.commands_for_version version network)
      )

let select_commands ctxt {chain; block; protocol; _} =
  let open Lwt_syntax in
  let timeout = timeout_seconds () in
  let* network = check_network ~timeout ctxt in
  let* _, commands_for_version =
    get_commands_for_version ~timeout ctxt network chain block protocol
  in
  Lwt.return_ok
    (Client_rpc_commands.commands
    @ Tezos_signer_backends_unix.Ledger.commands ()
    @ Client_keys_commands.commands network
    @ Client_helpers_commands.commands ()
    @ Mockup_commands.commands ()
    @ Tezos_proxy.Proxy_commands.commands ()
    @ Client_smart_rollup_commands.commands ()
    @ commands_for_version)

let () = Client_main_run.run (module Client_config) ~select_commands
