(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let search_gateway () =
  match
    Octez_igd_next.Igd_next_gen.search_gateway
      ~bind_addr:None
      ~broadcast_address:None
      ~timeout:None
      ~single_search_timeout:None
  with
  | Ok _ ->
      Format.printf "Gateway found\n%!" ;
      Lwt.return_unit
  | Error e ->
      Format.eprintf "Cannot find gateway: %s\n%!" e ;
      Lwt.return_unit

let search_gateway () = Tezos_base_unix.Event_loop.main_run search_gateway

module Term = struct
  let process _ = `Ok (search_gateway ())

  let docs = "UPNP OPTIONS"

  let search_gateway =
    let doc = "" in
    Cmdliner.Arg.(value & flag & info ~docs ~doc ["search-gateway"])

  let term = Cmdliner.Term.(ret (const process $ search_gateway))
end

module Manpage = struct
  let command_description = "Experimental"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:"Experimental" ~man "search-gateway"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
