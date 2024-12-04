(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let toplog (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  Log.info ~prefix:"TOP" ~color:Log.Color.FG.green fmt

let init_teztale cloud agent =
  let () = toplog "Initialize Teztale server" in
  let* teztale = Tezos.Teztale.run_server agent in
  let* () = Tezos.Teztale.wait_server teztale in
  let () = toplog "Teztale server is ready" in
  let* () =
    let domain = Agent.point agent |> Option.fold ~none:"localhost" ~some:fst in
    let port = teztale.server.conf.interface.port in
    let url = sf "http://%s:%d" domain port in
    let () = toplog "Teztale server URL is '%s'" url in
    Cloud.add_service cloud ~name:"teztale" ~url
  in
  Lwt.return teztale
