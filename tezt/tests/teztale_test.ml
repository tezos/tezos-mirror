(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Teztale

let team = Tag.layer1

let get_head server =
  let url =
    sf
      "http://%s:%d/head.json"
      server.Server.conf.interface.address
      server.Server.conf.interface.port
  in
  let*! res = Curl.get url in
  JSON.get "level" res |> JSON.as_int |> Lwt.return

let test =
  Protocol.register_test
    ~__FILE__
    ~title:"teztale basic test"
    ~tags:[team; "teztale"]
    ~uses:(fun _ -> [Constant.teztale_server; Constant.teztale_archiver])
  @@ fun protocol ->
  let final_level = Cli.get_int "final_level" ~default:10 in
  let user = {login = "ulogin"; password = "upassword"} in
  let admin = {login = "alogin"; password = "apassword"} in
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  let* () = Node.wait_for_ready node in
  let* server = Server.run ~users:[user] ~admin () in
  let* () =
    let* config =
      Process.spawn "cat" [server.filenames.conf_filename]
      |> Process.check_and_read_stdout
    in
    Log.info "Teztale server:\n%s\n" config ;
    Lwt.return_unit
  in
  let* _archiver =
    Archiver.run
      ~node_port:(Node.rpc_port node)
      user
      [server.Server.conf.interface]
  in
  let* () = Server.wait_for_readiness server in
  let* () = Client.bake_until_level ~target_level:final_level ~node client in
  let* head = get_head server in
  if head <> final_level then
    Test.fail "Teztale server did not reach level %d" final_level
  else Lwt.return_unit

let register ~protocols = test protocols
