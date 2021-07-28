(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_test.ml
   Subject:      Run the baker while performing a lot of transfers
*)

let to_account client alias =
  Client.show_address ~show_secret:true ~alias client

let sources_file_of_accounts (accounts : Account.key list) =
  let account_to_json (account : Account.key) =
    let mandatory name = function
      | None -> Test.fail "Unexpected Nothing in field %s" name
      | Some value -> value
    in
    `O
      [
        ("pkh", `String account.public_key_hash);
        ("pk", `String (account.public_key |> mandatory "public_key"));
        ("sk", `String (account.secret_key |> mandatory "secret_key"));
      ]
  in
  let accounts_json_obj = `A (List.map account_to_json accounts) in
  let sources = Temp.file "sources.json" in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Output sources (fun oc ->
        Lwt_io.fprintf oc "%s" @@ Ezjsonm.value_to_string accounts_json_obj)
  in
  return sources

let test_baker =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest"
    ~tags:["node"; "baker"]
  @@ fun protocol ->
  let* (node, client) = Client.init_activate_bake `Client ~protocol () in
  let* _ = Baker.init ~protocol node client in
  let keys : Constant.key list =
    List.filter
      (fun {Constant.alias; _} -> alias <> "activator")
      Constant.all_secret_keys
  in
  let* (accounts : Account.key list) =
    Lwt_list.map_s
      (fun (account : Constant.key) -> to_account client account.alias)
      keys
  in
  let* sources = sources_file_of_accounts accounts in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~sources ~transfers:100 client in
  Lwt.return_unit

let register ~protocols = test_baker ~protocols
