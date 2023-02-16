(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file client_simulation_flag.ml
   Subject:      Tests the behavior of the --simulation flag.
*)

let test_client =
  Protocol.register_test
    ~__FILE__
    ~title:"Test client simulation"
    ~tags:["client"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  Log.info "Import an encrypted account with some tez" ;
  let encrypted_account =
    {
      alias = "encrypted_account";
      public_key_hash = "";
      public_key = "";
      Account.secret_key =
        Encrypted
          "edesk1n2uGpPtVaeyhWkZzTEcaPRzkQHrqkw5pk8VkZvp3rM5KSc3mYNH5cJEuNcfB91B3G3JakKzfLQSmrgF4ht";
    }
  in
  let* () =
    Client.import_encrypted_secret_key
      client
      encrypted_account
      ~password:"password"
  in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 100)
      ~giver:"bootstrap1"
      ~receiver:encrypted_account.alias
      client
  in
  Log.info "Tests that --simulation does not ask for the key password." ;
  let* () =
    Client.transfer
      ~amount:Tez.one
      ~giver:"encrypted_account"
      ~receiver:Constant.bootstrap1.alias
      ~simulation:true
      ~burn_cap:Tez.one
      client
  in
  Log.info "Tests that the client asks for the password w/o --simulation" ;
  let process =
    Client.spawn_transfer
      ~amount:Tez.one
      ~giver:"encrypted_account"
      ~receiver:Constant.bootstrap1.alias
      client
      ~burn_cap:Tez.one
  in
  let* stdout = Process.check_and_read_stdout ~expect_failure:true process in
  let* () =
    if stdout =~ rex "End_of_file" then unit
    else Test.fail "Transfer should have failed."
  in
  Log.info "Tests that --simulation does not ask for the key password" ;
  let* delegate = Client.gen_and_show_keys ~alias:"delegate" client in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 100)
      ~giver:Constant.bootstrap1.alias
      ~receiver:delegate.alias
      ~burn_cap:Tez.one
      client
  in
  let* _delegate = Client.register_delegate ~delegate:delegate.alias client in
  let*! () =
    Client.set_delegate
      ~src:"encrypted_account"
      ~delegate:delegate.public_key_hash
      ~simulation:true
      client
  in
  Log.info "Tests that the client asks for the password w/o --simulation" ;
  let*? process =
    Client.set_delegate
      client
      ~src:"encrypted_account"
      ~delegate:delegate.public_key_hash
  in
  let* stdout = Process.check_and_read_stdout ~expect_failure:true process in
  let* () =
    if stdout =~ rex "End_of_file" then unit
    else Test.fail "Delegating should have failed."
  in
  unit

let register ~protocols = test_client protocols
