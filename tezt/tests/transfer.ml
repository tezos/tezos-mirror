(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client / Transfer
   Invocation:   dune exec tezt/tests/main.exe -- --file transfer.ml
   Subject:      Test transfers
*)

let test_zero_transfer_to_implicit_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Test Zero Transfer to Implicit Contract"
    ~tags:["client"; "transfer"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let pubkey = Account.Bootstrap.keys.(2).public_key_hash in
  let err =
    rexf
      "Transactions of 0ꜩ towards a contract without code are forbidden \
       \\(%s\\)."
      pubkey
  in
  Client.spawn_transfer
    ~amount:Tez.zero
    ~giver:(Account.Bootstrap.alias 2)
    ~receiver:(Account.Bootstrap.alias 3)
    client
  |> Process.check_error ~msg:err

let test_zero_transfer_to_nonexistent_contract =
  Protocol.register_test
    ~__FILE__
    ~title:"Test Zero Transfer to Nonexistent Contract"
    ~tags:["client"; "transfer"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let nonexistent = "KT1Fcq4inD44aMhmUiTEHR1QMQwJT7p2u641" in
  let err = rexf "Contract %s does not exist" nonexistent in
  Client.spawn_transfer
    ~amount:Tez.zero
    ~giver:(Account.Bootstrap.alias 2)
    ~receiver:nonexistent
    client
  |> Process.check_error ~msg:err

(* Tests that the client stops asking for the password after
   three erroneous passwords given by the user *)
let test_encrypted_source_stop_loop_password =
  Protocol.register_test
    ~__FILE__
    ~title:"Test invalid inputs when transferring from encrypted source account"
    ~tags:["client"; "transfer"; "stop"; "loop"; "password"]
    ~uses_node:false
  @@ fun protocol ->
  Log.info "Import an encrypted account with some tez" ;
  let* client = Client.init_mockup ~protocol () in
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
    let Account.{alias; secret_key; _} = encrypted_account in
    Client.import_encrypted_secret_key
      client
      ~alias
      secret_key
      ~password:"password"
  in
  (* Fund the encrypted account *)
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 100)
      ~giver:"bootstrap1"
      ~receiver:encrypted_account.alias
      client
  in
  let giver = "encrypted_account" in
  let receiver = Constant.bootstrap1.alias in
  Log.info "Stops after three tries" ;
  let* () =
    ["\n\n\n"; "\n\n\nign"; "\n\n\npassword"]
    |> Lwt_list.iter_s (fun stdin ->
           let process, output_channel =
             Client.spawn_command_with_stdin
               client
               ["transfer"; "0.1"; "from"; giver; "to"; receiver]
           in
           let* () = Lwt_io.write_line output_channel stdin in
           let* () = Lwt_io.close output_channel in
           Process.check_error process ~msg:(rex "3 incorrect password attempt"))
  in
  Log.info "Password succeed before three tries" ;
  let* () =
    ["password\n"; "\npassword\n"; "\n\npassword\n"]
    |> Lwt_list.iter_s (fun stdin ->
           let process, output_channel =
             Client.spawn_command_with_stdin
               client
               ["transfer"; "0.1"; "from"; giver; "to"; receiver]
           in
           let* () = Lwt_io.write_line output_channel stdin in
           let* () = Lwt_io.close output_channel in
           Process.check process)
  in
  unit

let register ~protocols =
  test_zero_transfer_to_implicit_contract protocols ;
  test_zero_transfer_to_nonexistent_contract protocols ;
  test_encrypted_source_stop_loop_password protocols
