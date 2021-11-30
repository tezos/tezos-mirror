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

(*

   Helpers
   =======

*)
let test ~__FILE__ ~output_file ?(tags = []) title =
  Protocol.register_regression_test
    ~output_file
    ~__FILE__
    ~title
    ~tags:("sc_rollup" :: tags)

let setup f ~protocol =
  let sc_rollup_enable = [(["sc_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base sc_rollup_enable in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

(*

   Tests
   =====

*)

(* Originate a new SCORU of the arithmetic kind
   --------------------------------------------

   - Rollup addresses are fully determined by operation hashes and origination nonce.

*)
let test_origination =
  let output_file = "sc_rollup_origination" in
  test
    ~__FILE__
    ~output_file
    "origination of a SCORU executes without error"
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* rollup_address =
        Client.originate_sc_rollup
          ~burn_cap:Tez.(of_int 9999999)
          ~src:bootstrap1_key
          ~kind:"arith"
          ~boot_sector:""
          client
      in
      let* () = Client.bake_for client in
      Regression.capture rollup_address ;
      return ())

(* Configuration of a rollup node
   ------------------------------

   A rollup node has a configuration file that must be initialized.

*)
let with_fresh_rollup f tezos_node tezos_client bootstrap1_key =
  let* rollup_address =
    Client.originate_sc_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"arith"
      ~boot_sector:""
      tezos_client
  in
  let sc_rollup_node = Sc_rollup_node.create tezos_node in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node rollup_address
  in
  let* () = Client.bake_for tezos_client in
  f rollup_address sc_rollup_node configuration_filename

let test_rollup_node_configuration =
  let output_file = "sc_rollup_node_configuration" in
  test
    ~__FILE__
    ~output_file
    "configuration of a smart contract optimistic rollup node"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun _rollup_address _sc_rollup_node filename ->
      let read_configuration =
        let open Ezjsonm in
        match from_channel (open_in filename) with
        | `O fields ->
            (* Remove 'data-dir' as it is non deterministic. *)
            `O (List.filter (fun (s, _) -> s <> "data-dir") fields) |> to_string
        | _ ->
            failwith "The configuration file does not have the expected format."
      in
      Regression.capture read_configuration ;
      return ())

let register ~protocols =
  test_origination ~protocols ;
  test_rollup_node_configuration ~protocols
