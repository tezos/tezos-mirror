(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Basic
   Invocation:   dune exec tezt/tests/main.exe -- --file basic.ml
   Subject:      .
*)

(* This example is included in the documentation (docs/developers/tezt.rst).
   It is part of the tests to ensure we keep it up-to-date. *)
let check_node_initialization (history_mode : Node.history_mode) :
    Protocol.t list -> unit =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf "node initialization (%s mode)" (Node.show_history_mode history_mode))
    ~tags:["basic"; "node"; Node.show_history_mode history_mode]
  @@ fun protocol ->
  let metrics_addr = Constant.default_host in
  let metrics_port = Port.fresh () in
  let* node =
    Node.init [History_mode history_mode] ~metrics_addr ~metrics_port
  in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  let* () = repeat 10 (fun () -> Client.bake_for_and_wait client) in
  Log.info "Baked 10 blocks." ;
  let* level = Node.wait_for_level node 11 in
  Log.info "Level is now %d." level ;
  let* identity = Node.wait_for_identity node in
  if identity = "" then Test.fail "identity is empty" ;
  Log.info "Identity is not empty." ;
  let addr = Format.sprintf "http://%s:%d/metrics" metrics_addr metrics_port in
  let* metrics =
    Process.spawn ~log_output:false "curl" ["-s"; addr]
    |> Process.check_and_read_stdout
  in
  if metrics = "" then Test.fail "Unable to read metrics" else return ()

(* This test checks that tests with tag [ci_disabled] do not run in the CI.
   It happened once when [Test.run] was accidentally called twice.
   To make sure that the test is selected by Manifezt often enough,
   it is tagged with some [~uses] that are often selected. *)
let () =
  Test.register
    ~__FILE__
    ~title:"meta: failing test with tag ci_disabled"
    ~tags:[Tag.ci_disabled]
    ~uses:
      [
        Constant.octez_node;
        Constant.octez_client;
        Constant.octez_dal_node;
        Constant.octez_dac_node;
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
      ]
  @@ fun () ->
  Test.fail
    "This test is not supposed to be run because it has tag ci_disabled. If it \
     does run (in the CI), it means that the tag ci_disabled is not working \
     properly anymore."

let register ~protocols =
  check_node_initialization Archive protocols ;
  check_node_initialization (Full None) protocols ;
  check_node_initialization (Rolling None) protocols
