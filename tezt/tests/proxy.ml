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
   Component: Client - proxy mode
   Invocation: dune exec tezt/tests/main.exe -- --file proxy.ml
   Subject: Basic tests of the client's --mode proxy.
  *)

(* Test.
   Bake a few blocks in proxy mode.
 *)
let bake protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "bake (%s)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "bake"]
  @@ fun () ->
  let* node = Node.init [] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  Client.set_mode (Proxy node) client ;
  let* () = repeat 10 (fun () -> Client.bake_for client) in
  Log.info "Baked 10 blocks." ;
  let* level = Node.wait_for_level node 11 in
  Log.info "Level is now %d." level ;
  return ()

(* Test.
   Do some transfers and bakes the corresponding blocks in proxy mode.
 *)
let transfer protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "transfer (%s)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "transfer"]
  @@ fun () ->
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  Client.set_mode (Proxy node) client ;
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  Log.info "Baked 1 block." ;
  let* () =
    Client.transfer
      ~wait:"none"
      ~amount:5
      ~giver:"bootstrap1"
      ~receiver:"bootstrap2"
      client
  in
  Log.info "Transferred 5 tez." ;
  let* () = Client.bake_for client in
  Log.info "Baked block for bootstrap1." ;
  let* () =
    Client.transfer
      ~wait:"none"
      ~amount:10
      ~giver:"bootstrap2"
      ~receiver:"bootstrap3"
      client
  in
  Log.info "Transferred 10 tez." ;
  let* () = Client.bake_for ~key:"bootstrap2" client in
  Log.info "Baked block for bootstrap2." ;
  return ()

let register protocol = bake protocol ; transfer protocol
