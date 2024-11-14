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
   Component:    Forging future block
   Invocation:   dune exec tezt/tests/main.exe -- --file forge_block.ml
   Subject:      Test forging a block with a timestamp in the future and verify
                 that it is rejected if it is more than 5 seconds in the future.
*)
let team = Tag.layer1

let test_forge_block () : unit =
  Test.register
    ~__FILE__
    ~title:"Test forge block"
    ~tags:[team; "protocol"; "sandbox"]
  @@ fun () ->
  let protocol_hash = Protocol.demo_counter_hash in
  Log.info "Test setup network" ;
  let* sandbox_node =
    Node.init [Synchronisation_threshold 0; Connections 100]
  in
  Log.info "Test protocol exists" ;
  let sandbox_endpoint = Client.Node sandbox_node in
  let* sandbox_client = Client.init ~endpoint:sandbox_endpoint () in
  let* protocols = Client.Admin.list_protocols sandbox_client in
  Check.(
    list_mem
      string
      ~__LOC__
      Protocol.demo_counter_hash
      protocols
      ~error_msg:"Expected %R to be in the set of known protocols %L") ;
  Log.info "Test activate proto demo time shifted ok" ;
  let parameter_file = Temp.file (protocol_hash ^ "-parameters.json") in
  Base.write_file parameter_file ~contents:"{}" ;
  let delta = Ptime.Span.of_int_s (-1) in
  let* () =
    Client.activate_protocol_and_wait
      ~parameter_file
      ~protocol_hash
      ~fitness:1
      ~key:"activator"
      ~timestamp:(Client.Ago delta)
      sandbox_client
  in
  Log.info "Test activate proto demo time shifted ko" ;
  let parameter_file = Temp.file (protocol_hash ^ "-parameters.json") in
  Base.write_file parameter_file ~contents:"{}" ;
  let delta = Ptime.Span.of_int_s (-30) in
  let* () =
    Client.spawn_activate_protocol
      ~block:"genesis"
      ~parameter_file
      ~protocol_hash
      ~fitness:1
      ~key:"activator"
      ~timestamp:(Client.Ago delta)
      sandbox_client
    |> Process.check_error ~msg:(rex "Block in the future.")
  in
  unit

let register_protocol_independent () = test_forge_block ()
