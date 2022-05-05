(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe  <gb.fefe@protonmail.com>                    *)
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
   Component:    Protocol demo counter
   Invocation:   dune exec tezt/tests/main.exe -- --file demo_counter.ml
   Subject:      Minimal test for the protocol demo counter

 *)

let check_a ?__LOC__ client expected =
  let* a = Demo_client.get_a client in
  Check.((a = expected) ?__LOC__ int)
    ~error_msg:"expected amount for a = %R, got %L " ;
  unit

let check_b ?__LOC__ client expected =
  let* b = Demo_client.get_b client in
  Check.((b = expected) ?__LOC__ int)
    ~error_msg:"expected amount for b = %R, got %L" ;
  unit

let register () =
  Test.register ~__FILE__ ~title:(sf "demo_counter") ~tags:["demo_counter"]
  @@ fun () ->
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Demo_client.activate client in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 0 in
  let* () = check_b ~__LOC__ client 0 in
  let* () = Demo_client.increment_a client in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 1 in
  let* () = check_b ~__LOC__ client 0 in
  let* () = Demo_client.increment_a client in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 2 in
  let* () = check_b ~__LOC__ client 0 in
  let* () = Demo_client.increment_b client in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 2 in
  let* () = check_b ~__LOC__ client 1 in
  let* () = Demo_client.transfer client 2 in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 0 in
  let* () = check_b ~__LOC__ client 3 in
  let* () = Demo_client.transfer client ~-1 in
  let* () = Demo_client.bake client in
  let* () = check_a ~__LOC__ client 1 in
  let* () = check_b ~__LOC__ client 2 in
  return ()
