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

let config_init node args =
  let* () = Node.config_init node args in
  Node.config_show node

let config_reset node args =
  let* () = Node.config_reset node args in
  Node.config_show node

(* Checks that the config have the expected keys *)
let check_config_keys config expected_keys =
  let keys =
    JSON.as_object config |> List.map fst |> List.sort String.compare
  in
  Check.((keys = List.sort String.compare expected_keys) ~__LOC__ (list string))
    ~error_msg:"Config should contain keys %R but contains keys %L."

(* Checks that the p2p config have the expected keys *)
let check_p2p_config config expected_p2p_keys =
  let p2p = JSON.(config |-> "p2p") in
  let p2p_keys =
    JSON.as_object p2p |> List.map fst |> List.sort String.compare
  in
  Check.(
    (p2p_keys = List.sort String.compare expected_p2p_keys)
      ~__LOC__
      (list string))
    ~error_msg:"P2P config should contain keys %R but contains keys %L."

let test_config_reset () =
  let node = Node.create [] in
  let* initial_config = config_init node [] in
  let* reset_config = config_reset node [Metrics_addr ":1234"] in
  (* Checks the consistency of the reset config *)
  check_config_keys reset_config ["data-dir"; "network"; "p2p"; "metrics_addr"] ;
  check_p2p_config
    reset_config
    ["bootstrap-peers"; "expected-proof-of-work"; "listen-addr"] ;
  (* Checks the new value *)
  let metrics_addr =
    JSON.(reset_config |-> "metrics_addr" |=> 0 |> as_string)
  in
  Check.((metrics_addr = ":1234") string)
    ~error_msg:"config.rpc.listen-addrs[0] contains %L but should contain %R." ;
  (* Reset again and and check the equality with initial config *)
  let* final_config = config_reset node [] in
  return
  @@ Check.((JSON.encode initial_config = JSON.encode final_config) string)
       ~error_msg:
         "Configs after reset should be identical. Was %L before, and now %R."

let register () =
  Test.register ~__FILE__ ~title:"config reset" ~tags:["config"; "reset"]
  @@ test_config_reset
