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

let config_reset node args =
  let* () = Node.config_reset node args in
  Node.config_show node

let register () =
  Test.register ~__FILE__ ~title:"config reset" ~tags:["config"] @@ fun () ->
  let node = Node.create [] in
  let check_default_config ~__LOC__ c =
    let keys = JSON.as_object c |> List.map fst |> List.sort String.compare in
    Check.((keys = ["data-dir"; "p2p"]) ~__LOC__ (list string))
      ~error_msg:"Config should contain keys %R but contains keys %L." ;
    let p2p = JSON.(c |-> "p2p") in
    let p2p_keys =
      JSON.as_object p2p |> List.map fst |> List.sort String.compare
    in
    Check.(
      (p2p_keys = ["bootstrap-peers"; "listen-addr"]) ~__LOC__ (list string))
      ~error_msg:"P2P config should contain keys %R but contains keys %L." ;
    let addr = JSON.(p2p |-> "listen-addr" |> as_string) in
    Check.((addr = "[::]:9732") ~__LOC__ string)
      ~error_msg:"P2P listening address should be %R but is %L."
  in
  let* c1 = config_reset node [] in
  check_default_config ~__LOC__ c1 ;
  let* c2 = config_reset node ["--rpc-addr=:1234"] in
  let c2_addr = JSON.(c2 |-> "rpc" |-> "listen-addrs" |=> 0 |> as_string) in
  Check.((c2_addr = ":1234") string)
    ~error_msg:"config.rpc.listen-addrs[0] contains %L but should contain %R." ;
  let* c3 = config_reset node [] in
  check_default_config ~__LOC__ c3 ;
  Check.((JSON.encode c1 = JSON.encode c3) string)
    ~error_msg:
      "Configs after reset should be identical. Was %L before, and now %R." ;
  unit
