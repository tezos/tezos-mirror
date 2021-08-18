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

let cap = 100

let check_connections_below_cap () =
  Test.register
    ~__FILE__
    ~title:"CLI under connections cap"
    ~tags:["cli"; "connections"]
  @@ fun () ->
  let* _node = Node.init [Connections cap] in
  unit

let on_terminate resolver status =
  match status with
  | Unix.WEXITED x when x = 1 -> Lwt.wakeup resolver ()
  | _ -> ()

let check_connections_above_cap () =
  Test.register
    ~__FILE__
    ~title:"CLI above connections cap"
    ~tags:["cli"; "connections"; "bad"]
  @@ fun () ->
  let (has_failed, on_failure) = Lwt.task () in
  let node = Node.create [] in
  let* _node =
    Node.run
      ~on_terminate:(on_terminate on_failure)
      node
      [Connections (cap * 2)]
  in
  let is_ready_p =
    let* () = Node.wait_for_ready node in
    Lwt.return_true
  in
  let has_failed_p =
    let* () = has_failed in
    Lwt.return_false
  in
  let* is_ready = Lwt.pick [is_ready_p; has_failed_p] in
  if is_ready then Test.fail "The node should fail and should not be ready"
  else unit

let register_protocol_independent () =
  check_connections_below_cap () ;
  check_connections_above_cap ()
