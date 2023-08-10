(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

open Tezt

(* These tests can be run locally to generate the data needed to run a
   stresstest. *)
module Local = struct
  let generate_baker_accounts () = Test.fail "Not implemented"

  let generate_network_configuration () = Test.fail "Not implemented"

  let generate_manager_operations () = Test.fail "Not implemented"
end

(* These tests must be run remotely by the nodes participating in
   a network that wants to be stresstested. *)
module Remote = struct
  let run_stresstest () = Test.fail "Not implemented"
end

let () =
  let open Tezt.Test in
  register
    ~__FILE__
    ~title:"Generate baker accounts"
    ~tags:["generate_baker_accounts"]
    Local.generate_baker_accounts ;
  register
    ~__FILE__
    ~title:"Generate Network Configuration"
    ~tags:["generate_network_configuration"]
    Local.generate_network_configuration ;
  register
    ~__FILE__
    ~title:"Generate manager operations"
    ~tags:["generate_operations"]
    Local.generate_manager_operations ;
  register
    ~__FILE__
    ~title:"Run stresstest"
    ~tags:["run_stresstest"]
    Remote.run_stresstest ;
  Tezt.Test.run ()
