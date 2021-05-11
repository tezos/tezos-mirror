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

(* This module runs remote tests as specified in the config file. Each
   module in this repository defines a specific test suite.  A config file
   where you indicate the [node_path] and the [address] of the remote node
   must be provided. This configuration is in [/tmp/runner_config.json].
   You can modify the location by giving the new path in the environment
   variable [TEZT_RUNNER_CONFIG].

   The JSON template is :
   {
     "address" : "ip address of the remote machine",
     "node_path" : "path to the node executable on the remote machine",
     "ssh_alias" : "alias in your ssh config", // Optional
     "ssh_user" : "the ssh user", // Optional
     "ssh_port" : "the ssh port", // Optional
     "ssh_id" : "the ssh identity file" // Optional
   }
*)
let () =
  Basic.register ~protocols:[Alpha] ;
  Double_bake.register ~protocols:[Alpha] ;
  Test.run ()
