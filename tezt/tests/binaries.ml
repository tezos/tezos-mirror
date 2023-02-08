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

(* Testing
   -------
   Component:    Released and Experimental binaries
   Invocation:   dune exec tezt/tests/main.exe -- --file binaries.ml
   Subject:      Perform global coherence tests across released and experimental binaries.
*)

(* TODO: tezos/tezos#4769

   We should be able to get the Version flag string from the Node
   module, e.g. by exporting make_argument. *)
let version_flag = "--version"

(* TODO: tezos/tezos#4804
   Should we implement this via Component.run commands when possible?
*)
let spawn_command path =
  Process.run_and_read_stdout ("./" ^ path) [version_flag]

let test_versions path =
  let node = Node.create [] in
  (* We remove octez-node as it will be checked separately. It is the
     binary whose version we assume to be canonical. *)
  let* node_version = Node.get_version node in
  let commands =
    Base.read_file path |> String.split_on_char '\n'
    |> List.filter @@ fun str ->
       (not (String.equal str String.empty))
       && not (String.equal str "octez-node")
  in
  let loop cmd =
    Log.info
      "Check that %s supports %s as version flag, and returns version %s."
      cmd
      version_flag
      node_version ;
    let* result = spawn_command cmd in
    let error_msg = "%s: expected version %L, got version %R" in
    Check.((node_version = String.trim result) ~__LOC__ string ~error_msg) ;
    unit
  in
  Lwt_list.iter_s loop commands

(* Test that all released binaries support the --version flag, and
   that they report the same version value as the Octez node. *)
let test_released_versions () =
  Test.register
    ~__FILE__
    ~title:"Released binaries: report consistent version"
    ~tags:["binaries"; "released"; "node"; "baker"; "version"]
  @@ fun () -> test_versions Constant.released_executables

(* Test that all experimental binaries support the --version flag, and
   that they report the same version value as the Octez node. *)
let test_experimental_versions () =
  Test.register
    ~__FILE__
    ~title:"Experimental binaries: report consistent version"
    ~tags:["binaries"; "experimental"; "version"]
  @@ fun () -> test_versions Constant.experimental_executables

let register_protocol_independent () =
  test_released_versions () ;
  test_experimental_versions ()
