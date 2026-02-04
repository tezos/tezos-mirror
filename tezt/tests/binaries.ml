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

(* Executables which use the version of octez-evm-node instead of octez-node.
   If one day one adds script-inputs/octez-etherlink-executables or something,
   it would be better to read this list from this file. *)
let evm_executables = ["octez-evm-node"; "etherlink-governance-observer"]

(* TODO: tezos/tezos#4769

   We should be able to get the Version flag string from the Node
   module, e.g. by exporting make_argument. *)
let version_flag = "--version"

let version_rex = rex "^[a-f0-9]+ \\([^()]*\\) \\(.*\\)\n$"

(* TODO: tezos/tezos#4804
   Should we implement this via Component.run commands when possible?
*)
let get_and_check_version executable =
  let* version =
    Process.run_and_read_stdout ("./" ^ Uses.path executable) [version_flag]
  in
  Check.(version =~ version_rex) ~error_msg:"expected version =~ %R, got %L" ;
  return (String.trim version)

let lookup_or_fail path =
  match Uses.lookup path with
  | None ->
      (* We are hoping that [Uses.make] was already called for all executables.
         For instance, since the [Constant] module is linked before this test,
         it should be the case for all paths declared in [Constant]. *)
      failwith
        ("tezt/tests/binaries.ml: lookup_or_fail: executable " ^ path
       ^ " has no corresponding Uses.t. Try to add it to Constant.Unused.")
  | Some uses -> uses

(* We remove octez-node as it will be checked separately. It is the
   binary whose version we assume to be canonical. *)
let read_executable_list path =
  read_file path |> String.split_on_char '\n'
  |> ( List.filter @@ fun str ->
       (not (String.equal str String.empty))
       && not (String.equal str "octez-node") )
  |> List.map lookup_or_fail

(* Test that all released and experimental executables support the --version flag,
   and that they report the same version as the main node of the release
   (Octez node or Octez EVM node). *)
let register_protocol_independent () =
  let released_executables =
    read_executable_list Constant.released_executables
  in
  let experimental_executables =
    read_executable_list Constant.experimental_executables
  in
  let executables = released_executables @ experimental_executables in
  Fun.flip List.iter executables @@ fun executable ->
  let executable_name = Filename.basename (Uses.path executable) in
  let compare_with_executable =
    if List.mem executable_name evm_executables then Constant.octez_evm_node
    else Constant.octez_node
  in
  Test.register
    ~__FILE__
    ~title:(executable_name ^ " --version")
    ~tags:["version"]
    ~uses:[executable; compare_with_executable]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* expected_version = get_and_check_version compare_with_executable in
  Log.info
    "Check that %s supports %s as version flag, and returns version %s."
    executable_name
    version_flag
    expected_version ;
  let* version = get_and_check_version executable in
  let error_msg = executable_name ^ ": expected version %R, got %L" in
  Check.((version = expected_version) ~__LOC__ string ~error_msg) ;
  unit
