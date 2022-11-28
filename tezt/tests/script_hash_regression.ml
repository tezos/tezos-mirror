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

(* Testing
   -------
   Component: Script hash regression tests
   Invocation: dune exec tezt/tests/main.exe -- -f script_hash_regression.ml
   Subject: Regression tests of Michelson script hashing
*)

let test_script_hash_regression =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test script hash regression"
    ~tags:["script"; "michelson"; "hash"]
  @@ fun protocol ->
  let pytest_script_dir =
    sf
      "tests_python/contracts_%s"
      (match protocol with
      | Protocol.Alpha -> "alpha"
      | _ -> sf "%03d" @@ Protocol.number protocol)
  in
  let scrub_script_dirs output =
    replace_string (rex pytest_script_dir) ~by:"[CONTRACT_PATH]" output
  in
  let hooks =
    let Process_hooks.{on_spawn = _; on_log} = Tezos_regression.hooks in
    (* We don't actually care about the location of the scripts *)
    let on_log output = on_log (scrub_script_dirs output) in
    (* We don't care about the actual command invoked, only script hashes. *)
    let on_spawn _cmd _args = () in
    Process_hooks.{on_spawn; on_log}
  in
  let all_pytest_scripts =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4008

       Once the location of scripts is decided upon, then we should
       only fetch script from that location. We'll notice when that
       happens, becase this test will fail when
       [tests_python/contracts_NNN/] no longer exists. *)
    let sub_dirs =
      [
        "attic";
        "entrypoints";
        "opcodes";
        "macros";
        "mini_scenarios";
        "non_regression";
        "ill_typed";
        "legacy";
      ]
    in
    List.concat_map
      (fun dir ->
        Sys.readdir (pytest_script_dir // dir)
        |> Array.to_list
        |> List.map (fun script -> pytest_script_dir // dir // script))
      sub_dirs
  in
  (* Sort scripts for more legible output *)
  let scripts = all_pytest_scripts |> List.sort String.compare in
  let* client = Client.init_mockup ~protocol () in
  let* (_hashes : string list) =
    Client.hash_scripts ~hooks ~display_names:true scripts client
  in
  unit

let register ~protocols = test_script_hash_regression protocols
