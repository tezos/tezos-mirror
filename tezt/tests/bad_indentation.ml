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
   Components: Michelson
   Invocation: dune exec tezt/tests/main.exe -- --file bad_indentation.ml
   Subject: Tests for the "hash script" and "convert script" commands on
            badly-indented scripts
*)

let script_hash = "exprv8K6ceBpFH5SFjQm4BRYSLJCHQBFeQU6BFTdvQSRPaPkzdLyAL"

let tags = ["client"; "michelson"; "typechecking"]

let test_bad_indentation_ill_typed =
  Protocol.register_test
    ~__FILE__
    ~title:"Bad indentation contract is ill-typed"
    ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script =
    Michelson_script.(find ["ill_typed"; "badly_indented"] protocol |> path)
  in
  let process = Client.spawn_typecheck_script ~scripts:[script] client in
  Process.check_error ~exit_code:1 ~msg:(rex "syntax error in program") process

let test_bad_indentation_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"Bad indentation contract hash is expected"
    ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script =
    Michelson_script.(find ["ill_typed"; "badly_indented"] protocol |> path)
  in
  let* returned_hash = Client.hash_script ~script client in
  Check.(
    (returned_hash = script_hash)
      string
      ~__LOC__
      ~error_msg:"Expected script hash %R, got %L") ;
  unit

let test_formatted_typechecks =
  Protocol.register_test
    ~__FILE__
    ~title:"Formatted bad indentation contract is well-typed"
    ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* formatted_script =
    Client.convert_script
      ~script:
        Michelson_script.(find ["ill_typed"; "badly_indented"] protocol |> path)
      ~src_format:`Michelson
      ~dst_format:`Michelson
      client
  in
  let* () = Client.typecheck_script ~scripts:[formatted_script] client in
  unit

let test_formatted_hash =
  Protocol.register_test
    ~__FILE__
    ~title:"Formatted bad indentation contract hash is expected"
    ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* formatted_script =
    Client.convert_script
      ~script:
        Michelson_script.(find ["ill_typed"; "badly_indented"] protocol |> path)
      ~src_format:`Michelson
      ~dst_format:`Michelson
      client
  in
  let* returned_hash = Client.hash_script ~script:formatted_script client in
  Check.(
    (returned_hash = script_hash)
      string
      ~__LOC__
      ~error_msg:"Expected script hash %R, got %L") ;
  unit

let register ~protocols =
  test_bad_indentation_ill_typed protocols ;
  test_bad_indentation_hash protocols ;
  test_formatted_typechecks protocols ;
  test_formatted_hash protocols
