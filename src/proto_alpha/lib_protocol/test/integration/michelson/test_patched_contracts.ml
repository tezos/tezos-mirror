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

(** Testing
    -------
    Component:    Protocol Migration (patched scripts)
    Invocation:   cd src/proto_alpha/lib_protocol/test/integration/michelson
                  dune exec ./main.exe -- test "^patched contracts$"
    Subject:      Migration
*)

open Tezos_micheline
open Protocol
open Legacy_script_patches_for_J

let script_hash_testable =
  Alcotest.testable Script_expr_hash.pp Script_expr_hash.equal

let patched_contracts =
  [
    exprtgpMFzTtyg1STJqANLQsjsMXmkf8UuJTuczQh8GPtqfw18x6Lc;
    exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw;
    expruqNpURkmjQk5RGHjLrnS1U3DZnEsQCvQQNLSpN1powRmJeQgoJ;
    expruwujdJkc5y4iPzr83Sd3KrJhzxSUb67JdCZmXNKiTTNvEkMrRU;
    expruZKjVy3JbWcJmjtnvMGvRamkDhMgM3urGGdne3pkN9VKgK7VnD;
    exprv98vtze1uwbDXdpb27R8RQabWZMZDXGNAwaAZwCg6WSvXu8fw3;
    expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe;
    exprubv5oQmAUP8BwktmDgMWqTizYDJVhzHhJESGZhJ2GkHESZ1VWg;
  ]

let readable_micheline m =
  let open Micheline in
  map_node (fun _ -> ()) Michelson_v1_primitives.string_of_prim (root m)

let contract_path ?(ext = "patched.tz") hash =
  Filename.concat "patched_contracts"
  @@ Format.asprintf "%a.%s" Script_expr_hash.pp hash ext

let read_file ?ext hash =
  let filename = contract_path ?ext hash in
  Lwt_io.(with_file ~mode:Input filename read)

(* Test that the hashes of the scripts in ./patched_contract/<hash>.original.tz
   match hashes of the contracts being updated by the migration. *)
let test_original_contract {legacy_script_hash; _} () =
  let open Lwt_result_syntax in
  let*! code = read_file ~ext:"original.tz" legacy_script_hash in
  let michelson = Michelson_v1_parser.parse_toplevel ~check:true code in
  let*? prog = Micheline_parser.no_parsing_error michelson in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Alpha_context.Script.expr_encoding
      prog.expanded
  in
  Alcotest.check
    script_hash_testable
    "Expr hash doesn't match"
    legacy_script_hash
    (Script_expr_hash.hash_bytes [bytes]) ;
  return ()

(* Test that the binary-encoded versions of the patched contracts used during the
   migration correspond to the content of the `./patched_contracts/<hash>.tz`
   files *)
let test_patched_contract {legacy_script_hash; patched_code} () =
  let open Lwt_result_syntax in
  let*! expected_michelson = read_file legacy_script_hash in
  let*? program =
    Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_toplevel ~check:true expected_michelson
  in
  match
    Micheline_diff.diff
      ~prev:(readable_micheline patched_code)
      ~current:(readable_micheline program.expanded)
      ()
  with
  | Some diff ->
      let msg =
        Format.asprintf
          "Patched code for %a different than expected!\n%a"
          Script_expr_hash.pp
          legacy_script_hash
          Micheline_printer.print_expr
          diff
      in
      Alcotest.fail msg
  | None -> return ()

(* Test that the diff files `./patched_contracts/<hash>.diff`
   are the results of the `diff` command on the corresponding
   original and patched files *)
let verify_diff {legacy_script_hash; _} () =
  let open Lwt_result_syntax in
  let*! expected_diff = read_file ~ext:"diff" legacy_script_hash in
  let original_code = contract_path ~ext:"original.tz" legacy_script_hash in
  (* The other test asserts that this is indeed the patched code. *)
  let current_code = contract_path ~ext:"patched.tz" legacy_script_hash in
  let diff_cmd =
    ( "",
      [|
        "diff";
        "-u";
        "--label";
        original_code;
        "--label";
        current_code;
        original_code;
        current_code;
      |] )
  in
  let*! actual_diff = Lwt_process.pread diff_cmd in
  Alcotest.(check string) "same diff" expected_diff actual_diff ;
  return ()

let tests =
  List.concat_map
    (fun patch ->
      [
        Tztest.tztest
          (Format.asprintf
             "check original contract hash %a"
             Script_expr_hash.pp
             patch.legacy_script_hash)
          `Quick
          (test_original_contract patch);
        Tztest.tztest
          (Format.asprintf
             "check patched contract %a"
             Script_expr_hash.pp
             patch.legacy_script_hash)
          `Quick
          (test_patched_contract patch);
        Tztest.tztest
          (Format.asprintf
             "verify patch for %a"
             Script_expr_hash.pp
             patch.legacy_script_hash)
          `Quick
          (verify_diff patch);
      ])
    patched_contracts
