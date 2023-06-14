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
    Invocation:   dune exec src/proto_018_Proxford/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_patched_contracts.ml
    Subject:      Migration
*)

open Tezos_micheline
open Protocol

module type LEGACY_SCRIPT_PATCHES = sig
  type t

  val script_hash : t -> Script_expr_hash.t

  val code : t -> Michelson_v1_primitives.prim Micheline.canonical

  val patches : t list
end

module type LEGACY_PATCH_TESTS = sig
  type t

  val tests : t -> unit Alcotest_lwt.test_case list
end

let script_hash_testable =
  Alcotest.testable Script_expr_hash.pp Script_expr_hash.equal

(** This functor provides testing for legacy script patches. Patches to
    be tested should be placed in a module conformal to the signature
    [LEGACY_SCRIPT_PATCHES]. It should contain a list of patches and for
    each patch it has to provide a hash of the patched contract and the
    new code (as binary-encoded Micheline).

    Additionally for each patch 3 files need to be placed in
    [patched_contracts] subdirectory:
    * script_hash.original.tz – containing the original version of the
      script;
    * script_hash.patched.tz - containing the patched version;
    * script_hash.diff - containing the diff between the two.

    These files are there so that reviewers of the migration can easily
    see what changes are made to each contract and these tests make sure
    that the patched code supplied in file is identical to the one
    included in the migration; and that the diff is correct. *)
module Legacy_patch_test (Patches : LEGACY_SCRIPT_PATCHES) :
  LEGACY_PATCH_TESTS with type t = Patches.t = struct
  type t = Patches.t

  let readable_micheline m =
    let open Micheline in
    map_node (fun _ -> ()) Michelson_v1_primitives.string_of_prim (root m)

  let path = project_root // Filename.dirname __FILE__

  let contract_path ?(ext = "patched.tz") hash =
    Filename.concat "patched_contracts"
    @@ Format.asprintf "%a.%s" Script_expr_hash.pp hash ext

  let read_file ?ext hash =
    let filename = path // contract_path ?ext hash in
    Lwt_io.(with_file ~mode:Input filename read)

  (* Test that the hashes of the scripts in ./patched_contract/<hash>.original.tz
     match hashes of the contracts being updated by the migration. *)
  let test_original_contract legacy_script_hash () =
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
  let test_patched_contract patch () =
    let open Lwt_result_syntax in
    let*! expected_michelson = read_file @@ Patches.script_hash patch in
    let*? program =
      Micheline_parser.no_parsing_error
      @@ Michelson_v1_parser.parse_toplevel ~check:true expected_michelson
    in
    match
      Micheline_diff.diff
        ~prev:(readable_micheline @@ Patches.code patch)
        ~current:(readable_micheline program.expanded)
        ()
    with
    | Some diff ->
        let msg =
          Format.asprintf
            "Patched code for %a different than expected!\n%a"
            Script_expr_hash.pp
            (Patches.script_hash patch)
            Micheline_printer.print_expr
            diff
        in
        Alcotest.fail msg
    | None -> return ()

  (* Test that the diff files `./patched_contracts/<hash>.diff`
     are the results of the `diff` command on the corresponding
     original and patched files *)
  let verify_diff legacy_script_hash () =
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
    let*! actual_diff = Lwt_process.pread ~cwd:path diff_cmd in
    Alcotest.(check string) "same diff" expected_diff actual_diff ;
    return ()

  let typecheck_patched_script code () =
    let open Lwt_result_syntax in
    (* Number 3 below controls how many accounts should be
       created. This number shouldn't be too small or the context
       won't have enough at least [minimal_stake] tokens. *)
    let* block, _contracts = Context.init3 () in
    let* inc = Incremental.begin_construction block in
    let ctxt = Incremental.alpha_ctxt inc in
    let* _code, _ctxt =
      Lwt.map Environment.wrap_tzresult
      @@ Script_ir_translator.parse_code
           ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
           ~code:(Script_repr.lazy_expr code)
           ctxt
    in
    return ()

  let tests (patch : Patches.t) =
    let script_hash = Patches.script_hash patch in
    [
      Tztest.tztest
        (Format.asprintf
           "check original contract hash %a"
           Script_expr_hash.pp
           script_hash)
        `Quick
        (test_original_contract script_hash);
      Tztest.tztest
        (Format.asprintf
           "check patched contract %a"
           Script_expr_hash.pp
           script_hash)
        `Quick
        (test_patched_contract patch);
      Tztest.tztest
        (Format.asprintf "verify patch for %a" Script_expr_hash.pp script_hash)
        `Quick
        (verify_diff script_hash);
      Tztest.tztest
        (Format.asprintf "type check %a" Script_expr_hash.pp script_hash)
        `Quick
        (typecheck_patched_script @@ Patches.code patch);
    ]
end

(* List modules containing patched scripts here: *)
let test_modules : (module LEGACY_SCRIPT_PATCHES) list =
  [(module Legacy_script_patches)]

let tests =
  List.concat_map
    (fun (module Patches : LEGACY_SCRIPT_PATCHES) ->
      let module Test = Legacy_patch_test (Patches) in
      List.concat_map Test.tests Patches.patches)
    test_modules

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("patched contracts", tests)]
  |> Lwt_main.run
