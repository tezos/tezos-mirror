(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

open Protocol
open Alpha_context

(** Testing
    -------
    Component:    Alpha_context 
    Invocation:   dune exec ./src/proto_alpha/lib_protocol/test/unit/main.exe -- test Alpha_context 
    Dependencies: helpers/block.ml
    Subject:      To test the modules (including the top-level)
                  in alpha_context.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)

(** Creates an Alpha_context without creating a full-fledged block *)
let create () =
  let accounts = Account.generate_accounts 1 in
  Block.alpha_context accounts

module Test_Script = struct
  (** Force serialise of lazy [Big_map.t] in a given [alpha_context] *)
  let test_force_bytes_in_context () =
    create () >>=? fun alpha_context ->
    let mbytes_pp ppf t =
      Format.pp_print_string ppf (Environment.Bytes.to_string t)
    in
    let open Alpha_context.Script in
    Environment.wrap_tzresult
    @@ force_bytes_in_context alpha_context
    @@ lazy_expr @@ Micheline.strip_locations
    @@ Prim (0, D_Unit, [], [])
    >>?= fun (bytes, _) ->
    Assert.equal
      ~loc:__LOC__
      Environment.Bytes.equal
      "script serialised incorrectly"
      mbytes_pp
      bytes
      (`Hex "030b" |> Hex.to_bytes_exn)
end

module Test_Big_map = struct
  (** Test failure path: look for a non-existent key in a [Big_map] *)
  let test_mem () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.mem
        alpha_context
        big_map_id
        (Script_expr_hash.hash_string ["0"; "0"])
      >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, is_member) ->
    Assert.equal_bool ~loc:__LOC__ is_member false

  (** Test failure code path of [get_opt] by looking for missing key in a [Big_map.t] *)
  let test_get_opt () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.get_opt
        alpha_context
        big_map_id
        (Script_expr_hash.hash_string ["0"; "0"])
      >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "get_opt should have failed looking for a non-existent key"
    | None -> return_unit

  (** Test existence of a non-existent [Big_map] in an [Alpha_context.t] *)
  let test_exists () =
    ( create () >>=? fun alpha_context ->
      Big_map.fresh ~temporary:true alpha_context >|= Environment.wrap_tzresult
      >>=? fun (alpha_context, big_map_id) ->
      Big_map.exists alpha_context big_map_id >|= Environment.wrap_tzresult )
    >>=? fun (_alpha_context, value) ->
    match value with
    | Some _ ->
        failwith "exists should have failed looking for a non-existent big_map"
    | None -> return_unit
end

let tests =
  [
    Tztest.tztest
      "Script.force_bytes_in_context: checks if it serialises a simple \
       michelson expression"
      `Quick
      Test_Script.test_force_bytes_in_context;
    Tztest.tztest
      "Big_map.mem: failure case - must return false when starting with an \
       empty map"
      `Quick
      Test_Big_map.test_mem;
    Tztest.tztest
      "Big_map.get_opt: failure case - looking up key that doesn't exist"
      `Quick
      Test_Big_map.test_get_opt;
    Tztest.tztest
      "Big_map.exists: failure case - looking up big_map that doesn't exist"
      `Quick
      Test_Big_map.test_exists;
  ]
