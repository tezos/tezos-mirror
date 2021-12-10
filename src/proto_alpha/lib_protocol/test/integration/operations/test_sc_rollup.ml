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
    Component:    Rollup layer 1 logic
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- test "^sc rollup$"
    Subject:      Test smart contract rollup
*)

open Protocol
open Alpha_context

exception Sc_rollup_test_error of string

let err x = Exn (Sc_rollup_test_error x)

(** [test_disable_feature_flag ()] tries to originate a smart contract
   rollup with the feature flag is deactivated and checks that it
   fails. *)
let test_disable_feature_flag () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Incremental.begin_construction b >>=? fun i ->
  let kind = Sc_rollup.Kind.Example_arith in
  let boot_sector = Sc_rollup.PVM.boot_sector_of_string "" in
  Op.sc_rollup_origination (I i) contract kind boot_sector >>=? fun op ->
  let expect_failure = function
    | Environment.Ecoproto_error (Apply.Sc_rollup_feature_disabled as e) :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ ->
        failwith
          "It should not be possible to send a smart contract rollup operation \
           when the feature flag is disabled."
  in
  Incremental.add_operation ~expect_failure i op >>= fun _i -> return_unit

(** [test_sc_rollups_all_well_defined] checks that [Sc_rollups.all]
    contains all the constructors of [Sc_rollup.Kind.t] and that
    the [kind_of_string] is consistent with the names declared in
    the PVM implementations. *)
let test_sc_rollups_all_well_defined () =
  let all_contains_all_constructors () =
    let tickets = ref ["Example_arith"] in
    let burn x = tickets := List.filter (( <> ) x) !tickets in
    let pick = function
      | Sc_rollup.Kind.Example_arith -> burn "Example_arith"
    in
    List.iter pick Sc_rollups.all ;
    if !tickets <> [] then
      failwith
        "The following smart-contract rollup kinds should occur in \
         [Sc_rollups.all]: %s\n"
        (String.concat ", " !tickets)
    else return_unit
  in
  let all_names_are_valid () =
    List.iter_es
      (fun k ->
        let (module P : Sc_rollups.PVM.S) = Sc_rollups.of_kind k in
        fail_unless
          (Sc_rollups.kind_of_string P.name = Some k)
          (err (Printf.sprintf "PVM name `%s' is not a valid kind name" P.name)))
      Sc_rollups.all
  in
  all_contains_all_constructors () >>=? fun () -> all_names_are_valid ()

let tests =
  [
    Tztest.tztest
      "check effect of disabled feature flag"
      `Quick
      test_disable_feature_flag;
    Tztest.tztest
      "check that all rollup kinds are correctly enumerated"
      `Quick
      test_sc_rollups_all_well_defined;
  ]
