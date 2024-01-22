(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
    Component:  Protocol Gas_monad
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_gas_monad.ml
    Subject:    Tests for the gas monad module
*)

open Protocol
open Alpha_context
module GM = Gas_monad

let ten_milligas = Gas.fp_of_milligas_int 10

let new_context ~limit =
  let open Lwt_result_syntax in
  let* b, _contract = Context.init1 () in
  let+ inc = Incremental.begin_construction b in
  Gas.set_limit (Incremental.alpha_ctxt inc) limit

let assert_gas_exhaustion ~loc ctxt gas_monad =
  match GM.run ctxt gas_monad with
  | Error _ -> return_unit
  | _ -> failwith "%s: expected gas-exhaustion error" loc

let assert_equal_gas ~loc g1 g2 =
  Assert.equal ~loc Gas.Arith.equal "Compare gas" Gas.Arith.pp g1 g2

let assert_inner_errors ~loc ctxt gas_monad ~errors ~remaining_gas =
  let open Lwt_result_syntax in
  match GM.run ctxt gas_monad with
  | Ok (Error e, ctxt) ->
      let* () =
        Assert.assert_equal_list
          ~loc
          ( = )
          "Inner error"
          Format.pp_print_string
          e
          errors
      in
      assert_equal_gas
        ~loc
        (Gas.remaining_operation_gas ctxt)
        (Gas.fp_of_milligas_int remaining_gas)
  | _ -> failwith "%s: expected inner error" loc

let assert_success ~loc ctxt gas_monad ~result ~remaining_gas =
  match GM.run ctxt gas_monad with
  | Ok (Ok x, ctxt) ->
      let open Lwt_result_syntax in
      let* () = Assert.equal_int ~loc x result in
      assert_equal_gas
        ~loc
        (Gas.remaining_operation_gas ctxt)
        (Gas.fp_of_milligas_int remaining_gas)
  | _ -> failwith "%s: expected successful result `%d' but got error" loc result

let with_context f ~limit =
  let open Lwt_result_syntax in
  let* ctxt = new_context ~limit in
  f ctxt

(** Test that consuming more gas than remaining results in a gas-exhaustion
    error.  *)
let test_gas_exhaustion () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 10) in
    let* y = GM.return 2 in
    GM.return (x + y)
  in
  assert_gas_exhaustion ~loc:__LOC__ ctxt gas_monad

(** Test that consuming more gas than remaining results in a gas-exhaustion
    error before an inner error is produced. *)
let test_gas_exhaustion_before_error () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 10) in
    let* () = GM.of_result (error "Oh no") in
    let* y = GM.return 2 in
    GM.return (x + y)
  in
  assert_gas_exhaustion ~loc:__LOC__ ctxt gas_monad

(** Test that consuming all remaining gas is feasible. *)
let test_successful_with_remaining_gas () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* y = GM.return 2 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    GM.return (x + y)
  in
  assert_success ~loc:__LOC__ ctxt gas_monad ~result:3 ~remaining_gas:0

(** Test that the context has the expected amount of spare gas after the
    computation. *)
let test_successful_with_spare_gas () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* y = GM.return 2 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 3) in
    GM.return (x + y)
  in
  assert_success ~loc:__LOC__ ctxt gas_monad ~result:3 ~remaining_gas:2

(** Test that an inner error is produced rather than a gas-exhaustion error. *)
let test_inner_error () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* () = GM.of_result (error "Oh no") in
    let* y = GM.return 2 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 10) in
    GM.return (x + y)
  in
  assert_inner_errors
    ~loc:__LOC__
    ctxt
    gas_monad
    ~errors:["Oh no"]
    ~remaining_gas:5

(* Test that no gas-exhaustion error is produced and that no gas is consumed
   when run in unlimited mode.
*)
let test_unlimited () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* y = GM.return 2 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 100) in
    let* () = GM.consume_gas (Saturation_repr.safe_int 3) in
    GM.return (x + y)
  in
  assert_success
    ~loc:__LOC__
    (Gas.set_unlimited ctxt)
    gas_monad
    ~result:3
    ~remaining_gas:10

let test_syntax_module () =
  with_context ~limit:ten_milligas @@ fun ctxt ->
  let gas_monad =
    let open Gas_monad.Syntax in
    let* none = return_none in
    let* nil = return_nil in
    let* t = return_true in
    let* f = return_false in
    let*? one = Ok 1 in
    let+ two = return 2 in
    (none, nil, t, f, one, two)
  in
  match GM.run ctxt gas_monad with
  | Ok (Ok (None, [], true, false, 1, 2), _ctxt) -> return_unit
  | _ -> failwith "Expected `Ok (None, [], true, false, 1, 2)`"

let tests =
  [
    Tztest.tztest "exhaustion" `Quick test_gas_exhaustion;
    Tztest.tztest
      "exhaustion before error"
      `Quick
      test_gas_exhaustion_before_error;
    Tztest.tztest
      "successful result with remaining gas"
      `Quick
      test_successful_with_remaining_gas;
    Tztest.tztest
      "successful result with spare gas"
      `Quick
      test_successful_with_spare_gas;
    Tztest.tztest "inner error" `Quick test_inner_error;
    Tztest.tztest "unlimited" `Quick test_unlimited;
    Tztest.tztest "syntax module" `Quick test_syntax_module;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("gas monad", tests)]
  |> Lwt_main.run
