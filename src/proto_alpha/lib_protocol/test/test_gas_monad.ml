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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe
      -- test "^gas monad$"
    Subject:    Tests for the gas monad module
*)

open Protocol
open Alpha_context
module GM = Gas_monad

let ( let* ) = ( >>=? )

module Gas_monad_syntax = struct
  let ( let* ) = GM.( >>$ )
end

let new_context ~limit =
  Context.init 1 >>=? fun (b, _contracts) ->
  Incremental.begin_construction b >|=? fun inc ->
  let state = Incremental.validation_state inc in
  Gas.set_limit state.ctxt (Saturation_repr.safe_int limit)

let assert_gas_exhaustion ~loc ctxt gas_monad =
  match GM.run ctxt gas_monad with
  | Error _ -> return ()
  | _ -> failwith "%s: expected gas-exhaustion error" loc

let assert_equal_gas ~loc g1 g2 =
  Assert.equal ~loc Gas.Arith.equal "Compare gas" Gas.Arith.pp g1 g2

let assert_inner_errors ~loc ctxt gas_monad ~errors ~remaining_gas =
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
        (Saturation_repr.safe_int remaining_gas)
  | _ -> failwith "%s: expected inner error" loc

let assert_success ~loc ctxt gas_monad ~result ~remaining_gas =
  match GM.run ctxt gas_monad with
  | Ok (Ok x, ctxt) ->
      let* () = Assert.equal_int ~loc x result in
      assert_equal_gas
        ~loc
        (Gas.remaining_operation_gas ctxt)
        (Saturation_repr.safe_int remaining_gas)
  | _ -> failwith "%s: expected successful result `%d' but got error" loc result

(** Test that consuming more gas than remaining results in a gas-exhaustion
    error.  *)
let test_gas_exhaustion () =
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
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
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
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
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
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
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
    let* x = GM.return 1 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 5) in
    let* y = GM.return 2 in
    let* () = GM.consume_gas (Saturation_repr.safe_int 3) in
    GM.return (x + y)
  in
  assert_success ~loc:__LOC__ ctxt gas_monad ~result:3 ~remaining_gas:2

(** Test that an inner error is produced rather than a gas-exhaustion error. *)
let test_inner_error () =
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
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
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad_syntax in
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

(** Test operator [>?$] with successful result. *)
let test_bind_result_ok () =
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad in
    GM.consume_gas (Saturation_repr.safe_int 1) >?$ fun () -> Ok 42
  in
  assert_success ~loc:__LOC__ ctxt gas_monad ~result:42 ~remaining_gas:9

(** Test operator [>?$] with failing result. *)
let test_bind_result_error () =
  let* ctxt = new_context ~limit:10 in
  let gas_monad =
    let open Gas_monad in
    GM.consume_gas (Saturation_repr.safe_int 1) >?$ fun () -> error "Oh no"
  in
  assert_inner_errors
    ~loc:__LOC__
    ctxt
    gas_monad
    ~errors:["Oh no"]
    ~remaining_gas:9

let tests =
  [
    Tztest.tztest "Test exhaustion" `Quick test_gas_exhaustion;
    Tztest.tztest
      "Test exhaustion before error"
      `Quick
      test_gas_exhaustion_before_error;
    Tztest.tztest
      "Test successful result"
      `Quick
      test_successful_with_remaining_gas;
    Tztest.tztest "Test successful result" `Quick test_successful_with_spare_gas;
    Tztest.tztest "Test inner error" `Quick test_inner_error;
    Tztest.tztest "Test unlimited" `Quick test_unlimited;
    Tztest.tztest "Test bind result ok" `Quick test_bind_result_ok;
    Tztest.tztest "Test bind result error" `Quick test_bind_result_error;
  ]
