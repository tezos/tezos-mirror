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
    Component:  Protocol (baking)
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/main.exe -- test "^baking module$"
    Subject:    some functions in the Baking module
*)

open Protocol
open Alpha_context

let minimal_time ~bd ~dp ~md ~p = if p = 0 then md else bd + (p * dp)

let emmystar_delay ~te ~ie ~md ~bd ~dp ~de ~p ~e =
  if p = 0 && e >= 3 * te / 5 then md else bd + (p * dp) + (de * max 0 (ie - e))

(* We test with three sets of constants:
   - the test constants, which are the default ones in this test framework
   - the mainnet constants
   - the sandbox constants
   For the last two, the values are picked manually, which means these
   values may get out of sync with the real values.
   (Actually, there are currently only two sets of relevant constants, as
   the values of the relevant test and sandbox constants coincide.) *)
let test_minimal_time () =
  Context.init 1 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun constants ->
  let bd =
    Stdlib.List.hd constants.parametric.time_between_blocks
    |> Period.to_seconds |> Int64.to_int
  in
  let dp =
    Stdlib.List.nth constants.parametric.time_between_blocks 1
    |> Period.to_seconds |> Int64.to_int
  in
  let md =
    constants.parametric.minimal_block_delay |> Period.to_seconds
    |> Int64.to_int
  in
  let prio_range = [0; 1; 2; 3] in
  let bd_dp_range = [(bd, dp); (60, 40)] in
  let md_range = [md; 30] in
  let range =
    List.product prio_range (Stdlib.List.combine bd_dp_range md_range)
  in
  List.iter_es
    (fun (priority, ((bd, dp), md)) ->
      Context.init
        ~time_between_blocks:
          [
            Period.of_seconds_exn (Int64.of_int bd);
            Period.of_seconds_exn (Int64.of_int dp);
          ]
        ~minimal_block_delay:(Period.of_seconds_exn (Int64.of_int md))
        1
      >>=? fun (b, _) ->
      Context.get_constants (B b) >>=? fun constants ->
      Baking.minimal_time
        constants.parametric
        ~priority
        b.header.shell.timestamp
      |> Environment.wrap_tzresult
      >>?= fun ts ->
      let expected_ts =
        Time.Protocol.add
          b.header.shell.timestamp
          (Int64.of_int (minimal_time ~bd ~dp ~md ~p:priority))
      in
      Assert.equal_int64
        ~loc:__LOC__
        (Time.Protocol.to_seconds ts)
        (Time.Protocol.to_seconds expected_ts))
    range

(* Same comment as for the previous test. *)
let test_minimal_valid_time () =
  Context.init 1 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun constants ->
  let md =
    constants.parametric.minimal_block_delay |> Period.to_seconds
    |> Int64.to_int
  in
  let bd =
    Stdlib.List.nth constants.parametric.time_between_blocks 0
    |> Period.to_seconds |> Int64.to_int
  in
  let dp =
    Stdlib.List.nth constants.parametric.time_between_blocks 1
    |> Period.to_seconds |> Int64.to_int
  in
  let de =
    constants.parametric.delay_per_missing_endorsement |> Period.to_seconds
    |> Int64.to_int
  in
  let te_range = [constants.parametric.endorsers_per_block] in
  let ie_range = [constants.parametric.initial_endorsers; 192] in
  let md_range = [md; 30] in
  let bd_dp_range = [(bd, dp); (60, 40)] in
  let de_range = [de; 8] in
  let p_range = 0 -- 2 in
  let e_range = 0 -- constants.parametric.endorsers_per_block in
  let range =
    List.product
      p_range
      (List.product
         e_range
         (List.product
            te_range
            (Stdlib.List.combine
               ie_range
               (Stdlib.List.combine
                  md_range
                  (Stdlib.List.combine bd_dp_range de_range)))))
  in
  List.iter_es
    (fun (p, (e, (te, (ie, (md, ((bd, dp), de)))))) ->
      Context.init
        ~endorsers_per_block:te
        ~time_between_blocks:
          [
            Period.of_seconds_exn (Int64.of_int bd);
            Period.of_seconds_exn (Int64.of_int dp);
          ]
        ~minimal_block_delay:(Period.of_seconds_exn (Int64.of_int md))
        ~initial_endorsers:ie
        ~delay_per_missing_endorsement:(Period.of_seconds_exn (Int64.of_int de))
        1
      >>=? fun (b, _) ->
      Context.get_constants (B b) >>=? fun constants ->
      Baking.minimal_valid_time
        constants.parametric
        ~priority:p
        ~endorsing_power:e
        ~predecessor_timestamp:b.header.shell.timestamp
      |> Environment.wrap_tzresult
      >>?= fun timestamp ->
      let delay = emmystar_delay ~te ~ie ~md ~bd ~dp ~de ~p ~e in
      let expected_timestamp =
        Time.Protocol.add b.header.shell.timestamp (Int64.of_int delay)
      in
      Assert.equal_int64
        ~loc:__LOC__
        (Time.Protocol.to_seconds timestamp)
        (Time.Protocol.to_seconds expected_timestamp))
    range

let tests =
  [
    Tztest.tztest "minimal time" `Quick test_minimal_time;
    Tztest.tztest "minimal valid time" `Quick test_minimal_valid_time;
  ]
