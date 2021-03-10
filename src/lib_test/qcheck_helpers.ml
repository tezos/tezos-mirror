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

let qcheck_wrap = List.map QCheck_alcotest.to_alcotest

let qcheck_eq ?pp ?cmp ?eq expected actual =
  let pass =
    match (eq, cmp) with
    | (Some eq, _) ->
        eq expected actual
    | (None, Some cmp) ->
        cmp expected actual = 0
    | (None, None) ->
        Stdlib.compare expected actual = 0
  in
  if pass then true
  else
    match pp with
    | None ->
        QCheck.Test.fail_reportf
          "@[<h 0>Values are not equal, but no pretty printer was provided.@]"
    | Some pp ->
        QCheck.Test.fail_reportf
          "@[<v 2>Equality check failed!@,expected:@,%a@,actual:@,%a@]"
          pp
          expected
          pp
          actual

let qcheck_eq' ?pp ?cmp ?eq ~expected ~actual () =
  qcheck_eq ?pp ?cmp ?eq expected actual

let int64_range a b =
  let int64_range_gen st =
    let range = Int64.sub b a in
    let raw_val = Random.State.int64 st range in
    let res = Int64.add a raw_val in
    assert (a <= res && res <= b) ;
    res
  in
  QCheck.int64 |> QCheck.set_gen int64_range_gen

let rec of_option_gen gen random =
  match gen random with None -> of_option_gen gen random | Some a -> a

let of_option_arb QCheck.{gen; print; small; shrink; collect; stats} =
  let gen = of_option_gen gen in
  let print = Option.map (fun print_opt a -> print_opt (Some a)) print in
  let small = Option.map (fun small_opt a -> small_opt (Some a)) small in
  (* Only shrink if the optional value is non-empty. *)
  let shrink =
    Option.map
      (fun shrink_opt a f -> shrink_opt (Some a) (Option.iter f))
      shrink
  in
  let collect =
    Option.map (fun collect_opt a -> collect_opt (Some a)) collect
  in
  let stats =
    List.map (fun (s, f_opt) -> (s, fun a -> f_opt (Some a))) stats
  in
  QCheck.make ?print ?small ?shrink ?collect ~stats gen
