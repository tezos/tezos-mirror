(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Tezos_benchmark
open Costlang

let fv_const = Free_variable.of_string "const"

let fv_v1 = Free_variable.of_string "v1"

let fv_v2 = Free_variable.of_string "v2"

module Term (X : S) = struct
  open X

  (* Some examples of cost functions *)
  (* [linear size] is a term "const" + "v1" * size  *)
  let linear =
    lam ~name:"size" @@ fun size ->
    free ~name:fv_const + (free ~name:fv_v1 * size)

  let linear_sum =
    lam ~name:"size1" @@ fun size1 ->
    lam ~name:"size2" @@ fun size2 ->
    free ~name:fv_const + (free ~name:fv_v1 * (size1 + size2))

  (* This term, once applied to an argument, is not a linear combination
     with free variables as coefficients (ie cannot be used to perform
     linear regressions)*)
  let not_linear =
    lam ~name:"x" @@ fun x -> free ~name:fv_v1 * free ~name:fv_v2 * x

  let applied_linear = app (app linear_sum (int 10)) (int 33)

  let applied_not_linear = app not_linear (int 10)
end

(* Test pretty-printing *)
module PP = Term (Pp)

let test_pp_1 () = PP.linear = "fun size -> (free(const) + (free(v1) * size))"

let test_pp_2 () =
  PP.linear_sum
  = "fun size1 -> fun size2 -> (free(const) + (free(v1) * (size1 + size2)))"

let test_pp_3 () =
  PP.applied_linear
  = "((fun size1 -> fun size2 -> (free(const) + (free(v1) * (size1 + size2)))) \
     10) 33"

(* Test evaluation *)
let test_eval1 () =
  let module Subst =
    Subst
      (struct
        let subst x =
          match
            List.assoc
              ~equal:Free_variable.equal
              x
              [(fv_v1, 88.); (fv_v2, 4.); (fv_const, -10.)]
          with
          | Some v -> v
          | None ->
              Format.eprintf "failed to get %a@." Free_variable.pp x ;
              raise Not_found
      end)
      (Eval)
  in
  let module Eval = Term (Subst) in
  let res = Subst.prj Eval.applied_linear in
  res = 3774.0

let test_eval2 () =
  let module Subst =
    Subst
      (struct
        let subst x =
          match
            List.assoc
              ~equal:Free_variable.equal
              x
              [(fv_v1, 2.); (fv_v2, 4.); (fv_const, -10.)]
          with
          | Some v -> v
          | None ->
              Format.eprintf "failed to get %a@." Free_variable.pp x ;
              raise Not_found
      end)
      (Eval)
  in
  let module M = Term (Subst) in
  let res = Subst.prj M.applied_not_linear in
  res = 80.0

(* Test evaluation to linear combination *)
module Mset_impl = Term (Eval_to_vector)

let test_eval_to_lincomb () =
  let res =
    Hash_cons_vector.prj
    @@ Eval_to_vector.prj
         Mset_impl.(Eval_to_vector.(applied_linear + applied_linear))
  in
  match Eval_linear_combination_impl.run (fun _ -> None) res with
  | exception _ -> false
  | {linear_comb; const} ->
      Free_variable.Sparse_vec.(
        equal linear_comb (of_list [(fv_v1, 86.); (fv_const, 2.)]))
      && const = 0.0

let test_eval_to_lincomb_fail () =
  let res =
    Hash_cons_vector.prj @@ Eval_to_vector.prj Mset_impl.(applied_not_linear)
  in
  match Eval_linear_combination_impl.run (fun _ -> None) res with
  | exception Eval_linear_combination "*" -> true
  | _ -> false

let tests =
  [
    Test.tztest_assert "pp1" `Quick test_pp_1;
    Test.tztest_assert "pp2" `Quick test_pp_2;
    Test.tztest_assert "pp3" `Quick test_pp_3;
    Test.tztest_assert "eval1" `Quick test_eval1;
    Test.tztest_assert "eval2" `Quick test_eval2;
    Test.tztest_assert "eval_to_linear_comb" `Quick test_eval_to_lincomb;
    Test.tztest_assert
      "eval_to_linear_comb_fail"
      `Quick
      test_eval_to_lincomb_fail;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-benchmark" [("costlang", tests)]
  |> Lwt_main.run
