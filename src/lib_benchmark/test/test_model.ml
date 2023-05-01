(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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
open Model

(* Test of model synthesis *)
(* Synthesize two models of the same type using an arbitrary binary operation *)
let test_synthesize () =
  (* Binary operation *)
  let module TestBinOp : Binary_operation = struct
    module Def (X : Costlang.S) = struct
      let op x y = X.(x + y)
    end
  end in
  (* A model to be synthesized *)
  let x_model : (int * (int * unit)) model =
    let module M = struct
      type arg_type = int * (int * unit)

      let name = Namespace.of_string "x"

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = arity_2

        let model =
          lam ~name:"size1" @@ fun size1 ->
          lam ~name:"size2" @@ fun size2 -> size1 * size2
      end
    end in
    (module M)
  in

  (* The another model to be synthesized *)
  let y_model : (int * (int * unit)) model =
    let module M = struct
      type arg_type = int * (int * unit)

      let name = Namespace.of_string "y"

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = arity_2

        let model =
          lam ~name:"size1" @@ fun size1 ->
          lam ~name:"size2" @@ fun size2 -> max size1 size2
      end
    end in
    (module M)
  in

  (* synthesize two models *)
  let (module Synthesized) =
    synthesize
      ~binop:(module TestBinOp)
      ~name:(Namespace.of_string "x_plus_y")
      ~x_label:"x"
      ~x_model
      ~y_label:"y"
      ~y_model
  in

  let module Pp = Synthesized.Def (Costlang.Pp) in
  let expected =
    "fun size1 -> fun size2 -> "
    ^ "let x = ((fun size1 -> fun size2 -> (size1 * size2)) size1) size2 in "
    ^ "let y = ((fun size1 -> fun size2 -> (max size1 size2)) size1) size2 in "
    ^ "(x + y)"
  in
  Pp.model = expected

let tests = [Test.tztest_assert "synthesize" `Quick test_synthesize]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-benchmark" [("model", tests)]
  |> Lwt_main.run
