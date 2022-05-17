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

open Support.Lib
open Test_fuzzing_helpers

module type TIER = sig
  val suffix : string

  type t

  val of_seq : int Seq.t -> t

  val iter : (int -> unit) -> t -> (unit, int) result Lwt.t

  val iter_s : (int -> unit Lwt.t) -> t -> (unit, int) result Lwt.t

  val iter_e : (int -> (unit, int) result) -> t -> (unit, int) result Lwt.t

  val iter_es :
    (int -> (unit, int) result Lwt.t) -> t -> (unit, int) result Lwt.t
end

module TieredSeq : TIER with type t = int Seq.t = struct
  let suffix = "XXX"

  include Seq

  type nonrec t = int t

  let of_seq s = s

  open Monad

  let iter f s =
    iter f s ;
    Lwt_result_syntax.return_unit

  let iter_s f s = Lwt_result.ok @@ iter_s f s

  let iter_e f s = Lwt.return @@ iter_e f s

  let iter_es f s = iter_es f s
end

module TestIter (Tier : TIER) = struct
  open QCheck2
  open Monad

  let test_iter =
    Test.make
      ~name:(Format.asprintf "Seq{,_%s}.iter" Tier.suffix)
      (Gen.triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let* () = TieredSeq.iter (IterOf.fn acc fn) (List.to_seq input) in
           return !acc)
          (let acc = ref init in
           let* () =
             Tier.iter (IterOf.fn acc fn) (Tier.of_seq @@ List.to_seq input)
           in
           return !acc))

  let test_iter_e =
    Test.make
      ~name:(Format.asprintf "Seq{,%s}.iter_e" Tier.suffix)
      (Gen.triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let* () = TieredSeq.iter_e (IterEOf.fn acc fn) (List.to_seq input) in
           return !acc)
          (let acc = ref init in
           let* () =
             Tier.iter_e (IterEOf.fn acc fn) (Tier.of_seq @@ List.to_seq input)
           in
           return !acc))

  let test_iter_s =
    Test.make
      ~name:(Format.asprintf "Seq{,%s}.iter_s" Tier.suffix)
      (Gen.triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let* () = TieredSeq.iter_s (IterSOf.fn acc fn) (List.to_seq input) in
           return !acc)
          (let acc = ref init in
           let* () =
             Tier.iter_s (IterSOf.fn acc fn) (Tier.of_seq @@ List.to_seq input)
           in
           return !acc))

  let test_iter_es =
    Test.make
      ~name:(Format.asprintf "Seq{,%s}.iter_es" Tier.suffix)
      (Gen.triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let* () =
             TieredSeq.iter_es (IterESOf.fn acc fn) (List.to_seq input)
           in
           return !acc)
          (let acc = ref init in
           let* () =
             Tier.iter_es (IterESOf.fn acc fn) (Tier.of_seq @@ List.to_seq input)
           in
           return !acc))

  let tests = [test_iter; test_iter_e; test_iter_s; test_iter_es]
end

module TieredSeq_s : TIER with type t = int Seq_s.t = struct
  let suffix = "s"

  include Seq_s

  type nonrec t = int t

  let iter f s = Lwt_result.ok @@ iter f s

  let iter_s f s = Lwt_result.ok @@ iter_s f s
end

module TestedSeq_s = TestIter (TieredSeq_s)

module TieredSeq_e : TIER with type t = (int, int) Seq_e.t = struct
  let suffix = "e"

  include Seq_e

  type nonrec t = (int, int) t

  let iter f s = Lwt.return @@ iter f s

  let iter_e f s = Lwt.return @@ iter_e f s
end

module TestedSeq_e = TestIter (TieredSeq_e)

module TieredSeq_es : TIER with type t = (int, int) Seq_es.t = struct
  let suffix = "es"

  include Seq_es

  type nonrec t = (int, int) t
end

module TestedSeq_es = TestIter (TieredSeq_es)

(* testing iter_ep is equivalent in two separate tiers
   NOTE: only for [Seq_s] *)
let iter_ep =
  let open QCheck2 in
  Test.make
    ~name:(Format.asprintf "Seq{,_s}.iter_ep")
    (Gen.quad Test_fuzzing_helpers.Fn.arith one one many)
    (fun (Fun (_, fn), const, init, input) ->
      let open Monad.Lwt_result_syntax in
      eq_es
        (let acc = ref init in
         let* () =
           Seq.iter_ep (IterESOf.monotonous acc fn const) (List.to_seq input)
         in
         return !acc)
        (let acc = ref init in
         let* () =
           Seq_s.iter_ep
             (IterESOf.monotonous acc fn const)
             (Seq_s.of_seq @@ List.to_seq input)
         in
         return !acc))

let iter_p =
  let open QCheck2 in
  Test.make
    ~name:(Format.asprintf "Seq{,_s}.iter_p")
    (Gen.quad Test_fuzzing_helpers.Fn.arith one one many)
    (fun (Fun (_, fn), const, init, input) ->
      let open Monad.Lwt_syntax in
      eq_es
        (let acc = ref init in
         let* () =
           Seq.iter_p (IterSOf.monotonous acc fn const) (List.to_seq input)
         in
         return_ok !acc)
        (let acc = ref init in
         let* () =
           Seq_s.iter_p
             (IterSOf.monotonous acc fn const)
             (Seq_s.of_seq @@ List.to_seq input)
         in
         return_ok !acc))

let wrap (name, (module Tier : TIER)) =
  let module M = TestIter (Tier) in
  (name, Lib_test.Qcheck_helpers.qcheck_wrap M.tests)

let () =
  let name = "Test_fuzzing_seq_tiered" in
  let tests =
    [
      ("TestedSeq_s", (module TieredSeq_s : TIER));
      ("TestedSeq_e", (module TieredSeq_e : TIER));
      ("TestedSeq_es", (module TieredSeq_es : TIER));
    ]
  in
  let tests = List.map wrap tests in
  let tests =
    tests
    @ [
        ("iter_p", Lib_test.Qcheck_helpers.qcheck_wrap [iter_p]);
        ("iter_ep", Lib_test.Qcheck_helpers.qcheck_wrap [iter_ep]);
      ]
  in
  Alcotest.run name tests
