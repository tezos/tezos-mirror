(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(* Testing
   -------
   Component:    Base
   Invocation:   dune exec src/lib_base/test/test_bounded.exe
   Subject:      Test the [Bounded] module.
*)

module Make_test_bounded_int32 (B : Bounded.Int32.BOUNDS) = struct
  include Bounded.Int32.Make (B)

  let gen : t QCheck2.Gen.t =
    let open QCheck2.Gen in
    let* n = QCheck2.Gen.int32 in
    let size =
      Tezos_stdlib.Compare.Int32.max
        1l
        (Int32.add (Int32.sub max_int min_int) 1l)
    in
    let in_bounds = Int32.add min_int (Int32.unsigned_rem n size) in
    match of_int32 in_bounds with
    | None -> failwith "Out of bounds"
    | Some x -> return x

  let print (x : t) : string = Int32.to_string (to_int32 x)

  let roundtrips_json : QCheck2.Test.t =
    QCheck2.Test.make
      ~name:"Bounded.Int32 roundtrips in JSON"
      ~print
      gen
      (fun t ->
        let b = Data_encoding.Json.construct encoding t in
        let tt = Data_encoding.Json.destruct encoding b in
        t = tt)

  let roundtrips_binary : QCheck2.Test.t =
    QCheck2.Test.make
      ~name:"Bounded.Int32 roundtrips in binary"
      ~print
      gen
      (fun t ->
        let b = Data_encoding.Binary.to_bytes_exn encoding t in
        let tt = Data_encoding.Binary.of_bytes_exn encoding b in
        t = tt)

  let tests = [roundtrips_binary; roundtrips_json]
end

module Empty = Make_test_bounded_int32 (struct
  let min_int = 1l

  let max_int = 0l
end)

module Small = Make_test_bounded_int32 (struct
  let min_int = 1l

  let max_int = 3l
end)

module Small_with_neg = Make_test_bounded_int32 (struct
  let min_int = -10l

  let max_int = 10l
end)

module Full = Make_test_bounded_int32 (Int32)

let int32_checks =
  let open Alcotest in
  [
    test_case "0 not in empty" `Quick (fun () ->
        assert (Empty.of_int32 0l = None));
    test_case "123 not in empty" `Quick (fun () ->
        assert (Empty.of_int32 123l = None));
    test_case "Int32.min_int not in empty" `Quick (fun () ->
        assert (Empty.of_int32 Int32.min_int = None));
    test_case "0 not in Small" `Quick (fun () ->
        assert (Small.of_int32 0l = None));
    test_case "1 in Small" `Quick (fun () ->
        assert (Option.map Small.to_int32 (Small.of_int32 1l) = Some 1l));
    test_case "2 in Small" `Quick (fun () ->
        assert (Option.map Small.to_int32 (Small.of_int32 2l) = Some 2l));
    test_case "4 not in Small" `Quick (fun () ->
        assert (Small.of_int32 4l = None));
    test_case "0 in full" `Quick (fun () ->
        assert (Option.map Full.to_int32 (Full.of_int32 0l) = Some 0l));
    test_case "123 in full" `Quick (fun () ->
        assert (Option.map Full.to_int32 (Full.of_int32 123l) = Some 123l));
    test_case "Int32.min_int in full" `Quick (fun () ->
        assert (
          Option.map Full.to_int32 (Full.of_int32 Int32.min_int)
          = Some Int32.min_int));
    test_case "Int32.max_int in full" `Quick (fun () ->
        assert (
          Option.map Full.to_int32 (Full.of_int32 Int32.max_int)
          = Some Int32.max_int));
  ]

let () =
  Alcotest.run
    "Bounded"
    [
      ("Int32", int32_checks);
      ("Int32 Small", Lib_test.Qcheck2_helpers.qcheck_wrap Small.tests);
      ( "Int32 Small_with_neg",
        Lib_test.Qcheck2_helpers.qcheck_wrap Small_with_neg.tests );
      ("Int32 Full", Lib_test.Qcheck2_helpers.qcheck_wrap Full.tests);
    ]
