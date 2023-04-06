(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module String = struct
  include String

  let hash = Hashtbl.hash
end

module Queue = Hash_queue.Make (String) (Int)

let mock_key n = Printf.sprintf "val<%d>" n

let gen_values n =
  let rec gen n acc =
    if n < 0 then acc else gen (n - 1) ((mock_key n, n) :: acc)
  in
  gen (n - 1) []

let add_multiple_values q vs = List.iter (fun (k, v) -> Queue.replace q k v) vs

(* Invariants:
   - (key, value) are ("val<i>", i) for i in [0, n-1]
   - keys are added in increasing order, hence ("val<0>", 0) is always the oldest
     value if `capacity` >= `n`.
   - there is no capacity check. If n > capacity, the oldest values are replaced
*)
let init_queue capacity n =
  let q = Queue.create capacity in
  let vs = gen_values n in
  add_multiple_values q vs ;
  q

let assert_eq_s eq pa pb =
  let open Lwt.Syntax in
  let* a = pa and* b = pb in
  assert (eq a b) ;
  Lwt.return_unit

let test_fold_s () =
  let q = init_queue 10 10 in
  let vs = Lwt.return @@ List.rev @@ gen_values 10 in
  let vs_from_fold =
    (* The resulting list is newest to oldest *)
    Queue.fold_s (fun k v acc -> Lwt.return ((k, v) :: acc)) q []
  in
  assert_eq_s
    (List.equal (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && Int.equal v1 v2))
    vs
    vs_from_fold

let test_fold_es () =
  let q = init_queue 10 10 in
  let vs = Lwt.return_ok @@ List.rev @@ gen_values 10 in
  let vs_from_fold =
    (* The resulting list is newest to oldest *)
    Queue.fold_es (fun k v acc -> Lwt.return_ok ((k, v) :: acc)) q []
  in
  assert_eq_s
    (Result.equal
       ~ok:
         (List.equal (fun (k1, v1) (k2, v2) ->
              String.equal k1 k2 && Int.equal v1 v2))
       ~error:(fun () () -> true))
    vs
    vs_from_fold

let test_fold_es_error () =
  let q = init_queue 10 10 in
  let vs = Lwt.return_error () in
  let vs_from_fold =
    (* The resulting list is newest to oldest *)
    Queue.fold_es (fun _k _v _acc -> Lwt.return_error ()) q []
  in
  assert_eq_s
    (Result.equal
       ~ok:
         (List.equal (fun (k1, v1) (k2, v2) ->
              String.equal k1 k2 && Int.equal v1 v2))
       ~error:(fun () () -> true))
    vs
    vs_from_fold

let () =
  Alcotest_lwt.run
    ~__FILE__
    "stdlib"
    [
      ( "hash_queue_lwt",
        [
          ("fold_s", `Quick, test_fold_s);
          ("fold_es", `Quick, test_fold_es);
          ("fold_es_error", `Quick, test_fold_es_error);
        ] );
    ]
  |> Lwt_main.run
