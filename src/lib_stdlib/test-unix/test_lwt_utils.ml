(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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
    _______

    Invocation: dune exec src/lib_stdlib/test-unix/main.exe \
                  -- --file test_lwt_utils.ml
 *)

module Assert = Assert

let rec n_pauses n =
  if n <= 0 then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    n_pauses (n - 1)

(* Check that the **first** successful promise would be picked among a sequence
   of already successful promises. *)
let test_already_successful () =
  let ps = List.map Lwt.return (range 0 3) in
  let* result = Lwt_utils.pick_successful ps in
  Assert.equal ~loc:__LOC__ result 0 ;
  Lwt.return_unit

(* Check that [pick_successful] is rejected when given a sequence of already
   rejected promises. *)
let test_already_rejected () =
  let failed_promises =
    List.map (fun _ -> Lwt.fail_with "I am a failed promise.") (range 0 3)
  in
  let result = Lwt_utils.pick_successful failed_promises in
  match Lwt.state result with
  | Lwt.Fail (Lwt_utils.None_successful _) -> Lwt.return_unit
  | Lwt.Fail exn ->
      Assert.fail
        ~loc:__LOC__
        (fun pp s -> Format.fprintf pp "%s" s)
        "Tezos_stdlib__Lwt_utils.None_successful"
        (Printexc.exn_slot_name exn)
  | _ ->
      Assert.fail_msg
        ~loc:__LOC__
        "Expected parent promise to fail with None_successful exception."

(* Check that the **first** successful promise would be picked among a sequence
   of pending promises. *)
let test_first_successful () =
  let ps =
    List.map
      (fun n ->
        let* () = n_pauses n in
        Lwt.return n)
      (List.rev (List.init 6 (fun i -> (i + 1) * 2)))
  in
  let* result = Lwt_utils.pick_successful ps in
  Assert.equal ~loc:__LOC__ result 2 ;
  Lwt.return_unit

(* Check that the **first** successful promise would be picked among a sequence
   of pending promises and the remaining would be cancelled if pending. *)
let test_first_successful_remaining_canceled () =
  let length = 6 in
  let promise_to_be_woken = 4 in
  let ns = range 0 length in
  let ps, rs = List.split (List.map (fun _ -> Lwt.task ()) ns) in
  let all_pending_should_be_pending =
    List.for_all (fun p -> Lwt.is_sleeping p) ps
  in
  Assert.assert_true __LOC__ all_pending_should_be_pending ;
  let _ = Lwt_utils.pick_successful ps in
  let r = List.nth rs promise_to_be_woken in
  Lwt.wakeup r () ;
  let ps = List.filteri (fun i _ -> i <> promise_to_be_woken) ps in
  let remaining_should_be_canceled =
    List.for_all (fun p -> Lwt.state p = Lwt.Fail Lwt.Canceled) ps
  in
  Assert.assert_true __LOC__ remaining_should_be_canceled ;
  Lwt.return_unit

(* Check that a valid result is returned when multiple promises are successful at
   the same time (race condition). *)
let test_multi_successful () =
  let successful_ns = range 1 4 in
  let successful_ps =
    List.map
      (fun i ->
        let* () = n_pauses 4 in
        Lwt.return i)
      successful_ns
  in
  let failure_ps =
    List.map
      (fun _ ->
        let* () = n_pauses 1 in
        Lwt.fail_with "I'm a failed promise")
      (range 4 7)
  in
  let ps = failure_ps @ successful_ps @ successful_ps in
  let* result = Lwt_utils.pick_successful ps in
  Assert.assert_true __LOC__ (List.mem result successful_ns) ;
  Lwt.return_unit

(* Check that rejected promises will be ignored. *)
let test_last_successful_ignore_rejected () =
  let exception I_am_an_exception in
  let length = 6 in
  let tasks = List.map (fun _ -> Lwt.task ()) (range 0 length) in
  let ps, rs = List.split tasks in
  let result = Lwt_utils.pick_successful ps in
  (* Ensure that all other promises failed. *)
  let* _ =
    Lwt_list.map_s
      (fun (p, r) ->
        Lwt.wakeup_exn r I_am_an_exception ;
        Lwt.catch (fun () -> p) (fun _ -> Lwt.return "not ok"))
      (List.tl tasks)
  in
  Assert.assert_true __LOC__ (Lwt.is_sleeping result) ;
  Lwt.wakeup (List.hd rs) "abc" ;
  let* result in
  Assert.equal ~loc:__LOC__ result "abc" ;
  Lwt.return_unit

(* Check that [pick_successful] fails if all promises are rejceted. *)
let test_none_sucessful () =
  let exception I_am_an_exception in
  let length = 6 in
  let ps =
    List.map
      (fun _ ->
        let* () = Lwt.pause () in
        Lwt.fail I_am_an_exception)
      (range 0 length)
  in
  Lwt.catch
    (fun () -> Lwt_utils.pick_successful ps)
    (function
      | Lwt_utils.None_successful __LINE_OF__ -> Lwt.return_unit
      | exn ->
          Assert.fail
            ~loc:__LOC__
            (fun pp s -> Format.fprintf pp "%s" s)
            "Tezos_stdlib__Lwt_utils.None_successful"
            (Printexc.exn_slot_name exn))

(* Check that [Stdlib.Invalid_argument _] is raised when promises list is empty. *)
let test_empty_promises () =
  Lwt.catch
    (fun () -> Lwt_utils.pick_successful [])
    (function
      | Invalid_argument _ -> Lwt.return_unit
      | exn ->
          Assert.fail
            ~loc:__LOC__
            (fun pp s -> Format.fprintf pp "%s" s)
            "Stdlib.Invalid_argument"
            (Printexc.exn_slot_name exn))

let test_needle_in_haystack () =
  let exception Failed in
  let length = 10_000 in
  let success_i = 7320 in
  let ps =
    List.map
      (fun i ->
        let pauses = 3 * (i + 1) mod 11 in
        (* 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 0, 3... *)
        let* () = n_pauses pauses in
        if i = success_i then Lwt.return success_i else Lwt.fail Failed)
      (range 0 length)
  in
  let* result = Lwt_utils.pick_successful ps in
  Assert.equal ~loc:__LOC__ result success_i ;
  Lwt.return_unit

(* Check that cancelling parent promise cancels all children promises. *)
let test_cancelling_parent_cancels_children () =
  let ps =
    List.map
      (fun _ ->
        let p, _ = Lwt.task () in
        p)
      (range 0 3)
  in
  let result = Lwt_utils.pick_successful ps in
  Lwt.cancel result ;
  let all_children_cancelled =
    List.for_all
      (fun p ->
        let state = Lwt.state p in
        match state with Lwt.Fail Lwt.Canceled -> true | _ -> false)
      ps
  in
  Assert.assert_true
    (Format.sprintf "LOC %s: Expected all children to be cancelled." __LOC__)
    all_children_cancelled ;
  Lwt.return_unit

let pick_fulfilled_tests =
  [
    ("test_already_successful", `Quick, test_already_successful);
    ("test_already_rejected", `Quick, test_already_rejected);
    ("test_first_successful", `Quick, test_first_successful);
    ( "test_first_successful_remaining_canceled",
      `Quick,
      test_first_successful_remaining_canceled );
    ("test_multi_successful", `Quick, test_multi_successful);
    ( "test_last_successful_ignore_rejected",
      `Quick,
      test_last_successful_ignore_rejected );
    ("test_none_sucessful", `Quick, test_none_sucessful);
    ("test_empty_promises", `Quick, test_empty_promises);
    ("test_needle_in_haystack", `Quick, test_needle_in_haystack);
    ( "test_cancelling_parent_cancels_children",
      `Quick,
      test_cancelling_parent_cancels_children );
  ]

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "Lwt_utils"
       [("pick_fulfilled", pick_fulfilled_tests)]
