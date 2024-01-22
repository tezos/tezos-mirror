(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Stdlib
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe \
                  -- --file test_hashtbl.ml
   Subject:      Test hashtable
*)

(* This test suite relies heavily on Lwt. It also deals with error a lot, but it
   checks most errors directly. So we don't need the syntactic support to
   alleviate the error handling. *)
open Support.Lib.Monad.Lwt_syntax
module Assert = Assert

module IntESHashtbl = Support.Lib.Hashtbl.Make_es (struct
  type t = int

  let equal x y = x = y

  let hash x = x
end)

let test_add_remove _ _ =
  let t = IntESHashtbl.create 2 in
  let* r = IntESHashtbl.find_or_make t 0 (fun () -> return_ok 0) in
  match r with
  | Error _ -> Assert.String.fail "Ok 0" "Error _" ~msg:"find_or_make"
  | Ok n -> (
      if not (n = 0) then
        Assert.String.fail
          "Ok 0"
          (Format.asprintf "Ok %d" n)
          ~msg:"find_or_make"
      else
        match IntESHashtbl.find t 0 with
        | None -> Assert.String.fail "Some (Ok 0)" "None" ~msg:"find"
        | Some p -> (
            let* r = p in
            match r with
            | Error _ ->
                Assert.String.fail "Some (Ok 0)" "Some (Error _)" ~msg:"find"
            | Ok n ->
                if not (n = 0) then
                  Assert.String.fail
                    "Some (Ok 0)"
                    (Format.asprintf "Some (Ok %d)" n)
                    ~msg:"find"
                else (
                  IntESHashtbl.remove t 0 ;
                  match IntESHashtbl.find t 0 with
                  | Some _ ->
                      Assert.String.fail "None" "Some _" ~msg:"remove;find"
                  | None -> Lwt.return_unit)))

let test_add_add _ _ =
  let t = IntESHashtbl.create 2 in
  let* _ = IntESHashtbl.find_or_make t 0 (fun () -> return_ok 0) in
  let* _ = IntESHashtbl.find_or_make t 0 (fun () -> return_ok 1) in
  match IntESHashtbl.find t 0 with
  | None -> Assert.String.fail "Some (Ok 0)" "None" ~msg:"find"
  | Some p -> (
      let* r = p in
      match r with
      | Error _ -> Assert.String.fail "Some (Ok 0)" "Some (Error _)" ~msg:"find"
      | Ok n ->
          if not (n = 0) then
            Assert.String.fail
              "Some (Ok 0)"
              (Format.asprintf "Some (Ok %d)" n)
              ~msg:"find"
          else Lwt.return_unit)

let test_length _ _ =
  let t = IntESHashtbl.create 2 in
  let* _ = IntESHashtbl.find_or_make t 0 (fun () -> Lwt.return_ok 0) in
  let* _ = IntESHashtbl.find_or_make t 1 (fun () -> Lwt.return_ok 1) in
  let* _ = IntESHashtbl.find_or_make t 2 (fun () -> Lwt.return_ok 2) in
  let* _ = IntESHashtbl.find_or_make t 3 (fun () -> Lwt.return_ok 3) in
  let l = IntESHashtbl.length t in
  if not (l = 4) then
    Assert.String.fail "4" (Format.asprintf "%d" l) ~msg:"length"
  else Lwt.return_unit

let test_self_clean _ _ =
  let t = IntESHashtbl.create 2 in
  let* _ = IntESHashtbl.find_or_make t 0 (fun () -> Lwt.return (Ok 0)) in
  let* _ = IntESHashtbl.find_or_make t 1 (fun () -> Lwt.return (Error [])) in
  let* _ = IntESHashtbl.find_or_make t 2 (fun () -> Lwt.return (Error [])) in
  let* _ = IntESHashtbl.find_or_make t 3 (fun () -> Lwt.return (Ok 3)) in
  let* _ = IntESHashtbl.find_or_make t 4 (fun () -> Lwt.return (Ok 4)) in
  let* _ = IntESHashtbl.find_or_make t 5 (fun () -> Lwt.return (Error [])) in
  let* () =
    Lwt.catch
      (fun () ->
        let* _ = IntESHashtbl.find_or_make t 6 (fun () -> Lwt.fail Not_found) in
        Assert.fail_msg "Not_found exception should propagate")
      (function Not_found -> Lwt.return_unit | exn -> Lwt.reraise exn)
  in
  let l = IntESHashtbl.length t in
  if not (l = 3) then
    Assert.String.fail "3" (Format.asprintf "%d" l) ~msg:"length"
  else Lwt.return_unit

let test_order _ _ =
  let t = IntESHashtbl.create 2 in
  let wter, wker = Lwt.task () in
  let world = ref [] in
  (* PROMISE A *)
  let p_a =
    let* r_a =
      IntESHashtbl.find_or_make t 0 (fun () ->
          let* r = wter in
          world := "a_inner" :: !world ;
          Lwt.return r)
    in
    world := "a_outer" :: !world ;
    Lwt.return r_a
  in
  let* () = Lwt.pause () in
  (* PROMISE B *)
  let p_b =
    let* r_b =
      IntESHashtbl.find_or_make t 0 (fun () ->
          world := "b_inner" :: !world ;
          Lwt.return (Ok 1024))
    in
    world := "b_outer" :: !world ;
    Lwt.return r_b
  in
  let* () = Lwt.pause () in
  (* Wake up A *)
  Lwt.wakeup wker (Ok 0) ;
  (* Check that both A and B get expected results *)
  let* () =
    let* r = p_a in
    match r with
    | Error _ -> Assert.String.fail "Ok 0" "Error _" ~msg:"find_or_make(a)"
    | Ok n ->
        if not (n = 0) then
          Assert.String.fail
            "Ok 0"
            (Format.asprintf "Ok %d" n)
            ~msg:"find_or_make(a)"
        else Lwt.return_unit
  in
  let* () =
    let* r = p_b in
    match r with
    | Error _ -> Assert.String.fail "Ok 0" "Error _" ~msg:"find_or_make(b)"
    | Ok n ->
        if not (n = 0) then
          Assert.String.fail
            "Ok 0"
            (Format.asprintf "Ok %d" n)
            ~msg:"find_or_make(b)"
        else Lwt.return_unit
  in
  (* Check that the `world` record is as expected *)
  match !world with
  | ["b_outer"; "a_outer"; "a_inner"] | ["a_outer"; "b_outer"; "a_inner"] ->
      Lwt.return_unit
  | world ->
      Assert.String.fail
        "[outers;a_inner]"
        Format.(asprintf "[%a]" (pp_print_list pp_print_string) world)
        ~msg:"world"

let tests =
  [
    Alcotest_lwt.test_case "add_remove" `Quick test_add_remove;
    Alcotest_lwt.test_case "add_add" `Quick test_add_add;
    Alcotest_lwt.test_case "length" `Quick test_length;
    Alcotest_lwt.test_case "self_clean" `Quick test_self_clean;
    Alcotest_lwt.test_case "order" `Quick test_order;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "hashtbl" [("hashtbl-lwt", tests)] |> Lwt_main.run
