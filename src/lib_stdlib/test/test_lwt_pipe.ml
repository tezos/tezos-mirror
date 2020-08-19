(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Lwt.Infix

(* Basic push/pop test *)

let rec producer queue = function
  | 0 ->
      Lwt.return_unit
  | n ->
      Lwt_pipe.push queue () >>= fun () -> producer queue (pred n)

let rec consumer queue = function
  | 0 ->
      Lwt.return_unit
  | n ->
      Lwt_pipe.pop queue >>= fun _ -> consumer queue (pred n)

let rec gen f = function 0 -> [] | n -> f () :: gen f (pred n)

let run capacity unit_weight actors actor_work =
  let q = Lwt_pipe.create ~size:(capacity, fun () -> unit_weight) () in
  assert (Lwt_pipe.is_empty q) ;
  let producers = gen (fun () -> producer q actor_work) actors in
  let consumers = gen (fun () -> consumer q actor_work) actors in
  Lwt.join producers
  >>= fun () ->
  Lwt.join consumers
  >>= fun () ->
  assert (Lwt_pipe.is_empty q) ;
  Lwt.return_unit

let push_pop () =
  Lwt_list.iter_p
    (fun (capacity, unit_weight, actors, actor_work) ->
      run capacity unit_weight actors actor_work)
    [ (max_int, 0, 1, 100);
      (max_int, 0, 10, 10);
      (max_int, 0, 100, 1);
      (10, 1, 20, 20);
      (10, 3, 10, 10);
      (10, 3, 1, 100);
      (10, 9, 1, 3);
      (10, 6, 3, 1);
      (1, 1, 10, 10);
      (1, 1, 50, 2) ]

(* push/pop_all *)

let run capacity unit_weight prods production min_iterations =
  let q = Lwt_pipe.create ~size:(capacity, fun () -> unit_weight) () in
  let producers = gen (fun () -> producer q production) prods in
  let rec consume iterations =
    Lwt_unix.sleep 0.01
    >>= fun () ->
    match Lwt_pipe.pop_all_now q with
    | _ :: _ ->
        consume (iterations + 1)
    | [] ->
        Lwt.return iterations
  in
  let consumer = consume 0 in
  Lwt.join producers
  >>= fun () ->
  consumer
  >>= fun iterations ->
  assert (Lwt_pipe.is_empty q) ;
  assert (iterations >= min_iterations) ;
  Lwt.return_unit

let push_pop_all () =
  Lwt_list.iter_p
    (fun (capacity, unit_weight, prods, prodtion, exp) ->
      run capacity unit_weight prods prodtion exp)
    [ (max_int, 0, 1, 100, 1);
      (max_int, 0, 10, 10, 1);
      (max_int, 0, 100, 1, 1);
      (10, 1, 10, 10, 10);
      (10, 3, 10, 5, 17);
      (10, 9, 1, 3, 3);
      (10, 6, 3, 1, 3);
      (1, 1, 2, 2, 4) ]

(* Scaffolding and main *)

let with_timeout t f () =
  let timeout = Lwt_unix.sleep t >|= fun () -> Error () in
  let main = f () >|= fun () -> Ok () in
  Lwt.pick [main; timeout]
  >|= function Ok () -> () | Error () -> assert false

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Lwt_pipe"
       [ ( "push-pop",
           [ ("push-pop", `Quick, with_timeout 0.2 push_pop);
             ("push-pop-all", `Quick, with_timeout 0.5 push_pop_all) ] ) ]
