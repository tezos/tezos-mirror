(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**

  This is a test for the Lwt_pipe module.

  It tests mainly the core functionality: pushing/popping, and size limitation.
  An additional test (introspection) runs through a pre-made scenario, calling
  functions one after the other, and checks internal state as it evolves.

*)

open Lwt.Infix

(* Basic push/pop test *)

let rec producer queue = function
  | 0 -> Lwt.return_unit
  | n -> Lwt_pipe.push queue () >>= fun () -> producer queue (pred n)

let rec consumer queue = function
  | 0 -> Lwt.return_unit
  | n -> Lwt_pipe.pop queue >>= fun _ -> consumer queue (pred n)

let rec gen f = function 0 -> [] | n -> f () :: gen f (pred n)

let run capacity unit_weight actors actor_work =
  let q = Lwt_pipe.create ~size:(capacity, fun () -> unit_weight) () in
  let producers = gen (fun () -> producer q actor_work) actors in
  let consumers = gen (fun () -> consumer q actor_work) actors in
  Lwt.join producers >>= fun () ->
  Lwt.join consumers >>= fun () ->
  assert (Lwt_pipe.is_empty q) ;
  Lwt.return_unit

let push_pop () =
  Lwt_list.iter_p
    (fun (capacity, unit_weight, actors, actor_work) ->
      run capacity unit_weight actors actor_work)
    [
      (max_int, 0, 1, 100);
      (max_int, 0, 10, 10);
      (max_int, 0, 100, 1);
      (10, 1, 20, 20);
      (10, 3, 10, 10);
      (10, 3, 1, 100);
      (10, 9, 1, 3);
      (10, 6, 3, 1);
      (1, 1, 10, 10);
      (1, 1, 50, 2);
    ]

(* push/pop_all *)

let run capacity unit_weight prods production exp_iterations =
  let q = Lwt_pipe.create ~size:(capacity, fun () -> unit_weight) () in
  let producers = gen (fun () -> producer q production) prods in
  let rec consume iterations =
    Lwt_unix.sleep 0.01 >>= fun () ->
    match Lwt_pipe.pop_all_now q with
    | _ :: _ -> consume (iterations + 1)
    | [] -> Lwt.return iterations
  in
  let consumer = consume 0 in
  Lwt.join producers >>= fun () ->
  consumer >>= fun iterations ->
  assert (Lwt_pipe.is_empty q) ;
  assert (iterations = exp_iterations) ;
  Lwt.return_unit

let push_pop_all () =
  Lwt_list.iter_p
    (fun (capacity, unit_weight, prods, prodtion, exp) ->
      run capacity unit_weight prods prodtion exp)
    [
      (max_int, 0, 1, 100, 1);
      (max_int, 0, 10, 10, 1);
      (max_int, 0, 100, 1, 1);
      (10, 1, 10, 10, 12);
      (10, 9, 1, 3, 3);
      (10, 6, 3, 1, 3);
      (1, 1, 2, 2, 4);
    ]

(* fifo *)

let rec producer q = function
  | 0 -> Lwt_pipe.push q 0
  | n -> Lwt_pipe.push q n >>= fun () -> producer q (pred n)

let rec consumer q = function
  | 0 -> Lwt_pipe.pop q >|= fun m -> assert (0 = m)
  | n ->
      Lwt_pipe.pop q >>= fun m ->
      assert (n = m) ;
      consumer q (pred n)

let run capacity unit_weight work =
  let q = Lwt_pipe.create ~size:(capacity, fun _ -> unit_weight) () in
  let producer = producer q work in
  let consumer = consumer q work in
  producer >>= fun () ->
  consumer >>= fun () ->
  assert (Lwt_pipe.is_empty q) ;
  Lwt.return_unit

let count_down () =
  Lwt_list.iter_p
    (fun (capacity, unit_weight, work) -> run capacity unit_weight work)
    [
      (max_int, 0, 0);
      (max_int, 0, 1);
      (max_int, 0, 10);
      (max_int, 0, 100);
      (10, 9, 10);
      (10, 5, 1);
      (1, 1, 0);
      (1, 1, 1);
      (1, 1, 10);
      (1, 1, 100);
    ]

(* introspection *)

let introspect () =
  let q = Lwt_pipe.create ~size:(4, fun n -> n) () in
  assert (Lwt_pipe.is_empty q) ;
  assert (Lwt.state @@ Lwt_pipe.empty q = Lwt.Return ()) ;
  let peek_0 = Lwt_pipe.peek q in
  assert (Lwt.state peek_0 = Lwt.Sleep) ;
  let () = assert (Lwt_pipe.push_now q 0) in
  Lwt.pause () >>= fun () ->
  assert (Lwt.state peek_0 = Lwt.Return 0) ;
  let () = assert (Lwt_pipe.push_now q 0) in
  let () = assert (Lwt_pipe.push_now q 0) in
  (* can push 4 and then even more because of weight 0 *)
  let () = assert (Lwt_pipe.push_now q 0) in
  let () = assert (Lwt_pipe.push_now q 0) in
  let empty_0 = Lwt_pipe.empty q in
  assert (Lwt.state empty_0 = Lwt.Sleep) ;
  assert (Lwt_pipe.peek_all q = [0; 0; 0; 0; 0]) ;
  let pop_0 = Lwt_pipe.pop q in
  assert (Lwt.state pop_0 = Lwt.Return 0) ;
  let () = match Lwt_pipe.pop_now q with Some 0 -> () | _ -> assert false in
  let pop_234 = Lwt_pipe.pop_all q in
  assert (Lwt.state pop_234 = Lwt.Return [0; 0; 0]) ;
  Lwt.pause () >>= fun () ->
  assert (Lwt.state empty_0 = Lwt.Return ()) ;
  let () = assert (Lwt_pipe.push_now q 1) in
  let peek_0 = Lwt_pipe.peek q in
  assert (Lwt.state peek_0 = Lwt.Return 1) ;
  let () = assert (Lwt_pipe.push_now q 1) in
  let () = assert (Lwt_pipe.push_now q 1) in
  let push_big = Lwt_pipe.push q 4 in
  let push_small = Lwt_pipe.push q 1 in
  assert (Lwt.state push_big = Lwt.Sleep) ;
  assert (Lwt.state push_small = Lwt.Sleep) ;
  let () = match Lwt_pipe.pop_now q with Some 1 -> () | _ -> assert false in
  Lwt.pause () >>= fun () ->
  assert (Lwt.state push_big = Lwt.Sleep) ;
  assert (Lwt.state push_small = Lwt.Return ()) ;
  let () = match Lwt_pipe.pop_now q with Some 1 -> () | _ -> assert false in
  let () = match Lwt_pipe.pop_now q with Some 1 -> () | _ -> assert false in
  let () = match Lwt_pipe.pop_now q with Some 1 -> () | _ -> assert false in
  Lwt.pause () >>= fun () ->
  assert (Lwt.state push_big = Lwt.Return ()) ;
  let () = match Lwt_pipe.pop_now q with Some 4 -> () | _ -> assert false in
  Lwt.return_unit

(* Scaffolding and main *)

let with_timeout t f () =
  let timeout = Lwt_unix.sleep t >|= fun () -> Error () in
  let main = f () >|= fun () -> Ok () in
  Lwt.pick [main; timeout] >|= function Ok () -> () | Error () -> assert false

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Lwt_pipe"
       [
         ( "push-pop",
           [
             ("push-pop", `Quick, with_timeout 0.2 push_pop);
             ("push-pop-all", `Quick, with_timeout 0.5 push_pop_all);
           ] );
         ("fifo", [("count-down", `Quick, with_timeout 0.2 count_down)]);
         ("scenarii", [("introspect", `Quick, introspect)]);
       ]
