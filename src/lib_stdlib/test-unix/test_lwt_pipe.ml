(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    -------
    Component:    Stdlib
    Invocation:   dune exec src/lib_stdlib/test-unix/main.exe
    Subject:      Lwt_pipe
*)

open Lwt.Syntax

(**

  This is the part of the test for the Bounded sub-module.

  It tests mainly the core functionality: pushing/popping, and size limitation.
  An additional test (introspection) runs through a pre-made scenario, calling
  functions one after the other, and checks internal state as it evolves.

*)
module Bounded = struct
  (**

  It tests mainly the core functionality: pushing/popping, and size limitation.
  An additional test (introspection) runs through a pre-made scenario, calling
  functions one after the other, and checks internal state as it evolves.

*)

  (* Basic push/pop test *)

  let rec producer queue = function
    | 0 -> Lwt.return_unit
    | n ->
        let* () = Lwt_pipe.Bounded.push queue () in
        producer queue (pred n)

  let rec consumer queue = function
    | 0 -> Lwt.return_unit
    | n ->
        let* _ = Lwt_pipe.Bounded.pop queue in
        consumer queue (pred n)

  let rec gen f = function 0 -> [] | n -> f () :: gen f (pred n)

  let run max_size unit_weight actors actor_work =
    let q =
      Lwt_pipe.Bounded.create ~max_size ~compute_size:(fun () -> unit_weight) ()
    in
    let producers = gen (fun () -> producer q actor_work) actors in
    let consumers = gen (fun () -> consumer q actor_work) actors in
    let* () = Lwt.join producers in
    let* () = Lwt.join consumers in
    assert (Lwt_pipe.Bounded.is_empty q) ;
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

  let run max_size unit_weight prods production exp_iterations =
    let q =
      Lwt_pipe.Bounded.create ~max_size ~compute_size:(fun () -> unit_weight) ()
    in
    let producers = gen (fun () -> producer q production) prods in
    let rec consume iterations =
      let* () = Lwt_unix.sleep 0.01 in
      match Lwt_pipe.Bounded.pop_all_now q with
      | _ :: _ -> consume (iterations + 1)
      | [] -> Lwt.return iterations
    in
    let consumer = consume 0 in
    let* () = Lwt.join producers in
    let* iterations = consumer in
    assert (Lwt_pipe.Bounded.is_empty q) ;
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
    | 0 -> Lwt_pipe.Bounded.push q 0
    | n ->
        let* () = Lwt_pipe.Bounded.push q n in
        producer q (pred n)

  let rec consumer q = function
    | 0 ->
        let+ m = Lwt_pipe.Bounded.pop q in
        assert (0 = m)
    | n ->
        let* m = Lwt_pipe.Bounded.pop q in
        assert (n = m) ;
        consumer q (pred n)

  let run max_size unit_weight work =
    let q =
      Lwt_pipe.Bounded.create ~max_size ~compute_size:(fun _ -> unit_weight) ()
    in
    let producer = producer q work in
    let consumer = consumer q work in
    let* () = producer in
    let* () = consumer in
    assert (Lwt_pipe.Bounded.is_empty q) ;
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
    let q = Lwt_pipe.Bounded.create ~max_size:4 ~compute_size:(fun n -> n) () in
    assert (Lwt_pipe.Bounded.is_empty q) ;
    let peek_0 = Lwt_pipe.Bounded.peek q in
    assert (Lwt.state peek_0 = Lwt.Sleep) ;
    let () = assert (Lwt_pipe.Bounded.push_now q 0) in
    let* () = Lwt.pause () in
    assert (Lwt.state peek_0 = Lwt.Return 0) ;
    let () = assert (Lwt_pipe.Bounded.push_now q 0) in
    let () = assert (Lwt_pipe.Bounded.push_now q 0) in
    (* can push 4 and then even more because of weight 0 *)
    let () = assert (Lwt_pipe.Bounded.push_now q 0) in
    let () = assert (Lwt_pipe.Bounded.push_now q 0) in
    assert (Lwt_pipe.Bounded.peek_all_now q = [0; 0; 0; 0; 0]) ;
    let pop_0 = Lwt_pipe.Bounded.pop q in
    assert (Lwt.state pop_0 = Lwt.Return 0) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 0 -> () | _ -> assert false
    in
    let pop_234 = Lwt_pipe.Bounded.pop_all q in
    assert (Lwt.state pop_234 = Lwt.Return [0; 0; 0]) ;
    let* () = Lwt.pause () in
    let () = assert (Lwt_pipe.Bounded.push_now q 1) in
    let peek_0 = Lwt_pipe.Bounded.peek q in
    assert (Lwt.state peek_0 = Lwt.Return 1) ;
    let () = assert (Lwt_pipe.Bounded.push_now q 1) in
    let () = assert (Lwt_pipe.Bounded.push_now q 1) in
    let push_big = Lwt_pipe.Bounded.push q 4 in
    let push_small = Lwt_pipe.Bounded.push q 1 in
    assert (Lwt.state push_big = Lwt.Sleep) ;
    assert (Lwt.state push_small = Lwt.Sleep) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 1 -> () | _ -> assert false
    in
    let* () = Lwt.pause () in
    assert (Lwt.state push_big = Lwt.Sleep) ;
    assert (Lwt.state push_small = Lwt.Return ()) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 1 -> () | _ -> assert false
    in
    assert (Lwt.state push_big = Lwt.Sleep) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 1 -> () | _ -> assert false
    in
    assert (Lwt.state push_big = Lwt.Sleep) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 1 -> () | _ -> assert false
    in
    let* () = Lwt.pause () in
    assert (Lwt.state push_big = Lwt.Return ()) ;
    let () =
      match Lwt_pipe.Bounded.pop_now q with Some 4 -> () | _ -> assert false
    in
    Lwt.return_unit
end

(**

  This is a test for the Unbounded submodule

  It tests mainly the core functionality: pushing/popping.
  An additional test (introspection) runs through a pre-made scenario, calling
  functions one after the other, and checks internal state as it evolves.

*)
module Unbounded = struct
  (* Basic push/pop test *)

  let rec producer queue = function
    | 0 -> Lwt.return_unit
    | n ->
        Lwt_pipe.Unbounded.push queue () ;
        producer queue (pred n)

  let rec consumer queue = function
    | 0 -> Lwt.return_unit
    | n ->
        let* _ = Lwt_pipe.Unbounded.pop queue in
        consumer queue (pred n)

  let rec gen f = function 0 -> [] | n -> f () :: gen f (pred n)

  let run actors actor_work =
    let q = Lwt_pipe.Unbounded.create () in
    let producers = gen (fun () -> producer q actor_work) actors in
    let consumers = gen (fun () -> consumer q actor_work) actors in
    let* () = Lwt.join producers in
    let* () = Lwt.join consumers in
    assert (Lwt_pipe.Unbounded.is_empty q) ;
    Lwt.return_unit

  let push_pop () =
    Lwt_list.iter_p
      (fun (actors, actor_work) -> run actors actor_work)
      [(1, 100); (10, 10); (100, 1); (20, 20); (1, 3); (3, 1); (50, 2)]

  (* fifo *)

  let rec producer q = function
    | 0 -> Lwt_pipe.Unbounded.push q 0
    | n ->
        Lwt_pipe.Unbounded.push q n ;
        producer q (pred n)

  let rec consumer q = function
    | 0 ->
        let+ m = Lwt_pipe.Unbounded.pop q in
        assert (0 = m)
    | n ->
        let* m = Lwt_pipe.Unbounded.pop q in
        assert (n = m) ;
        consumer q (pred n)

  let run work =
    let q = Lwt_pipe.Unbounded.create () in
    producer q work ;
    let* () = consumer q work in
    assert (Lwt_pipe.Unbounded.is_empty q) ;
    Lwt.return_unit

  let count_down () = Lwt_list.iter_p (fun work -> run work) [0; 1; 10; 100]
end

let with_timeout t f () =
  let timeout =
    let+ () = Lwt_unix.sleep t in
    Error ()
  in
  let main =
    let+ () = f () in
    Ok ()
  in
  let+ r = Lwt.pick [main; timeout] in
  match r with Ok () -> () | Error () -> assert false

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "Lwt_pipe"
       [
         ( "Bounded",
           [
             ("push-pop", `Quick, with_timeout 0.2 Bounded.push_pop);
             ("push-pop-all", `Quick, with_timeout 0.5 Bounded.push_pop_all);
             ("count-down", `Quick, with_timeout 0.2 Bounded.count_down);
             ("introspect", `Quick, Bounded.introspect);
           ] );
         ( "Unbounded",
           [
             ("push-pop", `Quick, Unbounded.push_pop);
             ("count-down", `Quick, Unbounded.count_down);
           ] );
       ]
