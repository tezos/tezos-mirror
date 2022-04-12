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

(** Testing
    _______

    Invocation: dune build @src/lib_stdlib/test/runtest
 *)

module Assert = Lib_test.Assert

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

let string_of_opt_int = function None -> "None" | Some i -> string_of_int i

let string_of_int_list l =
  Printf.sprintf "[%s]" @@ (List.map string_of_int l |> String.concat ";")

let test_create () =
  let q = Queue.create 10 in
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 10 (Queue.capacity q)

let test_replace () =
  let q = Queue.create 10 in
  Queue.replace q "v" 3 ;
  let v = Queue.find_opt q "v" in
  Assert.equal ~msg:__LOC__ (Some 3) v

let test_replace_existing () =
  let q = Queue.create 10 in
  Queue.replace q "v" 3 ;
  Queue.replace q "v" 12 ;
  let v = Queue.find_opt q "v" in
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 12) v

let test_replace_incr_length () =
  let q = init_queue 10 5 in
  Queue.replace q "v" 12 ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 6 (Queue.length q)

let test_peek () =
  let q = init_queue 10 10 in
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 0) (Queue.peek q)

let test_peek_empty () =
  let q = Queue.create 10 in
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ None (Queue.peek q)

let test_peek_at_most () =
  let q = init_queue 10 10 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    [0; 1; 2]
    (Queue.peek_at_most q 3)

let test_peek_at_most_above_length () =
  let q = init_queue 3 2 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    [0; 1]
    (Queue.peek_at_most q 3)

let test_peek_at_most_above_capacity () =
  let q = init_queue 3 3 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    [0; 1; 2]
    (Queue.peek_at_most q 4)

let test_take () =
  let q = init_queue 10 10 in
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 0) (Queue.take q) ;
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 1) (Queue.peek q) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 9 (Queue.length q)

let test_take_empty () =
  let q = Queue.create 10 in
  Assert.equal ~msg:__LOC__ None (Queue.take q)

let test_take_at_most () =
  let q = init_queue 10 10 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    [0; 1; 2]
    (Queue.take_at_most q 3) ;
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 3) (Queue.peek q) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 7 (Queue.length q)

let test_take_at_most_above_length () =
  let q = init_queue 10 2 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    (Queue.take_at_most q 3)
    [0; 1] ;
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ None (Queue.peek q) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 0 (Queue.length q)

let test_take_at_most_above_capacity () =
  let q = init_queue 3 3 in
  Assert.equal
    ~prn:string_of_int_list
    ~msg:__LOC__
    (Queue.take_at_most q 4)
    [0; 1; 2] ;
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ None (Queue.peek q) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 0 (Queue.length q)

let test_replace_above_capacity () =
  let q = init_queue 10 10 in
  let length_before = Queue.length q in
  Queue.replace q "new_key" 10 ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ length_before (Queue.length q) ;
  Assert.equal ~prn:string_of_opt_int ~msg:__LOC__ (Some 1) (Queue.peek q) ;
  Assert.equal
    ~prn:string_of_opt_int
    ~msg:__LOC__
    (Queue.find_opt q (mock_key 0))
    None

let test_filter () =
  let q = init_queue 10 10 in
  Queue.filter q (fun _ v -> v < 5) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 5 (Queue.length q)

let test_filter_none () =
  let q = init_queue 10 10 in
  Queue.filter q (fun _ v -> v < 15) ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 10 (Queue.length q)

let test_clear () =
  let q = init_queue 10 10 in
  Queue.clear q ;
  Assert.equal ~prn:string_of_int ~msg:__LOC__ 0 (Queue.length q)

let test_fold () =
  let q = init_queue 10 10 in
  let vs = gen_values 10 in
  let vs_from_fold =
    (* The resulting list is newest to oldest *)
    Queue.fold (fun k v acc -> (k, v) :: acc) q []
  in
  Assert.make_equal_list
    ~msg:__LOC__
    (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && Int.equal v1 v2)
    (fun (k, v) -> Printf.sprintf "(%s, %d)" k v)
    vs
    (vs_from_fold |> List.rev)

let test_elements () =
  let q = init_queue 10 10 in
  let (_, vs) = gen_values 10 |> List.split in
  let elts = Queue.elements q in
  Assert.make_equal_list ~msg:__LOC__ Int.equal string_of_int vs elts

let test_take_replace_keep_order () =
  let q = init_queue 10 5 in
  let _ = Queue.take_at_most q 3 in
  (* Queue should be [3; 4] *)
  Queue.replace q "val<25>" 25 ;
  (* Queue is now be [3; 4; 25] *)
  Assert.make_equal_list
    ~msg:__LOC__
    Int.equal
    string_of_int
    [3; 4; 25]
    (Queue.elements q)

let () =
  Alcotest.run
    "stdlib"
    [
      ( "hash_queue",
        [
          ("capacity (create n) = n", `Quick, test_create);
          ("replace", `Quick, test_replace);
          ("replace_existing", `Quick, test_replace_existing);
          ("peek", `Quick, test_peek);
          ("peek_empty", `Quick, test_peek_empty);
          ("peek_at_most", `Quick, test_peek_at_most);
          ("peek_at_most_above_length", `Quick, test_peek_at_most_above_length);
          ( "peek_at_most_above_capacity",
            `Quick,
            test_peek_at_most_above_capacity );
          ("take", `Quick, test_take);
          ("take_empty", `Quick, test_take_empty);
          ("take_at_most", `Quick, test_take_at_most);
          ("take_at_most_above_length", `Quick, test_take_at_most_above_length);
          ( "take_at_most_above_capacity",
            `Quick,
            test_take_at_most_above_capacity );
          ("replace_above_capacity", `Quick, test_replace_above_capacity);
          ("filter", `Quick, test_filter);
          ("filter_none", `Quick, test_filter_none);
          ("clear", `Quick, test_clear);
          ("fold", `Quick, test_fold);
          ("elements", `Quick, test_elements);
          ("take_replace_keep_order", `Quick, test_take_replace_keep_order);
        ] );
    ]
