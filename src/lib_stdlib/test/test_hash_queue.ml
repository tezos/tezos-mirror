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

module Assert = Assert

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

let test_create () =
  let q = Queue.create 10 in
  Assert.Int.equal ~loc:__LOC__ 10 (Queue.capacity q)

let test_replace () =
  let q = Queue.create 10 in
  Queue.replace q "v" 3 ;
  let v = Queue.find_opt q "v" in
  Assert.Int.Option.equal ~loc:__LOC__ (Some 3) v

let test_replace_existing () =
  let q = Queue.create 10 in
  Queue.replace q "v" 3 ;
  Queue.replace q "v" 12 ;
  let v = Queue.find_opt q "v" in
  Assert.Int.Option.equal ~loc:__LOC__ (Some 12) v

let test_replace_incr_length () =
  let q = init_queue 10 5 in
  Queue.replace q "v" 12 ;
  Assert.Int.equal ~loc:__LOC__ 6 (Queue.length q)

let test_peek () =
  let q = init_queue 10 10 in
  Assert.Int.Option.equal ~loc:__LOC__ (Some 0) (Queue.peek q)

let test_peek_empty () =
  let q = Queue.create 10 in
  Assert.is_none ~pp:Format.pp_print_int ~loc:__LOC__ (Queue.peek q)

let test_peek_at_most () =
  let q = init_queue 10 10 in
  Assert.Int.List.equal ~loc:__LOC__ [0; 1; 2] (Queue.peek_at_most q 3)

let test_peek_at_most_above_length () =
  let q = init_queue 3 2 in
  Assert.Int.List.equal ~loc:__LOC__ [0; 1] (Queue.peek_at_most q 3)

let test_peek_at_most_above_capacity () =
  let q = init_queue 3 3 in
  Assert.Int.List.equal ~loc:__LOC__ [0; 1; 2] (Queue.peek_at_most q 4)

let test_take () =
  let q = init_queue 10 10 in
  Assert.Int.Option.equal ~loc:__LOC__ (Some 0) (Queue.take q) ;
  Assert.Int.Option.equal ~loc:__LOC__ (Some 1) (Queue.peek q) ;
  Assert.Int.equal ~loc:__LOC__ 9 (Queue.length q)

let test_take_empty () =
  let q = Queue.create 10 in
  Assert.is_none ~loc:__LOC__ (Queue.take q)

let test_take_at_most () =
  let q = init_queue 10 10 in
  Assert.Int.List.equal ~loc:__LOC__ [0; 1; 2] (Queue.take_at_most q 3) ;
  Assert.Int.Option.equal ~loc:__LOC__ (Some 3) (Queue.peek q) ;
  Assert.Int.equal ~loc:__LOC__ 7 (Queue.length q)

let test_take_at_most_above_length () =
  let q = init_queue 10 2 in
  Assert.Int.List.equal ~loc:__LOC__ (Queue.take_at_most q 3) [0; 1] ;
  Assert.is_none ~loc:__LOC__ (Queue.peek q) ;
  Assert.Int.equal ~loc:__LOC__ 0 (Queue.length q)

let test_take_at_most_above_capacity () =
  let q = init_queue 3 3 in
  Assert.Int.List.equal ~loc:__LOC__ (Queue.take_at_most q 4) [0; 1; 2] ;
  Assert.is_none ~loc:__LOC__ (Queue.peek q) ;
  Assert.Int.equal ~loc:__LOC__ 0 (Queue.length q)

let test_replace_above_capacity () =
  let q = init_queue 10 10 in
  let length_before = Queue.length q in
  Queue.replace q "new_key" 10 ;
  Assert.Int.equal ~loc:__LOC__ length_before (Queue.length q) ;
  Assert.Int.Option.equal ~loc:__LOC__ (Some 1) (Queue.peek q) ;
  Assert.is_none
    ~pp:Format.pp_print_int
    ~loc:__LOC__
    (Queue.find_opt q (mock_key 0))

let test_clear () =
  let q = init_queue 10 10 in
  Queue.clear q ;
  Assert.Int.is_zero ~loc:__LOC__ (Queue.length q)

let test_fold () =
  let q = init_queue 10 10 in
  let vs = gen_values 10 in
  let vs_from_fold =
    (* The resulting list is newest to oldest *)
    Queue.fold (fun k v acc -> (k, v) :: acc) q []
  in
  Assert.equal_list
    ~loc:__LOC__
    ~eq:(fun (k1, v1) (k2, v2) -> String.equal k1 k2 && Int.equal v1 v2)
    ~pp:(fun ppf (k, v) -> Format.fprintf ppf "(%s, %d)" k v)
    vs
    (vs_from_fold |> List.rev)

let test_elements () =
  let q = init_queue 10 10 in
  let _, vs = gen_values 10 |> List.split in
  let elts = Queue.elements q in
  Assert.Int.List.equal ~loc:__LOC__ vs elts

let test_keys () =
  let q = init_queue 10 10 in
  let ks, _ = gen_values 10 |> List.split in
  let keys = Queue.keys q in
  Assert.String.List.equal ~loc:__LOC__ ks keys

let test_bindings () =
  let q = init_queue 10 10 in
  let bs = gen_values 10 in
  let bndgs = Queue.bindings q in
  let module Assert_bindings = Assert.Make_equalities (struct
    type t = (string * int) list

    let eq = Stdlib.( = )

    let pp =
      Format.pp_print_list (fun fmt (k, v) ->
          Format.fprintf fmt "[%s -> %d]" k v)
  end) in
  Assert_bindings.equal ~loc:__LOC__ bs bndgs

let test_take_replace_keep_order () =
  let q = init_queue 10 5 in
  let _ = Queue.take_at_most q 3 in
  (* Queue should be [3; 4] *)
  Queue.replace q "val<25>" 25 ;
  (* Queue is now be [3; 4; 25] *)
  Assert.Int.List.equal ~loc:__LOC__ [3; 4; 25] (Queue.elements q)

let () =
  Alcotest.run
    ~__FILE__
    "stdlib"
    [
      ( "hash_queue",
        [
          ("capacity (create n) = n", `Quick, test_create);
          ("replace", `Quick, test_replace);
          ("replace_incr_length", `Quick, test_replace_incr_length);
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
          ("clear", `Quick, test_clear);
          ("fold", `Quick, test_fold);
          ("elements", `Quick, test_elements);
          ("keys", `Quick, test_keys);
          ("bindings", `Quick, test_bindings);
          ("take_replace_keep_order", `Quick, test_take_replace_keep_order);
        ] );
    ]
