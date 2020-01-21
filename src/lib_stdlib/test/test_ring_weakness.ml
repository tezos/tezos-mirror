(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* NOTE:
   This test is intended to be run manually. It is not part of a test-suite, it
   is not on the CI, it doesn't run with [dune runtest]. This is because the
   test is fragile and does not fit well in the yes-no tests of a CI. Instead,
   it is expected that anyone who modifies `WeakRingTable` runs the test to
   ensure that it is still leak free.

   To run the test, use the command
   [dune build @src/lib_stdlib/test/runweaknesstest]. This dune target invokes
   the present file; it also sets up some environment variables so that the
   OCaml heap stays small.

   To interpret the output of the test, refer to the comments below in this
   file.

*)

let () = Random.self_init ()

module K : Hashtbl.HashedType with type t = int list = struct
  type t = int list

  let equal l1 l2 =
    match (l1, l2) with ([x], [y]) -> x = y | _ -> assert false

  let hash = Hashtbl.hash
end

module WRT = WeakRingTable.Make (K)

let wrt = WRT.create 16

let size_of () = Obj.(reachable_words (repr wrt))

let len () = WRT.length wrt

module Spamming = struct
  let n = ref (Random.bits ())

  (* Unrelated memory to spice things up *)
  let x = ref []

  (* We retainer references to some keys to make sure that it does not cause a
     leak. *)
  let retainer = ref []

  let spam () =
    let r = Random.int 2000 in
    if r = 0 then x := (Random.float 4096., Random.int64 4096L) :: !x
    else if r <= 200 then
      let k = !n - Random.int 56 in
      WRT.remove wrt [k]
    else (
      incr n ;
      let k = [!n] in
      WRT.add wrt k (string_of_int !n).[0] ;
      if r <= 800 then retainer := k :: !retainer )
end

let rec run max_sz max_len n limit =
  if n > limit then ()
  else (
    if n mod 25_000 = 0 then
      (* Every so many insert operations, some statistics are printed.
         The most important of these statistics is the size of the
         data-structure (in words). Or more precisely, the maximum of all the
         sizes that have been observed so far. If there are no leaks, this
         number should plateau.
         Another statistic that's printed is the maximum number of elements ever
         observed inside the data-structure. It should also plateau. *)
      Format.printf
        "steps: %8d;        max size:     %8d words;        max len:     %8d \
         elements;        retained:  %8d\n\
         %!"
        n
        max_sz
        max_len
        (List.length !Spamming.retainer) ;
    Spamming.spam () ;
    run (max max_sz (size_of ())) (max max_len (len ())) (n + 1) limit )

let rec run_with_gc n limit =
  if n > limit then ()
  else (
    if n mod 25_000 = 0 then (
      Gc.full_major () ;
      Format.printf
        "steps: %8d;        post-GC size: %8d words;        post-GC len: %8d \
         elements;        retained:  %8d\n\
         %!"
        n
        (size_of ())
        (len ())
        (List.length !Spamming.retainer) ;
      () ) ;
    Spamming.spam () ;
    run_with_gc (n + 1) limit )

let () = Format.printf "START\n%!"

let () = run 0 0 0 750_000

let () = Format.printf "WITH_GC\n%!"

let () = run_with_gc 750_000 1_000_000

let () = Format.printf "END\n%!"
