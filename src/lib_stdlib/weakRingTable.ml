(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

(* WARNING

   This warning is intended for developers that are modifying this file.

   The correctness of this file (w.r.t. the documentation and w.r.t. its name)
   requires that the WeakRingTable has *weakness*. I.e., indefinitely inserting
   bindings in a WeakRingTable should never cause a memory leak.

   In order to test for the weakness of this module, simply run the following
   command:
   [dune build @src/lib_stdlib/test/runweaknesstest]

   This command executes the test file [test_ring_weakness.ml] with a few
   specific environment variables. Unfortunately, because this test is fragile
   and does not return true/false, it is not executed by the CI. You (YOU!) are
   required (REQUIRED) to perform the test locally before pushing changes to the
   remote repository.

   *)

module type S = sig
  type 'a t

  type key

  val create : int -> 'a t

  val add : 'a t -> key -> 'a -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val find_opt : 'a t -> key -> 'a option

  val remove : 'a t -> key -> unit

  val length : 'a t -> int
end

let prng = lazy (Random.State.make_self_init ())

module MakeGeneral (M : Hashtbl.SeededHashedType) = struct
  module Table = Ephemeron.K1.Make (struct
    (* NOTE: So that it is scanned by the GC, the type [t] must be an
       allocated/boxed type. *)
    type t = nativeint

    (* NOTE: the [nativeint] fed into this hash functions are the result of
       [Nativeint.of_int @@ M.hash v] (for arbistrary [v]s). We assume [M.hash]
       already has the expected properties of a good hash, and we assert that
       [Nativeint.of_int] does not degrade those properties. *)
    let hash a = Nativeint.to_int a

    let equal = Nativeint.equal
  end)

  type key = M.t

  module Visit_tracking = Set.Make (struct
    type t = nativeint

    let compare = Nativeint.compare
  end)

  type 'a t = {table : 'a Table.t; ring : (nativeint * M.t) Ring.t; seed : int}

  let create r n =
    let seed = if r then Random.State.bits (Lazy.force prng) else 0 in
    {table = Table.create n; ring = Ring.create n; seed}

  let add {ring; table; seed} k v =
    let i = Nativeint.of_int @@ M.hash seed k in
    Ring.add ring (i, k) ;
    Table.replace table i v

  let find_opt {table; seed; _} k =
    let i = Nativeint.of_int @@ M.hash seed k in
    Table.find_opt table i

  let fold f {table; ring; _} acc =
    Ring.fold
      ring
      ~init:(acc, Visit_tracking.empty)
      ~f:(fun (acc, visited) (i64, k) ->
        if Visit_tracking.mem i64 visited then (acc, visited)
        else
          match Table.find_opt table i64 with
          | None ->
              (acc, visited)
          | Some elt ->
              (f k elt acc, Visit_tracking.add i64 visited))
    |> fst

  let iter f t = fold (fun k v () -> f k v) t ()

  let remove {table; seed; _} k =
    let i = Nativeint.of_int @@ M.hash seed k in
    Table.remove table i

  let length {table; _} = Table.length table
end

module MakeSeeded (K : Hashtbl.SeededHashedType) : S with type key = K.t =
struct
  include MakeGeneral (K)

  let create n = create true n
end

module Make (K : Hashtbl.HashedType) : S with type key = K.t = struct
  include MakeGeneral (struct
    type t = K.t

    let equal = K.equal

    let hash _ k = K.hash k
  end)

  let create n = create false n
end
