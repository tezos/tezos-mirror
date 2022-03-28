(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module RingoMaker : Ringo.MAP_MAKER =
(val Ringo.(map_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise))

module Make
    (K : Stdlib.Hashtbl.HashedType) (V : sig
      type t
    end) =
struct
  module Ring = RingoMaker (K)
  include Ring

  type nonrec t = V.t t

  let elements q = Ring.fold (fun _ x acc -> x :: acc) q []

  (** [oldest_elements q n f] returns the (at most) [n] oldest elements of the
      queue and calls [f] on the bindings for these elements. The elements are
      returned from oldest to newest. *)
  let oldest_elements q n action =
    (* FIXME: https://gitlab.com/nomadic-labs/ringo/-/issues/5 *)
    (* Ring.fold is from newest to oldest elements. So we iterate on the
       elements until we reach the [n] ones at the end, i.e. the elements we
       want to "take". *)
    let first_index = Ring.length q - n in
    Ring.fold
      (fun k v (count, acc) ->
        let acc =
          if count >= first_index then (
            action k v q ;
            v :: acc)
          else acc
        in
        (count + 1, acc))
      q
      (0, [])
    |> snd

  (* Redefining fold to have elements treated in order of oldest to newest *)
  (* FIXME: https://gitlab.com/nomadic-labs/ringo/-/issues/5 *)
  let fold f q acc =
    let bindings = fold (fun k v acc -> (k, v) :: acc) q [] in
    List.fold_left (fun acc (k, v) -> f k v acc) acc bindings

  let peek q =
    match oldest_elements q 1 (fun _ _ _ -> ()) with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false

  let take q =
    match oldest_elements q 1 (fun k _ q -> remove q k) with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false

  let peek_at_most q n = oldest_elements q n (fun _ _ _ -> ())

  let take_at_most q n = oldest_elements q n (fun k _ q -> remove q k)
end
