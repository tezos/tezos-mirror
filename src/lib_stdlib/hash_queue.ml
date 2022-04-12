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
    (K : Hashtbl.HashedType) (V : sig
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
    let exception Elements of V.t list in
    let rev_elts =
      try
        Ring.fold_oldest_first
          (fun k v (count, acc) ->
            if count >= n then raise (Elements acc)
            else (
              action k v q ;
              (count + 1, v :: acc)))
          q
          (0, [])
        |> snd
      with Elements acc -> acc
    in
    List.rev rev_elts

  (* Redefining fold to have elements treated in order of oldest to newest *)
  let fold f q acc = Ring.fold_oldest_first f q acc

  let fold_s f q acc =
    let open Lwt.Syntax in
    fold
      (fun k v acc ->
        let* acc = acc in
        f k v acc)
      q
      (Lwt.return acc)

  let fold_es (type error) f q acc : (_, error) result Lwt.t =
    let open Lwt.Syntax in
    let exception Error of error in
    try
      let+ res =
        fold_s
          (fun k v acc ->
            let+ res = f k v acc in
            match res with Ok acc -> acc | Error e -> raise (Error e))
          q
          acc
      in
      Ok res
    with Error e -> Lwt.return_error e

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
