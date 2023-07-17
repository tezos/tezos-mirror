(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Associative list acting like a map.
   Invariants:
     - the list is sorted by keys,
     - not duplicated keys. *)
type t = (Cycle_repr.t * Deposits_repr.t) list

let rec check_well_formed = function
  | [] -> Result_syntax.return_unit
  | (c1, _) :: (c2, _) :: _ when Cycle_repr.(c2 <= c1) ->
      Error "Malformed unstaked frozen deposits"
  | _ :: tl -> check_well_formed tl

let id_check_well_formed l =
  let open Result_syntax in
  let+ () = check_well_formed l in
  l

(* A version of {!t} in which all cycles older than a given [unslashable_cycle]
   are squashed together using {!Deposits_repr.(++?)}. *)
type squashed = t

let empty = []

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun l -> l)
    id_check_well_formed
    (list (tup2 Cycle_repr.encoding Deposits_repr.encoding))

let squash_unslashable ~unslashable_cycle l =
  let open Result_syntax in
  match (unslashable_cycle, l) with
  | Some unslashable_cycle, (c, unslashable) :: tl
    when Cycle_repr.(c <= unslashable_cycle) ->
      let rec aux unslashable = function
        | (c, d) :: tl when Cycle_repr.(c <= unslashable_cycle) ->
            let* unslashable = Deposits_repr.(unslashable ++? d) in
            aux unslashable tl
        | slashable -> return ((unslashable_cycle, unslashable) :: slashable)
      in
      aux unslashable tl
  | _ -> return l

let get ~normalized_cycle l =
  List.assoc ~equal:Cycle_repr.( = ) normalized_cycle l
  |> Option.value ~default:Deposits_repr.zero

(* not tail-rec *)
let rec update ~f ~normalized_cycle l =
  let open Result_syntax in
  match l with
  | (c, d) :: tl when Cycle_repr.(c = normalized_cycle) ->
      let+ d = f d in
      (c, d) :: tl
  | ((c, _) as hd) :: tl when Cycle_repr.(c < normalized_cycle) ->
      let+ tl = update ~f ~normalized_cycle tl in
      hd :: tl
  | _ ->
      let+ d = f Deposits_repr.zero in
      (normalized_cycle, d) :: l
