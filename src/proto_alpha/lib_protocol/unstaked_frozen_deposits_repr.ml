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

(* Associative list acting like a map, i.e. not duplicated keys are expected.
   Though sorting is not required. *)
type t = (Cycle_repr.t * Deposits_repr.t) list

(* A version of {!t} in which all cycles older than a given [unslashable_cycle]
   are squashed together using {!Deposits_repr.(++?)}. *)
type squashed = t

let empty = []

let encoding =
  let open Data_encoding in
  list (tup2 Cycle_repr.encoding Deposits_repr.encoding)

let squash_unslashable ~unslashable_cycle l =
  let open Result_syntax in
  match unslashable_cycle with
  | None -> return l
  | Some unslashable_cycle ->
      let rec aux unslashable slashable = function
        | [] -> ok ((unslashable_cycle, unslashable) :: slashable)
        | (c, d) :: tl when Cycle_repr.(c <= unslashable_cycle) ->
            let* unslashable = Deposits_repr.(unslashable ++? d) in
            aux unslashable slashable tl
        | hd :: tl -> aux unslashable (hd :: slashable) tl
      in
      aux Deposits_repr.zero [] l

let get ~normalized_cycle l =
  List.assoc ~equal:Cycle_repr.( = ) normalized_cycle l
  |> Option.value ~default:Deposits_repr.zero

let update ~f ~normalized_cycle l =
  let open Result_syntax in
  let rec aux acc = function
    | [] -> return acc
    | (c, d) :: tl when Cycle_repr.(c = normalized_cycle) ->
        let+ d = f d in
        List.rev_append acc ((c, d) :: tl)
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] l
