(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += Invalid_slot of int

let () =
  register_error_kind
    `Permanent
    ~id:"slot.invalid_slot"
    ~title:"invalid slot"
    ~description:"Invalid slot"
    ~pp:(fun ppf x -> Format.fprintf ppf "invalid slot: %d" x)
    Data_encoding.(obj1 (req "bad_slot" int31))
    (function Invalid_slot x -> Some x | _ -> None)
    (fun x -> Invalid_slot x)

include Compare.Int

type slot = t

(* TODO? should there be some assertions to verify that slots are
   never too big ? Or do that in a storage module that depends on
   constants ? *)

let encoding = Data_encoding.uint16

let pp = Format.pp_print_int

let zero = 0

let to_int x = x

(* We assume 2^16 slots is big enough.

   We could increase that, but we would need to make sure there is no big
   performance penalty first. *)
let max_value = (1 lsl 16) - 1

let of_int_do_not_use_except_for_parameters i = i

let of_int i =
  if Compare.Int.(i < 0 || i > max_value) then error (Invalid_slot i) else ok i

let succ slot = of_int (slot + 1)

module Map = Map.Make (Compare.Int)
module Set = Set.Make (Compare.Int)

module Range = struct
  (* For now, we only need full intervals. If we ever need sparse ones, we
     could switch this representation to interval trees. [hi] and [lo] bounds
     are included. *)
  type t = Interval of {lo : int; hi : int}

  let create ~min ~count =
    error_when (min < 0) (Invalid_slot min) >>? fun () ->
    error_when (min > max_value) (Invalid_slot min) >>? fun () ->
    error_when (count < 1) (Invalid_slot count) >>? fun () ->
    error_when (count > max_value) (Invalid_slot count) >>? fun () ->
    let max = min + count - 1 in
    error_when (max > max_value) (Invalid_slot max) >>? fun () ->
    ok (Interval {lo = min; hi = max})

  let fold f init (Interval {lo; hi}) =
    let rec loop ~acc ~next =
      if Compare.Int.(next > hi) then acc
      else loop ~acc:(f acc next) ~next:(next + 1)
    in
    loop ~acc:(f init lo) ~next:(lo + 1)

  let fold_es f init (Interval {lo; hi}) =
    let rec loop ~acc ~next =
      if Compare.Int.(next > hi) then return acc
      else f acc next >>=? fun acc -> loop ~acc ~next:(next + 1)
    in
    f init lo >>=? fun acc -> loop ~acc ~next:(lo + 1)

  let rev_fold_es f init (Interval {lo; hi}) =
    let rec loop ~acc ~next =
      if Compare.Int.(next < lo) then return acc
      else f acc next >>=? fun acc -> loop ~acc ~next:(next - 1)
    in
    f init hi >>=? fun acc -> loop ~acc ~next:(hi - 1)
end

module Internal_for_tests = struct
  let of_int = of_int
end
