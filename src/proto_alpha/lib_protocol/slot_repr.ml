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

(* TODO? should there be some assertions to verify that slots are
   never too big ? Or do that in a storage module that depends on
   constants ? *)

let encoding = Data_encoding.uint16

let pp = Format.pp_print_int

let zero = 0

let succ = succ

let to_int x = x

let max_value = (1 lsl 16) - 1

let of_int_do_not_use_except_for_parameters i = i

let of_int_exn i =
  if Compare.Int.(i < 0 || i > max_value) then
    invalid_arg
      (Format.sprintf
         "valid slot values are in the interval [0, %d] (%d given)"
         max_value
         i)
  else i

module Map = Map.Make (Compare.Int)
module Set = Set.Make (Compare.Int)

module List = struct
  (* Expected invariant: list of increasing values *)
  (* TODO find a way to properly enforce this invariant *)
  type nonrec t = t list

  module Compressed = struct
    type elt = {skip : int; take : int}

    type encoded = elt list

    let elt_encoding =
      Data_encoding.(
        conv
          (fun {skip; take} -> (skip, take))
          (fun (skip, take) -> {skip; take})
          (obj2 (req "skip" uint16) (req "take" uint16)))

    let encoding = Data_encoding.list elt_encoding

    let encode l : encoded =
      let rec loop_taking ~pos ~skipped ~taken l =
        match l with
        | [] -> if taken > 0 then [{skip = skipped; take = taken}] else []
        | h :: t ->
            if h = pos then
              loop_taking ~pos:(pos + 1) ~skipped ~taken:(taken + 1) t
            else
              let elt = {skip = skipped; take = taken} in
              let skipped = h - pos in
              let taken = 1 in
              let elts = loop_taking ~pos:(h + 1) ~skipped ~taken t in
              elt :: elts
      in
      loop_taking ~pos:0 ~skipped:0 ~taken:0 l

    let decode (elts : encoded) =
      let rec loop ~pos elts =
        match elts with
        | [] -> Ok []
        | elt :: elts -> (
            let pos = pos + elt.skip in
            match
              List.init ~when_negative_length:() elt.take (fun i -> i + pos)
            with
            | Ok l -> (
                let pos = pos + elt.take in
                match loop ~pos elts with Ok t -> Ok (l @ t) | e -> e)
            | Error () ->
                Error "A compressed element contains a negative list size")
      in
      loop ~pos:0 elts
  end

  let encoding =
    Data_encoding.conv_with_guard
      Compressed.encode
      Compressed.decode
      Compressed.encoding

  let slot_range ~min ~count =
    error_when (min < 0) (Invalid_slot min) >>? fun () ->
    error_when (min > max_value) (Invalid_slot min) >>? fun () ->
    error_when (count < 1) (Invalid_slot count) >>? fun () ->
    error_when (count > max_value) (Invalid_slot count) >>? fun () ->
    let max = min + count - 1 in
    error_when (max > max_value) (Invalid_slot max) >>? fun () ->
    ok Misc.(min --> max)
end
