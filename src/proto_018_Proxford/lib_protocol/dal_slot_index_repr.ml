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

type t = int

let encoding = Data_encoding.uint8

let pp = Format.pp_print_int

let zero = 0

type error += Invalid_slot_index of {given : int; min : int; max : int}

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"dal_slot_index_repr.index.invalid_index"
    ~title:"Invalid Dal slot index"
    ~description:"The given index is out of range of representable slot indices"
    ~pp:(fun ppf (given, min, max) ->
      Format.fprintf
        ppf
        "The given index %d is out of range of representable slot indices [%d, \
         %d]"
        given
        min
        max)
    (obj3 (req "given" int31) (req "min" int31) (req "max" int31))
    (function
      | Invalid_slot_index {given; min; max} -> Some (given, min, max)
      | _ -> None)
    (fun (given, min, max) -> Invalid_slot_index {given; min; max})

let check_is_in_range ~number_of_slots slot_index =
  error_unless
    Compare.Int.(slot_index >= zero && slot_index < number_of_slots)
    (Invalid_slot_index
       {given = slot_index; min = zero; max = number_of_slots + 1})

let of_int ~number_of_slots slot_index =
  let open Result_syntax in
  let* () = check_is_in_range ~number_of_slots slot_index in
  return slot_index

let of_int_opt ~number_of_slots slot_index =
  Option.of_result @@ of_int ~number_of_slots slot_index

let to_int slot_index = slot_index [@@ocaml.inline always]

let to_int_list l = l [@@ocaml.inline always]

let compare = Compare.Int.compare

let equal = Compare.Int.equal

let slots_range ~number_of_slots ~lower ~upper =
  let open Result_syntax in
  let* () = check_is_in_range ~number_of_slots lower in
  let* () = check_is_in_range ~number_of_slots upper in
  return Misc.(lower --> upper)

let slots_range_opt ~number_of_slots ~lower ~upper =
  Option.of_result @@ slots_range ~number_of_slots ~lower ~upper
