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

type t = Z.t

type error += Invalid_position of int

let encoding = Data_encoding.z

let empty = Z.zero

let mem field pos =
  error_when Compare.Int.(pos < 0) (Invalid_position pos) >>? fun () ->
  ok @@ Z.testbit field pos

let add field pos =
  error_when Compare.Int.(pos < 0) (Invalid_position pos) >>? fun () ->
  ok @@ Z.logor field Z.(shift_left one pos)

let from_list positions = List.fold_left_e add empty positions

let fill ~length =
  error_when Compare.Int.(length < 0) (Invalid_position length) >>? fun () ->
  ok Z.(pred (shift_left one length))

let inter = Z.logand

let diff b1 b2 = Z.logand b1 (Z.lognot b2)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"bitfield_invalid_position"
    ~title:"Invalid bitfield’s position"
    ~description:"Bitfields does not accept negative positions"
    (obj1 (req "position" int31))
    (function Invalid_position i -> Some i | _ -> None)
    (fun i -> Invalid_position i)

let occupied_size_in_bits = Z.numbits

let to_z z = z
