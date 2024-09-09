(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type t = Z.t

type error += Invalid_position of int

let encoding = Data_encoding.n

let empty = Z.zero

let is_empty = Z.equal Z.zero

let equal = Z.equal

let mem field pos =
  let open Result_syntax in
  let* () = error_when Compare.Int.(pos < 0) (Invalid_position pos) in
  return @@ Z.testbit field pos

let add field pos =
  let open Result_syntax in
  let* () = error_when Compare.Int.(pos < 0) (Invalid_position pos) in
  return @@ Z.logor field Z.(shift_left one pos)

let remove field pos =
  let open Result_syntax in
  let* () = error_when Compare.Int.(pos < 0) (Invalid_position pos) in
  return @@ Z.logand field Z.(lognot (shift_left one pos))

let from_list positions = List.fold_left_e add empty positions

let to_list field =
  let[@tailrec] rec to_list pos acc field =
    if Z.equal Z.zero field then acc
    else
      let acc = if Z.testbit field 0 then pos :: acc else acc in
      to_list (pos + 1) acc (Z.shift_right field 1)
  in
  to_list 0 [] field |> List.rev

let fill ~length =
  let open Result_syntax in
  let* () = error_when Compare.Int.(length < 0) (Invalid_position length) in
  return Z.(pred (shift_left one length))

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

let cardinal =
  (* Cardinal of bitset is the hamming weight, i.e. the number of ones. *)
  Z.popcount

let to_z z = z
