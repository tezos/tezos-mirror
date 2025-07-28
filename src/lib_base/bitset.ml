(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type t = Z.t

let encoding = Data_encoding.z

type error +=
  | Invalid_position of int
  | Invalid_range of {pos : int; length : int}
  | Invalid_input of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"bitfield_invalid_position"
    ~title:"Invalid bitfield’s position"
    ~description:"Bitfields do not accept negative positions"
    (obj1 (req "position" int31))
    (function Invalid_position i -> Some i | _ -> None)
    (fun i -> Invalid_position i) ;
  register_error_kind
    `Permanent
    ~id:"bitfield_invalid_range"
    ~title:"Invalid bitfield’s position range"
    ~description:
      "Bitfields do not accept non-positive length nor negative positions"
    (obj2 (req "position" int31) (req "length" int31))
    (function Invalid_range {pos; length} -> Some (pos, length) | _ -> None)
    (fun (pos, length) -> Invalid_range {pos; length}) ;
  register_error_kind
    `Permanent
    ~id:"bitfield_invalid_input"
    ~title:"Invalid argument"
    ~description:"A bitset function was provided an invalid input"
    ~pp:(fun ppf name ->
      Format.fprintf ppf "Invalid input for function %s" name)
    (obj1 (req "function_name" string))
    (function Invalid_input f -> Some f | _ -> None)
    (fun f -> Invalid_input f)

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

let add_many field pos length =
  let open Result_syntax in
  let* () =
    error_when
      Compare.Int.(pos < 0 || length <= 0)
      (Invalid_range {pos; length})
  in
  return @@ Z.(logor field (shift_left (pred (shift_left one length)) pos))

let remove field pos =
  let open Result_syntax in
  let* () = error_when Compare.Int.(pos < 0) (Invalid_position pos) in
  return @@ Z.logand field Z.(lognot (shift_left one pos))

let remove_many field pos length =
  let open Result_syntax in
  let* () =
    error_when
      Compare.Int.(pos < 0 || length <= 0)
      (Invalid_range {pos; length})
  in
  return
  @@ Z.(logand field (lognot (shift_left (pred (shift_left one length)) pos)))

let shift_right field ~offset =
  let open Result_syntax in
  let* () = error_when Compare.Int.(offset < 0) (Invalid_input "shift_right") in
  return @@ Z.shift_right field offset

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
  let* () = error_when Compare.Int.(length < 0) (Invalid_input "fill") in
  return Z.(pred (shift_left one length))

let inter = Z.logand

let diff b1 b2 = Z.logand b1 (Z.lognot b2)

let occupied_size_in_bits = Z.numbits

let cardinal =
  (* The cardinal of a bitset is its hamming weight, i.e. the number of ones. *)
  Z.popcount

let to_z z = z

let from_z z =
  let open Result_syntax in
  let+ () = error_when (Z.sign z < 0) (Invalid_input "from_z") in
  z
