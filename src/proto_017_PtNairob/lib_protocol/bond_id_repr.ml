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

type t =
  | Tx_rollup_bond_id of Tx_rollup_repr.t
  | Sc_rollup_bond_id of Sc_rollup_repr.t

include Compare.Make (struct
  type nonrec t = t

  let compare id1 id2 =
    match (id1, id2) with
    | Tx_rollup_bond_id id1, Tx_rollup_bond_id id2 ->
        Tx_rollup_repr.compare id1 id2
    | Sc_rollup_bond_id id1, Sc_rollup_bond_id id2 ->
        Sc_rollup_repr.Address.compare id1 id2
    | Tx_rollup_bond_id _, Sc_rollup_bond_id _ -> -1
    | Sc_rollup_bond_id _, Tx_rollup_bond_id _ -> 1
end)

let encoding =
  let open Data_encoding in
  def "bond_id"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Tx_rollup_bond_id"
           (obj1 (req "tx_rollup" Tx_rollup_repr.encoding))
           (function Tx_rollup_bond_id id -> Some id | _ -> None)
           (fun id -> Tx_rollup_bond_id id);
         case
           (Tag 1)
           ~title:"Smart_rollup_bond_id"
           (obj1 (req "smart_rollup" Sc_rollup_repr.encoding))
           (function Sc_rollup_bond_id id -> Some id | _ -> None)
           (fun id -> Sc_rollup_bond_id id);
       ]

let pp ppf = function
  | Tx_rollup_bond_id id -> Tx_rollup_repr.pp ppf id
  | Sc_rollup_bond_id id -> Sc_rollup_repr.pp ppf id

let destruct id =
  (* String.starts_with from the stdlib 4.14, with [unsafe_get] replaced by
     [get], comparators replaced by their versions in [Compare.*]. *)
  let starts_with ~prefix s =
    let open String in
    let len_s = length s and len_pre = length prefix in
    let rec aux i =
      if Compare.Int.(i = len_pre) then true
      else if Compare.Char.(get s i <> get prefix i) then false
      else aux (i + 1)
    in
    Compare.Int.(len_s >= len_pre) && aux 0
  in
  if starts_with ~prefix:Tx_rollup_prefixes.rollup_address.prefix id then
    match Tx_rollup_repr.of_b58check_opt id with
    | Some id -> Result.ok (Tx_rollup_bond_id id)
    | None -> Result.error "Cannot parse transaction rollup id"
  else if starts_with ~prefix:Sc_rollup_repr.Address.prefix id then
    match Sc_rollup_repr.Address.of_b58check_opt id with
    | Some id -> Result.ok (Sc_rollup_bond_id id)
    | None -> Result.error "Cannot parse smart rollup id"
  else Result.error "Cannot parse rollup id"

let construct = function
  | Tx_rollup_bond_id id -> Tx_rollup_repr.to_b58check id
  | Sc_rollup_bond_id id -> Sc_rollup_repr.Address.to_b58check id

let rpc_arg =
  RPC_arg.make
    ~descr:"A bond identifier."
    ~name:"bond_id"
    ~construct
    ~destruct
    ()

module Internal_for_test = struct
  let destruct = destruct

  let construct = construct
end

module Index = struct
  type nonrec t = t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt encoding)
    | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end
