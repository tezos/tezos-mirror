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

type t = Tx_rollup_bond_id of Tx_rollup_repr.t

include Compare.Make (struct
  type nonrec t = t

  let compare id1 id2 =
    match (id1, id2) with
    | Tx_rollup_bond_id id1, Tx_rollup_bond_id id2 ->
        Tx_rollup_repr.compare id1 id2
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
           (function Tx_rollup_bond_id id -> Some id)
           (fun id -> Tx_rollup_bond_id id);
       ]

let pp ppf (Tx_rollup_bond_id id) = Tx_rollup_repr.pp ppf id

let rpc_arg =
  let construct (Tx_rollup_bond_id id) = Tx_rollup_repr.to_b58check id in
  let destruct id =
    Result.map_error
      (fun _ -> "Cannot parse tx rollup id")
      (Tx_rollup_repr.of_b58check id >>? fun id -> ok (Tx_rollup_bond_id id))
  in
  RPC_arg.make
    ~descr:"A bond identifier."
    ~name:"bond_id"
    ~construct
    ~destruct
    ()

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
