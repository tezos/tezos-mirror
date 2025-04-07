(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

open Types

let hex h =
  let n = String.length h in
  let s = if n mod 2 = 0 then h else "0" ^ h in
  Rope.of_string @@ Hex.to_string (`Hex s)

let bytes (b : b) = hex (b :> string)

let address (a : address) = hex (a :> string)

let bez nbytes i =
  let rec aux acc nbytes i =
    if nbytes <= 0 then Rope.concat Rope.empty acc
    else
      let c =
        Rope.make 1 @@ char_of_int @@ Z.(to_int @@ logand i @@ of_int 0xff)
      in
      aux (c :: acc) (nbytes - 1) @@ Z.shift_right i 8
  in
  aux [] nbytes i

let z nbytes i =
  if i > Z.zero then bez nbytes i
  else
    let base = Z.pred @@ Z.(pow (of_int 2)) (nbytes * 8) in
    let one = Z.(abs i lxor base) in
    bez nbytes (Z.succ one)

let transaction (t : b transaction_input) =
  let dst = match t.ti_to with None -> Rope.empty | Some to_ -> address to_ in
  let sig_b =
    match t.ti_signature with
    | None -> []
    | Some s -> [Rlp.int s.v; S (bytes s.r); S (bytes s.s)]
  in
  let data = match t.ti_data with None -> Rope.empty | Some d -> bytes d in
  Rope.concat2 (Rope.make 1 '\002')
  @@ Rlp.encode
  @@ L
       ([
          Rlp.int t.ti_chain_id;
          Rlp.int t.ti_nonce;
          Rlp.z t.ti_max_priority_fee;
          Rlp.z t.ti_max_fee;
          Rlp.int t.ti_gas_limit;
          S dst;
          Rlp.z t.ti_value;
          S data;
          L
            (List.map
               (fun al ->
                 L
                   [
                     S (address al.al_address);
                     L (List.map (fun b -> S (bytes b)) al.al_storage_keys);
                   ])
               t.ti_access_list);
        ]
       @ sig_b)
