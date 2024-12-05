(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

let bez nbytes r =
  let rec aux acc nbytes offset =
    if nbytes <= 0 then (acc, offset)
    else
      let c = int_of_char @@ Rope.get r offset in
      aux Z.(logor (shift_left acc 8) (of_int c)) (nbytes - 1) (offset + 1)
  in
  aux Z.zero nbytes 0

let z ?(unsigned = false) nbytes r =
  let pos = int_of_char (Rope.get r 0) lsr 7 = 0 in
  let z, offset = bez nbytes r in
  if pos || unsigned then (z, offset)
  else
    let base = Z.pred @@ Z.(pow (of_int 2)) (nbytes * 8) in
    (Z.(neg @@ (pred z lxor base)), offset)
