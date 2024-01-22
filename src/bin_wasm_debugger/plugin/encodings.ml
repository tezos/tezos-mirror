(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let encodings : (string, bytes -> string) Stdlib.Hashtbl.t =
  Stdlib.Hashtbl.create 17

let register name encoding = Stdlib.Hashtbl.add encodings name encoding

let get name = Stdlib.Hashtbl.find_opt encodings name
