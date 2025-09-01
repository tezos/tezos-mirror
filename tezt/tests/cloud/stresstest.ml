(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t = {tps : int; seed : int}

let encoding =
  let open Data_encoding in
  conv
    (fun {tps; seed} -> (tps, seed))
    (fun (tps, seed) -> {tps; seed})
    (obj2 (req "tps" int31) (req "seed" int31))

let to_string {tps; seed} = Format.sprintf "TPS : %d / SEED : %d" tps seed

let typ =
  Clap.typ
    ~name:"stresstest"
    ~dummy:{tps = 0; seed = 0}
    ~parse:(fun str ->
      match str |> String.split_on_char '/' with
      | [n] ->
          Some
            {tps = int_of_string n; seed = Random.int 1073741823 (* 2^30 -1 *)}
      | [n; seed] -> Some {tps = int_of_string n; seed = int_of_string seed}
      | _ -> None)
    ~show:to_string
