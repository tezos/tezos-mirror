(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Level_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

type is_attested_fn =
  number_of_slots:int -> number_of_lags:int -> lag_index:int -> int -> bool

type entry = int * is_attested_fn option

type cached_ops = entry list

type t = cached_ops Level_map.t

let create ~max_size = Level_map.create max_size

let find t ~level = Level_map.find_opt t level

let add t ~level ~attestation_ops = Level_map.replace t level attestation_ops
