(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context

module Level_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

(** Committee lookup function: given a slot, returns the delegate pkh *)
type committee_lookup = Slot.t -> Signature.Public_key_hash.t option

type t = {
  committees : committee_lookup Level_map.t;
  attestation_lags : int list;
  number_of_slots : int;
}

let set_committee t ~level lookup_fn =
  Level_map.replace t.committees level lookup_fn

(** [get_committee t ~level] retrieves the committee lookup function for the
    given level from the cache. *)
let[@warning "-32"] get_committee t ~level =
  Level_map.find_opt t.committees level

let create ~attestation_lags ~number_of_slots =
  let max_lag = List.fold_left max 0 attestation_lags in
  {
    committees = Level_map.create (3 * max_lag);
    attestation_lags;
    number_of_slots;
  }
