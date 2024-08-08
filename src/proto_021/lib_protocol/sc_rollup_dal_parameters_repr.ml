(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  number_of_slots : int64;
  attestation_lag : int64;
  slot_size : int64;
  page_size : int64;
}

let pp ppf {number_of_slots; attestation_lag; slot_size; page_size} =
  Format.fprintf
    ppf
    "number_of_slots: %Ld ; attestation_lag: %Ld ; slot_size: %Ld ; page_size: \
     %Ld"
    number_of_slots
    attestation_lag
    slot_size
    page_size

let equal t1 t2 =
  Compare.Int64.(
    t1.attestation_lag = t2.attestation_lag
    && t1.slot_size = t2.slot_size
    && t1.page_size = t2.page_size)

let encoding =
  let open Data_encoding in
  conv
    (fun {number_of_slots; attestation_lag; slot_size; page_size} ->
      (number_of_slots, attestation_lag, slot_size, page_size))
    (fun (number_of_slots, attestation_lag, slot_size, page_size) ->
      {number_of_slots; attestation_lag; slot_size; page_size})
    (obj4
       (req "number_of_slots" int64)
       (req "attestation_lag" int64)
       (req "slot_size" int64)
       (req "page_size" int64))
