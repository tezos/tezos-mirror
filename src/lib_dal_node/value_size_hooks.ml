(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* [share_size ()] is used to pass the share size to the store in a delayed
   fashion, when the protocol becomes known to the daemon. *)
let share_size_ref = ref None

let set_share_size size =
  match !share_size_ref with
  | None -> share_size_ref := Some size
  | Some previous_size ->
      if Int.equal size previous_size then ()
      else
        Stdlib.failwith
          "Store.set_share_size: new share size incompatible with current store"

let share_size () =
  match !share_size_ref with
  | None -> Stdlib.failwith "Store.share_size: value not initialized"
  | Some size -> size

(* Same idea as for [share_size]. *)
let number_of_slots_ref = ref None

let set_number_of_slots n =
  match !number_of_slots_ref with
  | None -> number_of_slots_ref := Some n
  | Some previous_number ->
      if Int.equal n previous_number then ()
      else
        Stdlib.failwith
          "Store.set_number_of_slots: new share size incompatible with current \
           store"

let number_of_slots () =
  match !number_of_slots_ref with
  | None -> Stdlib.failwith "Store.number_of_slots: value not initialized"
  | Some n -> n
