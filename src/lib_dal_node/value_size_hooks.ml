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
