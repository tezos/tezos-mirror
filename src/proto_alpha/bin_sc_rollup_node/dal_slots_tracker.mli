(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** The rollup node keeps the list of dal slots to which the rollup is
    subscribed to for each block it needs to process. This is to determine
    whether the inbox for a given block will need to be retrieved from the
    block operations, or from the data availability layer after lag levels
    have passed and the slot for the block has been declared available.

    The state of subscribed slots per block is persistent.
*)

type error += Cannot_read_block_metadata of Block_hash.t

(** [process_head node_ctxt head] performs the following operations:
    {ul
      {li it fetches the slot indices to which the rollup is subscribed to,
       and stores them in [Store.Dal_slot_subbscriptions] }
      {li it reads the endorsements for headers published endorsement_lag
      levels preceding [head] from the block metadata, determines which
      ones the rollup node will download, and stores the results in
      [Store.Dal_confirmed_slots].}
    }  *)
val process_head : Node_context.t -> Layer1.head -> unit tzresult Lwt.t
