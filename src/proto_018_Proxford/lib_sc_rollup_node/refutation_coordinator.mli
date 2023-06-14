(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Component for managing refutation games.
    This module is implemented as a single worker in the rollup node,
    which takes care of processing new L1 heads, and coordinating
    the refutation game players. (See {!Refutation_player}).
*)

(** Initiatilize the refuation coordinator. *)
val init : Node_context.rw -> unit tzresult Lwt.t

(** Process a new l1 head. This means that the coordinator will:
    {ol
      {li Gather all existing conflicts}
      {li Launch new refutation players for each conflict that doesn't
          have a player in this node}
      {li Kill all players whose conflict has disappeared from L1}
      {li Make all players play a step in the refutation}
    }
  *)
val process : Layer1.head -> unit tzresult Lwt.t

(** Shutdown the refutation coordinator. *)
val shutdown : unit -> unit Lwt.t
