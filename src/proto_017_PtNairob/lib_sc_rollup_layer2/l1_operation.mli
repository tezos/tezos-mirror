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

open Protocol.Alpha_context

(** L1 operations produced (and injected) by the rollup node. *)
type t =
  | Add_messages of {messages : string list}
  | Cement of {rollup : Sc_rollup.t; commitment : Sc_rollup.Commitment.Hash.t}
  | Publish of {rollup : Sc_rollup.t; commitment : Sc_rollup.Commitment.t}
  | Refute of {
      rollup : Sc_rollup.t;
      opponent : Sc_rollup.Staker.t;
      refutation : Sc_rollup.Game.refutation;
    }
  | Timeout of {rollup : Sc_rollup.t; stakers : Sc_rollup.Game.Index.t}

(** Encoding for L1 operations (used by injector for on-disk persistence). *)
val encoding : t Data_encoding.t

(** Manager operation for a given L1 operation. *)
val to_manager_operation : t -> packed_manager_operation

(** L1 operation corresponding to a manager operation if any. *)
val of_manager_operation : 'a manager_operation -> t option

(** Pretty printer (human readable) for L1 operations. *)
val pp : Format.formatter -> t -> unit

(** [false] if the injector will accept duplicate such operations. *)
val unique : t -> bool
