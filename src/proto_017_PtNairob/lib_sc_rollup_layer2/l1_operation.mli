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

open Octez_smart_rollup

(** L1 operations produced (and injected) by the rollup node. *)
type t =
  | Add_messages of {messages : string list}
  | Cement of {rollup : Address.t; commitment : Commitment.Hash.t}
  | Publish of {rollup : Address.t; commitment : Commitment.t}
  | Refute of {
      rollup : Address.t;
      opponent : Signature.Public_key_hash.t;
      refutation : Game.refutation;
    }
  | Timeout of {
      rollup : Address.t;
      stakers : Signature.Public_key_hash.t * Signature.Public_key_hash.t;
    }

(** Encoding for L1 operations (used by injector for on-disk persistence). *)
val encoding : t Data_encoding.t

(** Manager operation for a given L1 operation. *)
val to_manager_operation : t -> Protocol.Alpha_context.packed_manager_operation

(** L1 operation corresponding to a manager operation if any. *)
val of_manager_operation :
  'a Protocol.Alpha_context.manager_operation -> t option

(** Pretty printer (human readable) for L1 operations. *)
val pp : Format.formatter -> t -> unit

(** [false] if the injector will accept duplicate such operations. *)
val unique : t -> bool
