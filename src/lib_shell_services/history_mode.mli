(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

(** History modes for the chain history storage

   History modes allow a node to require less disk storage. Indeed,
    depending on the chosen history mode, some parts of the complete
    chain history can be deleted as they are not required
    anymore. Three history modes are provided:

    - Full mode (default mode): The node stores the minimal data since
    the genesis required to reconstruct (or 'replay') the complete
    chain's ledger state.

    - Rolling mode: This is the lightest mode as it only maintains a
    minimal rolling fragment of the chain data so the node can still
    validate new blocks and synchronize with the head.

    - Archive: This is the heaviest mode as it keeps the whole chain
    data to be able to query any information stored on the chain since
    the genesis. It is particularly suitable for indexers or block
    explorers.

*)

(** The type for defining the number of additional cycles to
   preserve. *)
type additional_cycles = {offset : int}

(** The type for defining an history mode. *)
type t = Archive | Full of additional_cycles | Rolling of additional_cycles

(** The default value for the number of additional cycles to
   preserve.*)
val default_offset : int

(** The default full history mode value. Based on [default_offset]. *)
val default_full : t

(** The default rolling history mode value. Based on
   [default_offset]. *)
val default_rolling : t

(** The default history mode value. *)
val default : t

val encoding : t Data_encoding.t

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val pp_short : Format.formatter -> t -> unit

val tag : t Tag.def

(** The module for handling legacy history modes. It is only used for
   legacy support, see {!Tezos_store.Legacy} and {!Tezos_store.Snapshots}.
*)
module Legacy : sig
  type t = Archive | Full | Rolling

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

(** [convert legacy] returns the history mode of a given [legacy]
   history mode, using the default offset values.*)
val convert : Legacy.t -> t
