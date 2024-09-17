(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** Timing buffer which records elapsed time associated with each line written to it *)
type t

(** Create a new timing buffer. *)
val create : unit -> t

(** Add a message. If there is something to output it will be written to the provided channel.  *)
val add_message : t -> out_channel -> string -> unit

(** Flush the buffer contents to the given channel. *)
val flush_buffer : t -> out_channel -> unit
