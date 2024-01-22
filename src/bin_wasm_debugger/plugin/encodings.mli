(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* [register name decoder] registers a decoder for the debugger. *)
val register : string -> (bytes -> string) -> unit

(* [get name] returns the decoder associated with the given name, if it is
   registered. *)
val get : string -> (bytes -> string) option
