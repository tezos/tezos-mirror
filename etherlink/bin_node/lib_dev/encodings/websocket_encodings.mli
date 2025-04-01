(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Close status for websocket close frame
   https://datatracker.ietf.org/doc/html/rfc6455#section-7.4.1 *)
type close_status = Normal_closure | Going_away | Policy | Message_too_big

val code_of_close_status : close_status -> int

val frame_encoding : Websocket.Frame.t Data_encoding.t
