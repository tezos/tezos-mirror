(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Daemon handling the bakers life cycle. *)

module Baker : sig
  type t
end

type state = {
  binaries_directory : string option;
  node_endpoint : string;
  baker_args : string list;
  mutable current_baker : Baker.t option;
}

val run : state:state -> unit tzresult Lwt.t
