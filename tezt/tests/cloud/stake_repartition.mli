(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Dal : sig
  type t =
    | Custom of int list
    | Mimic of {network : Network.public; max_nb_bakers : int option}

  val encoding : t Data_encoding.t

  val typ : t Clap.typ

  val parse_arg :
    stake_arg:t -> simulation_arg:Network_simulation.t -> int list Lwt.t
end

module Layer1 : sig
  type t = Auto | Manual of int list

  val encoding : t Data_encoding.t

  val typ : t Clap.typ
end
