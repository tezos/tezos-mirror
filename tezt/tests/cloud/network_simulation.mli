(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [t] allows to configure the simulation of a network, relying on the actual
   distribution of rights that will be found in the imported data (data-dir or
   snapshot). It requires yes crypto to be enabled.
   The simulate option has three modes:
     - scatter(x,y): selects the [x] biggest bakers found, and scatters their
       baking rights, in a round robin fashion, on [y] baker daemons. This is
       particularly useful to scatter the baking power across several baker
       daemons,
     - map(x,y,z): maps [y] keys from the biggest bakers found onto [y] baker
       daemons (theses daemons are handling a single key) and scatters the
       remaining [x-y] keys to [z] baker daemons. This is particularly useful to
       simulate the behaviour of an actual network,
     - disabled: no simulation, we rely on the configuration.stake parameter.
   For example:
     - scatter(10,2): [[0;2;4;6;8];[1;3;5;7;9]]
     - map(10,2,1):[[0];[1];[2;3;4;5;6;7;8;9]]
     - map(10,2,2):[[0];[1];[2;5;6;8];[3;5;8;9]] *)

type t = Scatter of int * int | Map of int * int * int | Disabled

val encoding : t Data_encoding.t

val typ : t Clap.typ
