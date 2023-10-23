(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type void = |

type minimal_state = {
  payload : string;
  level : Raw_level_repr.t option;
  message_counter : Z.t;
}

module type S = sig
  include Sc_rollup_PVM_sig.S

  val parse_boot_sector : string -> string option

  val pp_boot_sector : Format.formatter -> string -> unit
end

module Protocol_implementation :
  S
    with type context = unit
     and type state = minimal_state
     and type proof = void

val reference_initial_state_hash : Sc_rollup_repr.State_hash.t
