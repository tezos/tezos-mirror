(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                        *)
(*                                                                           *)
(*****************************************************************************)

type void = |

type minimal_state = {
  payload : string;
  level : Raw_level_repr.t option;
  message_counter : Z.t;
  tick : Z.t;
}

(* This encoding is used in the rollup node when embedding the state into an Irmin context. *)
val minimal_state_encoding : minimal_state Data_encoding.t

val make_empty_state : unit -> minimal_state

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = minimal_state
     and type proof = void
