(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech, <contact@trili.tech>                        *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_repr
module PS = Sc_rollup_PVM_sig

(* [void] definition from [Sc_rollup_origination_machine] *)
type void = |

let void =
  Data_encoding.(
    conv_with_guard
      (function (_ : void) -> .)
      (fun _ -> Error "void has no inhabitant")
      unit)

type minimal_state = {
  payload : string;
  level : Raw_level_repr.t option;
  message_counter : Z.t;
  tick : Z.t;
}

let minimal_state_encoding =
  let open Data_encoding in
  conv
    (fun {payload; level; message_counter; tick} ->
      (payload, level, message_counter, tick))
    (fun (payload, level, message_counter, tick) ->
      {payload; level; message_counter; tick})
  @@ obj4
       (req "payload" (string Hex))
       (req "level" (option Raw_level_repr.encoding))
       (req "message_counter" n)
       (req "tick" n)

let make_empty_state () =
  {payload = ""; level = None; message_counter = Z.zero; tick = Z.zero}

let state_hash _state =
  (* In order to synchronise with the node implementation of the PVM at genesis,
   * we set the state hash to be the initial state hash of the node
   * implementation. *)
  State_hash.of_b58check_exn
    "srs129JscUr3XsPcNFUEiKqVNP38tn8oksbGir1qYXgQs8QD7bcNNd"

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = minimal_state
     and type proof = void = struct
  let pp state =
    Lwt.return @@ fun fmt () -> Format.pp_print_string fmt state.payload

  type state = minimal_state

  type context = unit

  type hash = State_hash.t

  type proof = void

  let proof_encoding = void

  let proof_start_state = function (_ : proof) -> .

  let proof_stop_state = function (_ : proof) -> .

  let state_hash state = Lwt.return (state_hash state)

  let initial_state ~empty = Lwt.return empty

  let install_boot_sector state boot_sector =
    Lwt.return {state with payload = boot_sector}

  let verify_proof ~is_reveal_enabled:_ _input = function (_ : proof) -> .

  type output_proof = void

  let output_proof_encoding = void

  let output_of_output_proof = function (_ : proof) -> .

  let state_of_output_proof = function (_ : proof) -> .

  let verify_output_proof = function (_ : proof) -> .

  let check_dissection ~default_number_of_sections:_ ~start_chunk:_
      ~stop_chunk:_ =
    assert false

  let parse_boot_sector s = Some s

  let get_current_level {level; _} = Lwt.return level
end
