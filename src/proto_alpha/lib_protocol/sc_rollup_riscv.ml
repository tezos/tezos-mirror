(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_repr
module PS = Sc_rollup_PVM_sig

(* [void] definition from [Sc_rollup_machine_no_proofs] *)
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

let state_hash state =
  [Data_encoding.Binary.to_bytes_exn minimal_state_encoding state]
  |> Context_hash.hash_bytes |> State_hash.context_hash_to_state_hash

let reference_initial_state_hash = state_hash (make_empty_state ())

module type S = sig
  include PS.S

  val parse_boot_sector : string -> string option

  val pp_boot_sector : Format.formatter -> string -> unit
end

module Protocol_implementation :
  S
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

  let is_input_state ~is_reveal_enabled:_ state =
    Lwt.return
    @@
    match state.level with
    | None -> PS.Initial
    | Some level -> PS.First_after (level, state.message_counter)

  let set_input input state =
    Lwt.return
    @@
    match input with
    | PS.Inbox_message {inbox_level; message_counter; payload} ->
        {
          payload = Sc_rollup_inbox_message_repr.unsafe_to_string payload;
          level = Some inbox_level;
          message_counter;
          tick = Z.succ state.tick;
        }
    | PS.Reveal _s -> assert false

  let eval state = Lwt.return {state with tick = Z.succ state.tick}

  let verify_proof ~is_reveal_enabled:_ _input = function (_ : proof) -> .

  let produce_proof _context ~is_reveal_enabled:_ _state _step = assert false

  type output_proof = void

  let output_proof_encoding = void

  let output_of_output_proof = function (_ : proof) -> .

  let state_of_output_proof = function (_ : proof) -> .

  let verify_output_proof = function (_ : proof) -> .

  let produce_output_proof _context _state _output = assert false

  let check_dissection ~default_number_of_sections:_ ~start_chunk:_
      ~stop_chunk:_ =
    assert false

  let get_current_level {level; _} = Lwt.return level

  let parse_boot_sector s = Some s

  let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

  module Internal_for_tests = struct
    let insert_failure _state = assert false
  end
end
