(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type input = {
  inbox_level : Raw_level_repr.t;
  message_counter : Z.t;
  payload : Sc_rollup_inbox_message_repr.serialized;
}

let input_encoding =
  let open Data_encoding in
  conv
    (fun {inbox_level; message_counter; payload} ->
      (inbox_level, message_counter, (payload :> string)))
    (fun (inbox_level, message_counter, payload) ->
      let payload = Sc_rollup_inbox_message_repr.unsafe_of_string payload in
      {inbox_level; message_counter; payload})
    (obj3
       (req "inbox_level" Raw_level_repr.encoding)
       (req "message_counter" n)
       (req "payload" string))

let input_equal (a : input) (b : input) : bool =
  let {inbox_level; message_counter; payload} = a in
  (* To be robust to the addition of fields in [input] *)
  Raw_level_repr.equal inbox_level b.inbox_level
  && Z.equal message_counter b.message_counter
  && String.equal (payload :> string) (b.payload :> string)

type input_request =
  | No_input_required
  | Initial
  | First_after of Raw_level_repr.t * Z.t

let input_request_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"No_input_required"
        (Tag 0)
        (obj1 (req "no_input_required" unit))
        (function No_input_required -> Some () | _ -> None)
        (fun () -> No_input_required);
      case
        ~title:"Initial"
        (Tag 1)
        (obj1 (req "initial" unit))
        (function Initial -> Some () | _ -> None)
        (fun () -> Initial);
      case
        ~title:"First_after"
        (Tag 2)
        (obj2 (req "level" Raw_level_repr.encoding) (req "counter" n))
        (function
          | First_after (level, counter) -> Some (level, counter) | _ -> None)
        (fun (level, counter) -> First_after (level, counter));
    ]

let pp_input_request fmt request =
  match request with
  | No_input_required -> Format.fprintf fmt "No_input_required"
  | Initial -> Format.fprintf fmt "Initial"
  | First_after (l, n) ->
      Format.fprintf
        fmt
        "First_after (level = %a, counter = %a)"
        Raw_level_repr.pp
        l
        Z.pp_print
        n

let input_request_equal a b =
  match (a, b) with
  | No_input_required, No_input_required -> true
  | No_input_required, _ -> false
  | Initial, Initial -> true
  | Initial, _ -> false
  | First_after (l, n), First_after (m, o) ->
      Raw_level_repr.equal l m && Z.equal n o
  | First_after _, _ -> false

type output = {
  outbox_level : Raw_level_repr.t;
  message_index : Z.t;
  message : Sc_rollup_outbox_message_repr.t;
}

let output_encoding =
  let open Data_encoding in
  conv
    (fun {outbox_level; message_index; message} ->
      (outbox_level, message_index, message))
    (fun (outbox_level, message_index, message) ->
      {outbox_level; message_index; message})
    (obj3
       (req "outbox_level" Raw_level_repr.encoding)
       (req "message_index" n)
       (req "message" Sc_rollup_outbox_message_repr.encoding))

let pp_output fmt {outbox_level; message_index; message} =
  Format.fprintf
    fmt
    "@[%a@;%a@;%a@;@]"
    Raw_level_repr.pp
    outbox_level
    Z.pp_print
    message_index
    Sc_rollup_outbox_message_repr.pp
    message

module type S = sig
  type state

  val pp : state -> (Format.formatter -> unit -> unit) Lwt.t

  type context

  type hash = Sc_rollup_repr.State_hash.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> hash

  val proof_stop_state : proof -> hash option

  val proof_input_requested : proof -> input_request

  val proof_input_given : proof -> input option

  val state_hash : state -> hash Lwt.t

  val initial_state : context -> state Lwt.t

  val install_boot_sector : state -> string -> state Lwt.t

  val is_input_state : state -> input_request Lwt.t

  val set_input : input -> state -> state Lwt.t

  val eval : state -> state Lwt.t

  val verify_proof : proof -> bool Lwt.t

  val produce_proof :
    context -> input option -> state -> (proof, error) result Lwt.t

  val verify_origination_proof : proof -> string -> bool Lwt.t

  val produce_origination_proof :
    context -> string -> (proof, error) result Lwt.t

  type output_proof

  val output_proof_encoding : output_proof Data_encoding.t

  val output_of_output_proof : output_proof -> output

  val state_of_output_proof : output_proof -> hash

  val verify_output_proof : output_proof -> bool Lwt.t

  val produce_output_proof :
    context -> state -> output -> (output_proof, error) result Lwt.t

  module Internal_for_tests : sig
    val insert_failure : state -> state Lwt.t
  end
end
