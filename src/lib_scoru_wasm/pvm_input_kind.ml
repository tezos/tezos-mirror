(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type protocol = Nairobi | Proto_alpha

(* This type mimics [Sc_rollup_inbox_repr.internal_inbox_messages], without
   fully deserializing the `Transfer`, and is produced by reading the first bytes
   from the input:

   - `\000\000` corresponds to a Transfer,
   - `\000\001` a start_of_level,
   - `\000\002` an end_of_level,
   - `\000\003` an info_per_level input,
   - `\000\004` a protocol migration input,
   - Any other tag will considered as an `Other message`. *)
type internal_message_kind =
  | Transfer
  | Start_of_level
  | End_of_level
  | Info_per_level
  | Protocol_migration of protocol

(* This type mimics [Sc_rollup_inbox_repr.t] and produced by reading the first
   bytes from the input:

   - `\000` corresponds to an internal message,
   - `\001` an external one, and its content includes the tag.
   - Any other tag is considered as an `Other message`. Note that these messages
   are not discarded by the PVM, simply not recognized. *)
type t = Internal of internal_message_kind | External | Other

let protocol_from_raw payload =
  if String.length payload < 3 then None
  else
    let payload = String.sub payload 2 (String.length payload - 2) in
    match Data_encoding.(Binary.of_string_exn string payload) with
    | payload when String.equal payload Constants.proto_alpha_name ->
        Some (Protocol_migration Proto_alpha)
    | payload when String.equal payload Constants.nairobi_name ->
        Some (Protocol_migration Nairobi)
    | _ -> None

let internal_from_raw payload =
  if String.length payload < 2 then None
  else
    match String.get payload 1 with
    | '\000' -> Some Transfer
    | '\001' when String.length payload = 2 -> Some Start_of_level
    | '\002' when String.length payload = 2 -> Some End_of_level
    | '\003' -> Some Info_per_level
    | '\004' -> protocol_from_raw payload
    | _ -> None

let from_raw_input payload =
  if String.length payload < 1 then Other
  else
    match String.get payload 0 with
    | '\000' ->
        Option.fold
          ~none:Other
          ~some:(fun msg -> Internal msg)
          (internal_from_raw payload)
    | '\001' -> External
    | _ -> Other

module Internal_for_tests = struct
  let proto_to_binary = function
    | Nairobi ->
        Data_encoding.(Binary.to_string_exn string Constants.nairobi_name)
    | Proto_alpha ->
        Data_encoding.(Binary.to_string_exn string Constants.proto_alpha_name)

  let to_binary_input input message =
    match (input, message) with
    | Internal Transfer, Some message -> "\000\000" ^ message
    | External, Some message -> "\001" ^ message
    | Internal Start_of_level, None -> "\000\001"
    | Internal End_of_level, None -> "\000\002"
    | Internal Info_per_level, Some info -> "\000\003" ^ info
    | Internal (Protocol_migration proto), None ->
        "\000\004" ^ proto_to_binary proto
    | Other, _ ->
        Stdlib.failwith
          "`Other` messages are impossible cases from the PVM perspective."
    | Internal (Start_of_level | End_of_level), Some _ ->
        Stdlib.failwith
          "`Start_of_level` and `End_of_level` do not expect a payload"
    | Internal Transfer, None -> Stdlib.failwith "`Transfer` expects a payload"
    | Internal Info_per_level, None ->
        Stdlib.failwith "`Info_per_level` expects a payload"
    | Internal (Protocol_migration _), Some _ ->
        Stdlib.failwith "`Protocol_migration` does not expect a payload"
    | External, None -> Stdlib.failwith "`External` expects a payload"
end
