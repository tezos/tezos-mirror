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

open Protocol
open Alpha_context

(* Encoding reading string as Michelson data, of the form "Pair Unit...". It can
   only be used for decoding however, not encoding. *)
let parsed_string_encoding =
  Data_encoding.conv
    (fun _ -> Stdlib.failwith "This value is not supposed to be encoded")
    (fun s ->
      let parsed, _ = Michelson_v1_parser.parse_expression s in
      parsed.Michelson_v1_parser.expanded)
    Data_encoding.string

(* [input_encoding default_sender default_source default_destination] is an
   alternative encoding for {Sc_rollup_inbox_message_repr.t} that only encodes
   `Internal Transfer` and `External`. In the case of `Internal Transfer`, only
   the Micheline payload is mandatory, the other field are taken from the
   default one if they are missing. *)
let input_encoding default_sender default_source default_destination =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Transfer"
        (obj4
           (req "payload" parsed_string_encoding)
           (dft "sender" Contract_hash.encoding default_sender)
           (dft "source" Signature.Public_key_hash.encoding default_source)
           (dft
              "destination"
              Sc_rollup_repr.Address.encoding
              default_destination))
        (function
          | Sc_rollup.Inbox_message.(
              Internal (Transfer {payload; sender; source; destination})) ->
              Some (payload, sender, source, destination)
          | _ -> None)
        (fun (payload, sender, source, destination) ->
          Internal (Transfer {payload; sender; source; destination}));
      case
        (Tag 1)
        ~title:"External"
        (obj1 (req "external" string))
        (function
          | Sc_rollup.Inbox_message.External msg ->
              let (`Hex msg) = Hex.of_string msg in
              Some msg
          | Internal _ -> None)
        (fun msg ->
          External (Hex.to_string (`Hex msg) |> Option.value ~default:""));
    ]

(* Represent a set of inboxes, i.e. a set of set of inputs. The position of an
   inbox in the list represents its level. *)
type inboxes = Sc_rollup.Inbox_message.t list list

let inboxes_encoding default_sender default_source default_destination =
  Data_encoding.(
    list
      (list (input_encoding default_sender default_source default_destination)))

(* [parse_inboxes inputs config] parses an inbox from raw string. *)
let parse_inboxes inputs Config.{sender; source; destination} =
  let open Lwt_result_syntax in
  match Data_encoding.Json.from_string inputs with
  | Ok json ->
      let* full_inputs =
        (fun () ->
          Lwt.return
            (Data_encoding.Json.destruct
               (inboxes_encoding sender source destination)
               json))
        |> Repl_helpers.trap_exn
      in
      List.map_es
        (fun inputs ->
          List.map_es
            (fun input ->
              Protocol.Alpha_context.Sc_rollup.Inbox_message.(
                serialize input
                |> Result.map unsafe_to_string
                |> Environment.wrap_tzresult |> Lwt.return))
            inputs)
        full_inputs
  | Error e -> Error_monad.failwith "%s" e

(* [pp_input ppf bytes] is a pretty printer for Sc_rollup.Inbox messages that
   also handles deserialization from raw bytes. If the message cannot be
   decoded, it prints its hexadecimal representation. *)
let pp_input ppf bytes =
  let pp ppf = function
    | Sc_rollup.Inbox_message.(Internal (Transfer _) as msg) ->
        let json =
          Data_encoding.Json.construct Sc_rollup.Inbox_message.encoding msg
        in
        Data_encoding.Json.pp ppf json
    | Internal Start_of_level -> Format.fprintf ppf "Start_of_level"
    | Internal End_of_level -> Format.fprintf ppf "End_of_level"
    | External msg -> Format.fprintf ppf "%a" Hex.pp (Hex.of_string msg)
  in
  match
    Sc_rollup.Inbox_message.(
      deserialize (unsafe_of_string (Bytes.to_string bytes)))
  with
  | Error _ ->
      Format.fprintf ppf "Unknown encoding: %a" Hex.pp (Hex.of_bytes bytes)
  | Ok msg -> pp ppf msg

let compare_input_buffer_messages i1 i2 =
  let cmp_lvl =
    Int32.compare
      i1.Tezos_webassembly_interpreter.Input_buffer.raw_level
      i2.Tezos_webassembly_interpreter.Input_buffer.raw_level
  in
  if cmp_lvl = 0 then Z.compare i1.message_counter i2.message_counter
  else cmp_lvl
