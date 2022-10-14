(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type error +=
  | (* `Permanent *) Error_encode_inbox_message
  | (* `Permanent *) Error_decode_inbox_message

let () =
  let open Data_encoding in
  let msg =
    "Failed to encode a rollup management protocol inbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox_message_repr.error_encoding_inbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_encode_inbox_message -> Some () | _ -> None)
    (fun () -> Error_encode_inbox_message) ;
  let msg =
    "Failed to decode a rollup management protocol inbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox_message_repr.error_decoding_inbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_decode_inbox_message -> Some () | _ -> None)
    (fun () -> Error_decode_inbox_message)

type internal_inbox_message =
  | Transfer of {
      payload : Script_repr.expr;
      sender : Contract_hash.t;
      source : Signature.public_key_hash;
      destination : Sc_rollup_repr.Address.t;
    }
  | Start_of_level
  | End_of_level

let internal_inbox_message_encoding =
  let open Data_encoding in
  let kind name = req "internal_inbox_message_kind" (constant name) in
  union
    [
      case
        (Tag 0)
        ~title:"Transfer"
        (obj5
           (kind "transfer")
           (req "payload" Script_repr.expr_encoding)
           (req "sender" Contract_hash.encoding)
           (req "source" Signature.Public_key_hash.encoding)
           (req "destination" Sc_rollup_repr.Address.encoding))
        (function
          | Transfer {payload; sender; source; destination} ->
              Some ((), payload, sender, source, destination)
          | _ -> None)
        (fun ((), payload, sender, source, destination) ->
          Transfer {payload; sender; source; destination});
      case
        (Tag 1)
        ~title:"Start_of_level"
        (obj1 (kind "start_of_level"))
        (function Start_of_level -> Some () | _ -> None)
        (fun () -> Start_of_level);
      case
        (Tag 2)
        ~title:"End_of_level"
        (obj1 (kind "end_of_level"))
        (function End_of_level -> Some () | _ -> None)
        (fun () -> End_of_level);
    ]

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4027
   We should change the payload of [External] from [bytes] to [string]. *)

type t = Internal of internal_inbox_message | External of string

let encoding =
  let open Data_encoding in
  check_size
    Constants_repr.sc_rollup_message_size_limit
    (union
       [
         case
           (Tag 0)
           ~title:"Internal"
           internal_inbox_message_encoding
           (function
             | Internal internal_message -> Some internal_message
             | External _ -> None)
           (fun internal_message -> Internal internal_message);
         case
           (Tag 1)
           ~title:"External"
           Variable.string
           (function External msg -> Some msg | Internal _ -> None)
           (fun msg -> External msg);
       ])

type serialized = string

let serialize msg =
  let open Tzresult_syntax in
  match Data_encoding.Binary.to_string_opt encoding msg with
  | None -> fail Error_encode_inbox_message
  | Some str -> return str

let deserialize s =
  let open Tzresult_syntax in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> fail Error_decode_inbox_message
  | Some msg -> return msg

let unsafe_of_string s = s

let unsafe_to_string s = s
