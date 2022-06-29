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

type internal_inbox_message = {
  payload : Script_repr.expr;
  sender : Contract_hash.t;
  source : Signature.public_key_hash;
}

type t = Internal of internal_inbox_message | External of string

let encoding =
  let open Data_encoding in
  Data_encoding.union
    [
      case
        (Tag 0)
        ~title:"Internal"
        (obj3
           (req "payload" Script_repr.expr_encoding)
           (req "sender" Contract_hash.encoding)
           (req "source" Signature.Public_key_hash.encoding))
        (function
          | Internal {payload; sender; source} -> Some (payload, sender, source)
          | External _ -> None)
        (fun (payload, sender, source) -> Internal {payload; sender; source});
      case
        (Tag 1)
        ~title:"External"
        (* TODO: #3116
           Add size limit to constrain the maximum size of the string.
           The exact limit is yet to be decided. Could be added as a constant.
        *)
        string
        (function External msg -> Some msg | Internal _ -> None)
        (fun msg -> External msg);
    ]

type serialized = string

let to_bytes msg =
  let open Tzresult_syntax in
  match Data_encoding.Binary.to_string_opt encoding msg with
  | None -> fail Error_encode_inbox_message
  | Some str -> return str

module Internal_for_tests = struct
  let of_bytes bytes =
    let open Tzresult_syntax in
    match Data_encoding.Binary.of_string_opt encoding bytes with
    | None -> fail Error_decode_inbox_message
    | Some msg -> return msg
end
