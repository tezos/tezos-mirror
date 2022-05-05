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

open Alpha_context

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
    ~id:"rollup_management_protocol.error_encoding_inbox_message"
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
    ~id:"rollup_management_protocol.error_decoding_inbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_decode_inbox_message -> Some () | _ -> None)
    (fun () -> Error_decode_inbox_message)

type sc_message = {
  payload : Script_repr.expr;
      (** A Micheline value containing the parameters passed to the rollup. *)
  sender : Alpha_context.Contract.t;  (** The L1 caller contract. *)
  source : Signature.public_key_hash;
      (** The implicit account that originated the transaction. *)
}

type inbox_message = Sc_message of sc_message

let make_inbox_message ctxt ty ~payload ~sender ~source =
  let open Lwt_tzresult_syntax in
  let+ (payload, ctxt) =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_translator.Optimized
      ty
      payload
  in
  let payload = Micheline.strip_locations payload in
  (Sc_message {payload; sender; source}, ctxt)

let sc_inbox_message_encoding =
  let open Data_encoding in
  conv
    (fun {payload; sender; source} -> (payload, sender, source))
    (fun (payload, sender, source) -> {payload; sender; source})
  @@ obj3
       (req "payload" Script_repr.expr_encoding)
       (req "sender" Contract.encoding)
       (req "source" Signature.Public_key_hash.encoding)

let inbox_message_encoding =
  let open Data_encoding in
  conv
    (fun (Sc_message m) -> m)
    (fun m -> Sc_message m)
    sc_inbox_message_encoding

let bytes_of_inbox_message msg =
  let open Tzresult_syntax in
  match Data_encoding.Binary.to_bytes_opt inbox_message_encoding msg with
  | None -> fail Error_encode_inbox_message
  | Some bs -> return bs

let inbox_message_of_bytes bytes =
  let open Tzresult_syntax in
  match Data_encoding.Binary.of_bytes_opt inbox_message_encoding bytes with
  | None -> fail Error_decode_inbox_message
  | Some deposit -> return deposit
