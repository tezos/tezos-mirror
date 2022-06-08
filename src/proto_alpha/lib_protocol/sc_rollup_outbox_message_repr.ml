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
  | (* `Permanent *) Error_encode_outbox_message
  | (* `Permanent *) Error_decode_outbox_message

let () =
  let open Data_encoding in
  let msg =
    "Failed to encode a rollup management protocol outbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_outbox_message_repr.error_encoding_outbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_encode_outbox_message -> Some () | _ -> None)
    (fun () -> Error_encode_outbox_message) ;
  let msg =
    "Failed to decode a rollup management protocol outbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_outbox_message_repr.error_decoding_outbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_decode_outbox_message -> Some () | _ -> None)
    (fun () -> Error_decode_outbox_message)

type transaction = {
  unparsed_parameters : Script_repr.expr;  (** The payload. *)
  destination : Contract_hash.t;  (** The recipient contract. *)
  entrypoint : Entrypoint_repr.t;  (** Entrypoint of the destination. *)
}

type t = Atomic_transaction_batch of {transactions : transaction list}

let transaction_encoding =
  let open Data_encoding in
  (* TODO: #3116
     Add size limit to constrain the maximum size of the encoded message.
     The exact limit is yet to be decided. Could be added as a constant.
  *)
  conv
    (fun {unparsed_parameters; destination; entrypoint} ->
      (unparsed_parameters, destination, entrypoint))
    (fun (unparsed_parameters, destination, entrypoint) ->
      {unparsed_parameters; destination; entrypoint})
  @@ obj3
       (req "parameters" Script_repr.expr_encoding)
       (req "destination" Contract_repr.originated_encoding)
       (req "entrypoint" Entrypoint_repr.simple_encoding)

let encoding =
  let open Data_encoding in
  conv
    (fun (Atomic_transaction_batch {transactions}) -> transactions)
    (fun transactions -> Atomic_transaction_batch {transactions})
    (obj1 (req "transactions" (list transaction_encoding)))

let pp_transaction fmt {destination; entrypoint; unparsed_parameters = _} =
  Format.fprintf
    fmt
    "@[%a@;%a@]"
    Contract_hash.pp
    destination
    Entrypoint_repr.pp
    entrypoint

let pp fmt (Atomic_transaction_batch {transactions}) =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_space
    pp_transaction
    fmt
    transactions

let of_bytes bytes =
  let open Tzresult_syntax in
  match Data_encoding.Binary.of_string_opt encoding bytes with
  | Some x -> return x
  | None -> fail Error_decode_outbox_message

module Internal_for_tests = struct
  let to_bytes outbox_message =
    let open Tzresult_syntax in
    match Data_encoding.Binary.to_string_opt encoding outbox_message with
    | Some str -> return str
    | None -> fail Error_encode_outbox_message
end
