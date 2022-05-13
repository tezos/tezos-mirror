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
  | (* `Permanent *) Error_encode_outbox_message
  | (* `Permanent *) Error_decode_outbox_message

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
    (fun () -> Error_decode_inbox_message) ;
  let msg =
    "Failed to encode a rollup management protocol outbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"rollup_management_protocol.error_encoding_outbox_message"
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
    ~id:"rollup_management_protocol.error_decoding_outbox_message"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_decode_outbox_message -> Some () | _ -> None)
    (fun () -> Error_decode_outbox_message)

type sc_message = {
  payload : Script_repr.expr;
      (** A Micheline value containing the parameters passed to the rollup. *)
  sender : Alpha_context.Contract.t;  (** The L1 caller contract. *)
  source : Signature.public_key_hash;
      (** The implicit account that originated the transaction. *)
}

type inbox_message = Sc_message of sc_message

type transaction_internal = {
  unparsed_parameters_ty : Script_repr.expr;  (** The type of the parameters. *)
  unparsed_parameters : Script_repr.expr;  (** The payload. *)
  destination : Destination.t;  (** The recipient contract or rollup. *)
  entrypoint : Entrypoint.t;  (** Entrypoint of the destination. *)
}

type atomic_message_batch_internal = {
  transactions_internal : transaction_internal list;
}

type outbox_message_internal =
  | Atomic_transaction_batch_internal of atomic_message_batch_internal

type transaction =
  | Transaction : {
      destination : Destination.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) Script_typed_ir.ty;
      parameters : 'a;
      unparsed_parameters_ty : Script.expr;
      unparsed_parameters : Script.expr;
    }
      -> transaction

type atomic_transaction_batch = {transactions : transaction list}

type outbox_message = Atomic_transaction_batch of atomic_transaction_batch

let make_inbox_message ctxt ty ~payload ~sender ~source =
  let open Lwt_tzresult_syntax in
  let+ payload, ctxt =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_translator.Optimized
      ty
      payload
  in
  let payload = Micheline.strip_locations payload in
  (Sc_message {payload; sender; source}, ctxt)

let sc_message_encoding =
  let open Data_encoding in
  conv
    (fun {payload; sender; source} -> (payload, sender, source))
    (fun (payload, sender, source) -> {payload; sender; source})
  @@ obj3
       (req "payload" Script_repr.expr_encoding)
       (req "sender" Contract.encoding)
       (req "source" Signature.Public_key_hash.encoding)

let transaction_internal_encoding =
  let open Data_encoding in
  conv
    (fun {unparsed_parameters_ty; unparsed_parameters; destination; entrypoint} ->
      (unparsed_parameters_ty, unparsed_parameters, destination, entrypoint))
    (fun (unparsed_parameters_ty, unparsed_parameters, destination, entrypoint) ->
      {unparsed_parameters_ty; unparsed_parameters; destination; entrypoint})
  @@ obj4
       (req "parameters_ty" Script_repr.expr_encoding)
       (req "parameters" Script_repr.expr_encoding)
       (req "destination" Destination.encoding)
       (req "entrypoint" Entrypoint.simple_encoding)

let atomic_message_batch_encoding =
  let open Data_encoding in
  obj1
    (req
       "transactions"
       (conv
          (fun {transactions_internal} -> transactions_internal)
          (fun transactions_internal -> {transactions_internal})
          (list transaction_internal_encoding)))

let internal_outbox_message_encoding =
  let open Data_encoding in
  conv
    (fun (Atomic_transaction_batch_internal m) -> m)
    (fun m -> Atomic_transaction_batch_internal m)
    atomic_message_batch_encoding

let inbox_message_encoding =
  let open Data_encoding in
  conv (fun (Sc_message m) -> m) (fun m -> Sc_message m) sc_message_encoding

(** TODO: #2951
    Carbonate [to_bytes] step.
    Gas for encoding the value in binary format should be accounted for.
  *)
let bytes_of_inbox_message msg =
  let open Tzresult_syntax in
  match Data_encoding.Binary.to_bytes_opt inbox_message_encoding msg with
  | None -> fail Error_encode_inbox_message
  | Some bs -> return bs

let transactions_batch_of_internal ctxt {transactions_internal} =
  let open Lwt_tzresult_syntax in
  let or_internal_transaction ctxt
      {unparsed_parameters_ty; unparsed_parameters; destination; entrypoint} =
    let*? Ex_ty parameters_ty, ctxt =
      Script_ir_translator.parse_ty
        ~legacy:false
        ~allow_lazy_storage:false
        ~allow_contract:false
        ~allow_ticket:true
        ~allow_operation:false
        ctxt
        (Micheline.root unparsed_parameters_ty)
    in
    (* TODO: #2964
       We should rule out big-maps.
       [allow_forged] controls both tickets and big-maps. Here we only want to
       allow tickets. *)
    let* parameters, ctxt =
      Script_ir_translator.parse_data
        ctxt
        ~legacy:false
        ~allow_forged:true
        parameters_ty
        (Micheline.root unparsed_parameters)
    in
    return
      ( Transaction
          {
            destination;
            entrypoint;
            parameters_ty;
            parameters;
            unparsed_parameters_ty;
            unparsed_parameters;
          },
        ctxt )
  in
  let+ ctxt, transactions =
    List.fold_left_map_es
      (fun ctxt msg ->
        let+ t, ctxt = or_internal_transaction ctxt msg in
        (ctxt, t))
      ctxt
      transactions_internal
  in
  ({transactions}, ctxt)

(** TODO: #2951
    Carbonate [of_bytes] step.
    Gas for decoding the binary values should be be accounted for.
  *)
let outbox_message_of_bytes ctxt bytes =
  let open Lwt_tzresult_syntax in
  let*? (Atomic_transaction_batch_internal msg) =
    match
      Data_encoding.Binary.of_bytes_opt internal_outbox_message_encoding bytes
    with
    | Some x -> ok x
    | None -> error Error_decode_inbox_message
  in
  let+ ts, ctxt = transactions_batch_of_internal ctxt msg in
  (Atomic_transaction_batch ts, ctxt)

module Internal_for_tests = struct
  let make_transaction ctxt parameters_ty ~parameters ~destination ~entrypoint =
    let open Lwt_tzresult_syntax in
    let* unparsed_parameters, ctxt =
      Script_ir_translator.unparse_data ctxt Optimized parameters_ty parameters
    in
    let*? unparsed_parameters_ty, ctxt =
      Script_ir_translator.unparse_ty
        ctxt
        ~loc:Micheline.dummy_location
        parameters_ty
    in
    let*? ctxt =
      Gas.consume ctxt (Script.strip_locations_cost unparsed_parameters)
    in
    let unparsed_parameters = Micheline.strip_locations unparsed_parameters in
    let*? ctxt =
      Gas.consume ctxt (Script.strip_locations_cost unparsed_parameters_ty)
    in
    let unparsed_parameters_ty =
      Micheline.strip_locations unparsed_parameters_ty
    in
    return
      ( Transaction
          {
            destination;
            entrypoint;
            parameters_ty;
            parameters;
            unparsed_parameters_ty;
            unparsed_parameters;
          },
        ctxt )

  let make_atomic_batch transactions = Atomic_transaction_batch {transactions}

  let bytes_of_outbox_message (Atomic_transaction_batch {transactions}) =
    let open Tzresult_syntax in
    let to_internal_transaction
        (Transaction
          {
            destination;
            entrypoint;
            parameters_ty = _;
            parameters = _;
            unparsed_parameters_ty;
            unparsed_parameters;
          }) =
      {unparsed_parameters; unparsed_parameters_ty; destination; entrypoint}
    in
    let output_message_internal =
      Atomic_transaction_batch_internal
        {transactions_internal = List.map to_internal_transaction transactions}
    in
    match
      Data_encoding.Binary.to_bytes_opt
        internal_outbox_message_encoding
        output_message_internal
    with
    | Some x -> return x
    | None -> fail Error_encode_inbox_message

  let inbox_message_of_bytes bytes =
    let open Tzresult_syntax in
    match Data_encoding.Binary.of_bytes_opt inbox_message_encoding bytes with
    | None -> fail Error_decode_inbox_message
    | Some deposit -> return deposit
end
