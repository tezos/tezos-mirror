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

type error += (* Permanent *) Sc_rollup_invalid_destination

let () =
  let open Data_encoding in
  let msg = "Invalid destination" in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_management_protocol.sc_rollup_invalid_destination"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Sc_rollup_invalid_destination -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_destination)

type transaction =
  | Transaction : {
      destination : Contract_hash.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) Script_typed_ir.ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> transaction

type atomic_transaction_batch = {transactions : transaction list}

type outbox_message = Atomic_transaction_batch of atomic_transaction_batch

let make_internal_inbox_message ctxt ty ~payload ~sender ~source =
  let open Lwt_tzresult_syntax in
  let+ payload, ctxt =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_translator.Optimized
      ty
      payload
  in
  let payload = Micheline.strip_locations payload in
  (Sc_rollup.Inbox.Message.Internal {payload; sender; source}, ctxt)

let transactions_batch_of_internal ctxt transactions =
  let open Lwt_tzresult_syntax in
  let or_internal_transaction ctxt
      {Sc_rollup.Outbox.Message.unparsed_parameters; destination; entrypoint} =
    (* Lookup the contract-hash. *)
    (* Load the type and entrypoints of the script. *)
    let* ( Script_ir_translator.Ex_script (Script {arg_type; entrypoints; _}),
           ctxt ) =
      let* ctxt, _cache_key, cached = Script_cache.find ctxt destination in
      match cached with
      | Some (_script, ex_script) -> return (ex_script, ctxt)
      | None -> fail Sc_rollup_invalid_destination
    in
    (* Find the entrypoint type for the given entrypoint. *)
    let*? res, ctxt =
      Gas_monad.run
        ctxt
        (Script_ir_translator.find_entrypoint
           ~error_details:(Informative ())
           arg_type
           entrypoints
           entrypoint)
    in
    let*? (Ex_ty_cstr {ty = parameters_ty; _}) = res in
    (* Parse the parameters according to the entrypoint type. *)
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
      transactions
  in
  ({transactions}, ctxt)

(** TODO: #2951
    Carbonate [of_bytes] step.
    Gas for decoding the binary values should be be accounted for.
  *)
let outbox_message_of_bytes ctxt bytes =
  let open Lwt_tzresult_syntax in
  let*? (Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions}) =
    Sc_rollup.Outbox.Message.of_bytes bytes
  in
  let+ ts, ctxt = transactions_batch_of_internal ctxt transactions in
  (Atomic_transaction_batch ts, ctxt)

module Internal_for_tests = struct
  let make_transaction ctxt parameters_ty ~parameters ~destination ~entrypoint =
    let open Lwt_tzresult_syntax in
    let* unparsed_parameters, ctxt =
      Script_ir_translator.unparse_data ctxt Optimized parameters_ty parameters
    in
    let unparsed_parameters = Micheline.strip_locations unparsed_parameters in
    return
      ( Transaction
          {
            destination;
            entrypoint;
            parameters_ty;
            parameters;
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
            unparsed_parameters;
          }) =
      return
        {Sc_rollup.Outbox.Message.unparsed_parameters; destination; entrypoint}
    in
    let* transactions = List.map_e to_internal_transaction transactions in
    let output_message_internal =
      Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions}
    in
    Sc_rollup.Outbox.Message.Internal_for_tests.to_bytes output_message_internal

  let inbox_message_of_bytes =
    Sc_rollup.Inbox.Message.Internal_for_tests.of_bytes
end
