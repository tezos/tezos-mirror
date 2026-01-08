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
    ~id:"smart_rollup_management_protocol_invalid_destination"
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

type outbox_message =
  | Atomic_transaction_batch of atomic_transaction_batch
  | Whitelist_update of Sc_rollup.Whitelist.t option

let make_internal_transfer ctxt ty ~payload ~sender ~source ~destination =
  let open Lwt_result_syntax in
  let+ payload, ctxt =
    Script_ir_translator.unparse_data
      ctxt
      Script_ir_unparser.Optimized
      ty
      payload
  in
  ( Sc_rollup.Inbox_message.Internal
      (Transfer {payload; sender; source; destination}),
    ctxt )

let make_transaction ctxt ~parameters_ty ~unparsed_parameters ~destination
    ~entrypoint =
  let open Lwt_result_syntax in
  (* Parse the parameters according to the given type. *)
  let+ parameters, ctxt =
    Script_ir_translator.parse_data
      ctxt
      ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
      ~allow_forged_tickets:true
      ~allow_forged_lazy_storage_id:false
      parameters_ty
      (Micheline.root unparsed_parameters)
  in
  ( ctxt,
    Transaction
      {destination; entrypoint; parameters_ty; parameters; unparsed_parameters}
  )

let internal_untyped_transaction ctxt
    ({unparsed_parameters; destination; entrypoint} :
      Sc_rollup.Outbox.Message.transaction) =
  let open Lwt_result_syntax in
  let* Script_ir_translator.Ex_script (Script {arg_type; entrypoints; _}), ctxt
      =
    let* ctxt, _cache_key, cached = Script_cache.find ctxt destination in
    match cached with
    | Some (_script, ex_script) -> return (ex_script, ctxt)
    | None -> tzfail Sc_rollup_invalid_destination
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
  make_transaction
    ctxt
    ~parameters_ty
    ~unparsed_parameters
    ~destination
    ~entrypoint

let internal_typed_transaction ctxt
    ({unparsed_parameters; unparsed_ty; destination; entrypoint} :
      Sc_rollup.Outbox.Message.typed_transaction) =
  let open Lwt_result_syntax in
  (* Parse the parameters type according to the type. *)
  let*? Ex_ty parameters_ty, ctxt =
    Script_ir_translator.parse_any_ty
      ctxt
      ~legacy:false
      (Micheline.root unparsed_ty)
  in
  make_transaction
    ctxt
    ~parameters_ty
    ~unparsed_parameters
    ~destination
    ~entrypoint

let outbox_message_of_outbox_message_repr ctxt transactions =
  let open Lwt_result_syntax in
  match transactions with
  | Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions} ->
      let* ctxt, transactions =
        List.fold_left_map_es internal_untyped_transaction ctxt transactions
      in
      return (Atomic_transaction_batch {transactions}, ctxt)
  | Sc_rollup.Outbox.Message.Atomic_transaction_batch_typed {transactions} ->
      let* ctxt, transactions =
        List.fold_left_map_es internal_typed_transaction ctxt transactions
      in
      return (Atomic_transaction_batch {transactions}, ctxt)
  | Sc_rollup.Outbox.Message.Whitelist_update whitelist_opt ->
      return (Whitelist_update whitelist_opt, ctxt)

module Internal_for_tests = struct
  let make_transaction ctxt parameters_ty ~parameters ~destination ~entrypoint =
    let open Lwt_result_syntax in
    let* unparsed_parameters, ctxt =
      Script_ir_translator.unparse_data ctxt Optimized parameters_ty parameters
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

  let make_atomic_batch transactions = Atomic_transaction_batch {transactions}

  let serialize_outbox_transactions_untyped transactions =
    let open Result_syntax in
    let of_internal_transaction
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
    let* transactions = List.map_e of_internal_transaction transactions in
    let output_message_internal =
      Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions}
    in
    Sc_rollup.Outbox.Message.serialize output_message_internal

  let serialize_outbox_transactions_typed transactions =
    let open Result_syntax in
    let of_internal_transaction
        (Transaction
           {
             destination;
             entrypoint;
             parameters_ty;
             parameters = _;
             unparsed_parameters;
           }) =
      let unparsed_ty =
        Script_ir_unparser.serialize_ty_for_error parameters_ty
      in
      return
        {
          Sc_rollup.Outbox.Message.unparsed_parameters;
          unparsed_ty;
          destination;
          entrypoint;
        }
    in
    let* transactions = List.map_e of_internal_transaction transactions in
    let output_message_internal =
      Sc_rollup.Outbox.Message.Atomic_transaction_batch_typed {transactions}
    in
    Sc_rollup.Outbox.Message.serialize output_message_internal

  let deserialize_inbox_message = Sc_rollup.Inbox_message.deserialize
end
