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

open Alpha_context

type error += Zk_rollup_feature_disabled | Zk_rollup_negative_nb_ops

let () =
  let description = "ZK rollups will be enabled in a future proposal." in
  register_error_kind
    `Permanent
    ~id:"operation.zk_rollup_disabled"
    ~title:"ZK rollups are disabled"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Zk_rollup_feature_disabled -> Some () | _ -> None)
    (fun () -> Zk_rollup_feature_disabled) ;
  let description = "The value of [nb_ops] should never be negative." in
  register_error_kind
    `Permanent
    ~id:"operation.zk_rollup_negative_nb_ops"
    ~title:"ZK rollups negative number of operations"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Zk_rollup_negative_nb_ops -> Some () | _ -> None)
    (fun () -> Zk_rollup_negative_nb_ops)

let assert_feature_enabled ctxt =
  error_unless (Constants.zk_rollup_enable ctxt) Zk_rollup_feature_disabled

let originate ~ctxt_before_op ~ctxt ~public_parameters ~circuits_info
    ~init_state ~nb_ops =
  let open Lwt_result_syntax in
  let*? () = assert_feature_enabled ctxt in
  let*? () = error_when Compare.Int.(nb_ops < 0) Zk_rollup_negative_nb_ops in
  let+ ctxt, originated_zk_rollup, storage_size =
    Zk_rollup.originate
      ctxt
      {
        public_parameters;
        state_length = Array.length init_state;
        circuits_info;
        nb_ops;
      }
      ~init_state
  in
  let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
  let result =
    Apply_results.Zk_rollup_origination_result
      {
        balance_updates = [];
        originated_zk_rollup;
        (* TODO https://gitlab.com/tezos/tezos/-/issues/3544
           Carbonate ZKRU operations *)
        consumed_gas;
        storage_size;
      }
  in
  (ctxt, result, [])

(** [parse_ticket ~ticketer ~contents ~ty ctxt] reconstructs a ticket from
    individual parts submitted as part of a Zk_rollup_publish operation. *)
let parse_ticket ~ticketer ~contents ~ty ctxt =
  Script_ir_translator.parse_comparable_ty ctxt (Micheline.root ty)
  >>?= fun (Ex_comparable_ty contents_type, ctxt) ->
  Script_ir_translator.parse_comparable_data
    ctxt
    contents_type
    (Micheline.root contents)
  >>=? fun (contents, ctxt) ->
  return @@ (ctxt, Ticket_token.Ex_token {ticketer; contents_type; contents})

let publish ~ctxt_before_op ~ctxt ~zk_rollup ~l2_ops =
  let open Lwt_result_syntax in
  let*? () = assert_feature_enabled ctxt in

  let open Zk_rollup.Operation in
  (* Deposits (i.e. L2 operations with a positive price) cannot be published
     through an external operation *)
  let*? () =
    error_unless
      (List.for_all
         (fun (l2_op, _ticket_opt) -> Compare.Z.(l2_op.price.amount <= Z.zero))
         l2_ops)
      Zk_rollup.Errors.Deposit_as_external
  in
  (* Check that for every operation to publish:
      1. Their price is zero iff they have no ticket representation
      2. The "token id" of its price is the correct ticket hash
     Additionally, for operations with tickets, the hash of the ticket
     with the l1 destination from the operation's header is computed.
  *)
  let* ctxt, l2_ops_with_ticket_hashes =
    List.fold_left_map_es
      (fun ctxt (l2_op, ticket_opt) ->
        match ticket_opt with
        | None ->
            let*? () =
              error_unless
                Compare.Z.(l2_op.price.amount = Z.zero)
                Zk_rollup.Errors.Invalid_deposit_amount
            in
            return (ctxt, (l2_op, None))
        | Some Zk_rollup.Ticket.{ticketer; ty; contents} ->
            let*? () =
              error_when
                Compare.Z.(l2_op.price.amount = Z.zero)
                Zk_rollup.Errors.Invalid_deposit_amount
            in
            let* ctxt, ticket_token =
              parse_ticket ~ticketer ~contents ~ty ctxt
            in
            (* Compute the ticket hash with L1 address to be able
               to perform an exit / return token *)
            let* receiver_ticket_hash, ctxt =
              Ticket_balance_key.of_ex_token
                ctxt
                ~owner:(Contract (Implicit l2_op.l1_dst))
                ticket_token
            in
            (* Compute the ticket with zk rollup as owner, this is the hash
               that is used as token identifier inside the ZKRU (and this
               should be price's identifier in this L2 op) *)
            let* source_ticket_hash, ctxt =
              Ticket_balance_key.of_ex_token
                ctxt
                ~owner:(Zk_rollup zk_rollup)
                ticket_token
            in
            let*? () =
              error_unless
                Ticket_hash.(equal l2_op.price.id source_ticket_hash)
                Zk_rollup.Errors.Invalid_deposit_ticket
            in
            return (ctxt, (l2_op, Some receiver_ticket_hash)))
      ctxt
      l2_ops
  in
  let+ ctxt, paid_storage_size_diff =
    Zk_rollup.add_to_pending ctxt zk_rollup l2_ops_with_ticket_hashes
  in
  (* TODO https://gitlab.com/tezos/tezos/-/issues/3544
     Carbonate ZKRU operations *)
  let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
  let result =
    Apply_results.Zk_rollup_publish_result
      {balance_updates = []; consumed_gas; paid_storage_size_diff}
  in
  (ctxt, result, [])

let transaction_to_zk_rollup ~ctxt ~parameters_ty ~parameters ~dst_rollup ~since
    =
  let open Lwt_result_syntax in
  let*? () = assert_feature_enabled ctxt in
  let*? {ex_ticket; zkru_operation} =
    Zk_rollup_parameters.get_deposit_parameters parameters_ty parameters
  in
  let* ticket_size, ctxt = Ticket_scanner.ex_ticket_size ctxt ex_ticket in
  let limit = Constants.tx_rollup_max_ticket_payload_size ctxt in
  let*? () =
    error_when
      Compare.Int.(ticket_size > limit)
      (Zk_rollup.Errors.Ticket_payload_size_limit_exceeded
         {payload_size = ticket_size; limit})
  in
  let ex_token, ticket_amount =
    Ticket_token.token_and_amount_of_ex_ticket ex_ticket
  in
  (* Compute the ticket hash with zk rollup as owner *)
  let* ticket_hash, ctxt =
    Ticket_balance_key.of_ex_token ctxt ~owner:(Zk_rollup dst_rollup) ex_token
  in
  let ticket_amount = Script_int.(to_zint (ticket_amount :> n num)) in
  (* Check that the amount and id of the transferred ticket are what
     the operation's price claims. *)
  let*? () =
    error_unless
      Compare.Z.(ticket_amount = zkru_operation.price.amount)
      Zk_rollup.Errors.Invalid_deposit_amount
  in
  let*? () =
    error_unless
      Ticket_hash.(equal ticket_hash zkru_operation.price.id)
      Zk_rollup.Errors.Invalid_deposit_ticket
  in
  (* Compute the ticket hash with L1 address to be able
     to perform an exit / return token *)
  let* receiver_ticket_hash, ctxt =
    Ticket_balance_key.of_ex_token
      ctxt
      ~owner:(Contract (Implicit zkru_operation.l1_dst))
      ex_token
  in
  (* Add it to the rollup pending list *)
  let+ ctxt, paid_storage_size_diff =
    Zk_rollup.add_to_pending
      ctxt
      Zk_rollup.Operation.(zkru_operation.rollup_id)
      [(zkru_operation, Some receiver_ticket_hash)]
  in
  (* TODO https://gitlab.com/tezos/tezos/-/issues/3544
     Carbonate ZKRU operations *)
  let result =
    Apply_internal_results.(
      ITransaction_result
        (Transaction_to_zk_rollup_result
           {
             balance_updates = [];
             consumed_gas = Gas.consumed ~since ~until:ctxt;
             ticket_hash;
             paid_storage_size_diff;
           }))
  in
  (ctxt, result, [])
