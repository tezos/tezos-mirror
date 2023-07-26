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
  let limit = Constants.zk_rollup_max_ticket_payload_size ctxt in
  let*? () =
    error_when
      Saturation_repr.(ticket_size >! limit)
      (Zk_rollup.Errors.Ticket_payload_size_limit_exceeded
         {payload_size = ticket_size; limit})
  in
  let ex_token, ticket_amount =
    Ticket_scanner.ex_token_and_amount_of_ex_ticket ex_ticket
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

(*
   A ZKRU Update will set a new ZKRU state if the proof sent in the payload
   is verified. In order to verify this proof, the protocol needs to
   compute the "public inputs" expected by the Plonk circuits that define
   a given ZKRU.
   The proof's public inputs have to be collected by the protocol, as some of
   them will be passed in the operation's payload, but some must be computed
   by the protocol (e.g. the current L2 state).
   These public inputs will be collected as a string map linking
   the circuit identifier to a list of inputs for it (as a circuit might have
   been used several times in a proof).
   As explained in the documentation, circuits in ZKRUs will be grouped into
   three categories: pending (public) operations, private batches and
   fee circuit.
   Each of these expects a different set of public inputs. For this reason,
   the collection of circuit inputs will be collected in three separate steps.
*)

module SMap = Map.Make (String)

(* Helper function to collect inputs *)
let insert s x =
  SMap.update s (function None -> Some [x] | Some l -> Some (x :: l))

(* Traverse the list of pending L2 operations paired with their corresponding
   inputs sent in the [Update] computing the full set of inputs for each of
   them.
   Collect the L2 fees of all L2 operations, and the list of boolean flags
   determining whether each L2 operation will trigger an exit.
*)
let collect_pending_ops_inputs ~zk_rollup ~account ~rev_pi_map
    ~pending_ops_and_pis =
  let open Lwt_result_syntax in
  let open Zk_rollup.Update in
  let open Zk_rollup.Account in
  let* rev_pi_map, new_state, fees, rev_exit_validites =
    List.fold_left_es
      (fun (rev_pi_map, old_state, fees, rev_exit_validites)
           ((l2_op, _ticket_hash_opt), (name, (sent_pi : op_pi))) ->
        let new_state = sent_pi.new_state in
        let*? () =
          error_unless
            Compare.Int.(Array.length new_state = account.static.state_length)
            Zk_rollup.Errors.Inconsistent_state_update
        in
        let pi =
          Zk_rollup.Circuit_public_inputs.(
            Pending_op
              {
                old_state;
                new_state;
                fee = sent_pi.fee;
                exit_validity = sent_pi.exit_validity;
                zk_rollup;
                l2_op;
              })
        in
        let rev_pi_map =
          insert
            name
            (Zk_rollup.Circuit_public_inputs.to_scalar_array pi)
            rev_pi_map
        in
        return
          ( rev_pi_map,
            new_state,
            Bls.Primitive.Fr.add fees sent_pi.fee,
            sent_pi.exit_validity :: rev_exit_validites ))
      (rev_pi_map, account.dynamic.state, Bls.Primitive.Fr.zero, [])
      pending_ops_and_pis
  in
  return (rev_pi_map, new_state, fees, List.rev rev_exit_validites)

(* Traverse the partial inputs for the batches of private operations
   that the [update] claims to process, computing the full set of inputs.
   Check that all circuit identifiers used here are allowed to be used for
   private operations and collect the L2 fees. *)
let collect_pivate_batch_inputs ~zk_rollup ~account ~rev_pi_map ~update
    ~prev_state ~fees =
  let open Lwt_result_syntax in
  let open Zk_rollup.Update in
  let open Zk_rollup.Account in
  let is_private = function Some `Private -> true | _ -> false in
  List.fold_left_es
    (fun (rev_pi_map, old_state, fees) (name, (sent_pi : private_inner_pi)) ->
      let*? () =
        error_unless
          (is_private
             (Zk_rollup.Account.SMap.find name account.static.circuits_info))
          Zk_rollup.Errors.Invalid_circuit
      in
      let new_state = sent_pi.new_state in
      let*? () =
        error_unless
          Compare.Int.(Array.length new_state = account.static.state_length)
          Zk_rollup.Errors.Inconsistent_state_update
      in
      let pi =
        Zk_rollup.Circuit_public_inputs.(
          Private_batch {old_state; new_state; fees = sent_pi.fees; zk_rollup})
      in
      let rev_pi_map =
        insert
          name
          (Zk_rollup.Circuit_public_inputs.to_scalar_array pi)
          rev_pi_map
      in

      return (rev_pi_map, new_state, Bls.Primitive.Fr.add fees sent_pi.fees))
    (rev_pi_map, prev_state, fees)
    update.private_pis

let collect_fee_inputs ~prev_state ~update ~fees ~rev_pi_map =
  let open Zk_rollup.Update in
  let old_state = prev_state in
  let new_state = update.fee_pi.new_state in
  let pi = Zk_rollup.Circuit_public_inputs.(Fee {old_state; new_state; fees}) in
  let rev_pi_map =
    insert "fee" (Zk_rollup.Circuit_public_inputs.to_scalar_array pi) rev_pi_map
  in
  (rev_pi_map, new_state)

(* Collect and validate the public inputs for the verification *)
let collect_inputs ~zk_rollup ~account ~rev_pi_map ~pending_ops_and_pis ~update
    =
  let open Lwt_result_syntax in
  (* Collect the inputs for the pending L2 ops *)
  let* rev_pi_map, new_state, fees, exit_validities =
    collect_pending_ops_inputs
      ~zk_rollup
      ~account
      ~rev_pi_map
      ~pending_ops_and_pis
  in
  (* Collect the inputs for private batches of L2 ops *)
  let* rev_pi_map, new_state, fees =
    collect_pivate_batch_inputs
      ~zk_rollup
      ~account
      ~rev_pi_map
      ~update
      ~prev_state:new_state
      ~fees
  in
  (* Collect the inputs for the fee circuit, always identified as "fee" *)
  let rev_pi_map, new_state =
    collect_fee_inputs ~prev_state:new_state ~update ~fees ~rev_pi_map
  in
  let pi_map = SMap.map List.rev rev_pi_map in
  return (pi_map, exit_validities, new_state)

(* Perform the exits corresponding to the processed public l2 operations *)
let perform_exits ctxt exits =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (ctxt, storage_diff) ((op, ticket_hash_opt), exit_validity) ->
      let open Zk_rollup.Operation in
      match ticket_hash_opt with
      | None ->
          let*? () =
            error_unless
              Compare.Z.(Z.zero = op.price.amount)
              Zk_rollup.Errors.Invalid_deposit_amount
          in
          return (ctxt, storage_diff)
      | Some receiver_ticket_hash ->
          if exit_validity then
            let*? amount =
              Option.value_e
                ~error:
                  (Error_monad.trace_of_error
                     Zk_rollup.Errors.Invalid_deposit_amount)
                (Ticket_amount.of_zint (Z.abs @@ op.price.amount))
            in
            let* ctxt, diff =
              Ticket_transfer.transfer_ticket_with_hashes
                ctxt
                ~sender_hash:op.price.id
                ~dst_hash:receiver_ticket_hash
                amount
            in
            return (ctxt, Z.add diff storage_diff)
          else return (ctxt, storage_diff))
    (ctxt, Z.zero)
    exits

let update ~ctxt_before_op ~ctxt ~zk_rollup ~update =
  let open Lwt_result_syntax in
  let open Zk_rollup.Update in
  let*? () = assert_feature_enabled ctxt in
  let rev_pi_map = SMap.empty in
  let* ctxt, account = Zk_rollup.account ctxt zk_rollup in
  let update_public_length = List.length update.pending_pis in
  let* ctxt, pending_list_length =
    Zk_rollup.get_pending_length ctxt zk_rollup
  in
  let min_pending_to_process =
    Constants.zk_rollup_min_pending_to_process ctxt
  in
  (* The number of pending operations processed by an update must be at least
     [min(pending_list_length, min_pending_to_process)] and at most
     [pending_list_length].*)
  let*? () =
    error_when
      Compare.Int.(
        update_public_length < pending_list_length
        && update_public_length < min_pending_to_process)
      Zk_rollup.Errors.Pending_bound
  in
  let* ctxt, pending_ops =
    Zk_rollup.get_prefix ctxt zk_rollup update_public_length
  in
  (* It's safe to use [combine_drop], as at this point both lists will have the
     same length. *)
  let pending_ops_and_pis = List.combine_drop pending_ops update.pending_pis in
  (* Collect the inputs for the verification *)
  let* pi_map, exit_validities, new_state =
    collect_inputs ~zk_rollup ~account ~rev_pi_map ~pending_ops_and_pis ~update
  in
  (* Run the verification of the Plonk proof *)
  let verified =
    Plonk.verify
      account.static.public_parameters
      (SMap.bindings pi_map)
      update.proof
  in
  let*? () = error_unless verified Zk_rollup.Errors.Invalid_verification in
  (* Update the ZKRU storage with the new state and dropping the processed
     public L2 operations from the pending list *)
  let* ctxt =
    Zk_rollup.update
      ctxt
      zk_rollup
      ~pending_to_drop:update_public_length
      ~new_account:
        {account with dynamic = {account.dynamic with state = new_state}}
  in
  (* Perform exits of processed public L2 operations *)
  let exits = List.combine_drop pending_ops exit_validities in
  let* ctxt, exits_paid_storage_size_diff = perform_exits ctxt exits in

  (* TODO https://gitlab.com/tezos/tezos/-/issues/3544
     Carbonate ZKRU operations *)
  let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
  let result =
    Apply_results.Zk_rollup_update_result
      {
        balance_updates = [];
        consumed_gas;
        paid_storage_size_diff = exits_paid_storage_size_diff;
      }
  in
  return (ctxt, result, [])
