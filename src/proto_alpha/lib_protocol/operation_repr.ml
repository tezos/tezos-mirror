(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Protocol Implementation - Low level Repr. of Operations *)

module Kind = struct
  type preattestation_consensus_kind = Preattestation_consensus_kind

  type endorsement_consensus_kind = Endorsement_consensus_kind

  type 'a consensus =
    | Preendorsement_kind : preattestation_consensus_kind consensus
    | Endorsement_kind : endorsement_consensus_kind consensus

  type preendorsement = preattestation_consensus_kind consensus

  type endorsement = endorsement_consensus_kind consensus

  type dal_attestation = Dal_attestation_kind

  type seed_nonce_revelation = Seed_nonce_revelation_kind

  type vdf_revelation = Vdf_revelation_kind

  type 'a double_consensus_operation_evidence =
    | Double_consensus_operation_evidence

  type double_endorsement_evidence =
    endorsement_consensus_kind double_consensus_operation_evidence

  type double_preendorsement_evidence =
    preattestation_consensus_kind double_consensus_operation_evidence

  type double_baking_evidence = Double_baking_evidence_kind

  type activate_account = Activate_account_kind

  type proposals = Proposals_kind

  type ballot = Ballot_kind

  type reveal = Reveal_kind

  type transaction = Transaction_kind

  type origination = Origination_kind

  type delegation = Delegation_kind

  type event = Event_kind

  type increase_paid_storage = Increase_paid_storage_kind

  type update_consensus_key = Update_consensus_key_kind

  type drain_delegate = Drain_delegate_kind

  type failing_noop = Failing_noop_kind

  type register_global_constant = Register_global_constant_kind

  type transfer_ticket = Transfer_ticket_kind

  type dal_publish_slot_header = Dal_publish_slot_header_kind

  type sc_rollup_originate = Sc_rollup_originate_kind

  type sc_rollup_add_messages = Sc_rollup_add_messages_kind

  type sc_rollup_cement = Sc_rollup_cement_kind

  type sc_rollup_publish = Sc_rollup_publish_kind

  type sc_rollup_refute = Sc_rollup_refute_kind

  type sc_rollup_timeout = Sc_rollup_timeout_kind

  type sc_rollup_execute_outbox_message =
    | Sc_rollup_execute_outbox_message_kind

  type sc_rollup_recover_bond = Sc_rollup_recover_bond_kind

  type zk_rollup_origination = Zk_rollup_origination_kind

  type zk_rollup_publish = Zk_rollup_publish_kind

  type zk_rollup_update = Zk_rollup_update_kind

  type 'a manager =
    | Reveal_manager_kind : reveal manager
    | Transaction_manager_kind : transaction manager
    | Origination_manager_kind : origination manager
    | Delegation_manager_kind : delegation manager
    | Event_manager_kind : event manager
    | Register_global_constant_manager_kind : register_global_constant manager
    | Increase_paid_storage_manager_kind : increase_paid_storage manager
    | Update_consensus_key_manager_kind : update_consensus_key manager
    | Transfer_ticket_manager_kind : transfer_ticket manager
    | Dal_publish_slot_header_manager_kind : dal_publish_slot_header manager
    | Sc_rollup_originate_manager_kind : sc_rollup_originate manager
    | Sc_rollup_add_messages_manager_kind : sc_rollup_add_messages manager
    | Sc_rollup_cement_manager_kind : sc_rollup_cement manager
    | Sc_rollup_publish_manager_kind : sc_rollup_publish manager
    | Sc_rollup_refute_manager_kind : sc_rollup_refute manager
    | Sc_rollup_timeout_manager_kind : sc_rollup_timeout manager
    | Sc_rollup_execute_outbox_message_manager_kind
        : sc_rollup_execute_outbox_message manager
    | Sc_rollup_recover_bond_manager_kind : sc_rollup_recover_bond manager
    | Zk_rollup_origination_manager_kind : zk_rollup_origination manager
    | Zk_rollup_publish_manager_kind : zk_rollup_publish manager
    | Zk_rollup_update_manager_kind : zk_rollup_update manager
end

type 'a consensus_operation_type =
  | Endorsement : Kind.endorsement consensus_operation_type
  | Preendorsement : Kind.preendorsement consensus_operation_type

type consensus_content = {
  slot : Slot_repr.t;
  level : Raw_level_repr.t;
  (* The level is not required to validate an endorsement when it corresponds
     to the current payload, but if we want to filter endorsements, we need
     the level. *)
  round : Round_repr.t;
  block_payload_hash : Block_payload_hash.t;
      (* NOTE: This could be just the hash of the set of operations (the
         actual payload). The grandfather block hash should already be
         fixed by the operation.shell.branch field.  This is not really
         important but could make things easier for debugging *)
}

let consensus_content_encoding =
  let open Data_encoding in
  conv
    (fun {slot; level; round; block_payload_hash} ->
      (slot, level, round, block_payload_hash))
    (fun (slot, level, round, block_payload_hash) ->
      {slot; level; round; block_payload_hash})
    (obj4
       (req "slot" Slot_repr.encoding)
       (req "level" Raw_level_repr.encoding)
       (req "round" Round_repr.encoding)
       (req "block_payload_hash" Block_payload_hash.encoding))

let pp_consensus_content ppf content =
  Format.fprintf
    ppf
    "(%ld, %a, %a, %a)"
    (Raw_level_repr.to_int32 content.level)
    Round_repr.pp
    content.round
    Slot_repr.pp
    content.slot
    Block_payload_hash.pp_short
    content.block_payload_hash

type consensus_watermark =
  | Endorsement of Chain_id.t
  | Preendorsement of Chain_id.t
  | Dal_attestation of Chain_id.t

let to_watermark = function
  | Preendorsement chain_id ->
      Signature.Custom
        (Bytes.cat (Bytes.of_string "\x12") (Chain_id.to_bytes chain_id))
  | Dal_attestation chain_id
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4479

     We reuse the watermark of an endorsement. This is because this
     operation is temporary and aims to be merged with an endorsement
     later on. Moreover, there is a leak of abstraction with the shell
     which makes adding a new watermark a bit awkward. *)
  | Endorsement chain_id ->
      Signature.Custom
        (Bytes.cat (Bytes.of_string "\x13") (Chain_id.to_bytes chain_id))

let of_watermark = function
  | Signature.Custom b ->
      if Compare.Int.(Bytes.length b > 0) then
        match Bytes.get b 0 with
        | '\x12' ->
            Option.map
              (fun chain_id -> Preendorsement chain_id)
              (Chain_id.of_bytes_opt (Bytes.sub b 1 (Bytes.length b - 1)))
        | '\x13' ->
            Option.map
              (fun chain_id -> Endorsement chain_id)
              (Chain_id.of_bytes_opt (Bytes.sub b 1 (Bytes.length b - 1)))
        | _ -> None
      else None
  | _ -> None

type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type 'kind operation = {
  shell : Operation.shell_header;
  protocol_data : 'kind protocol_data;
}

and 'kind protocol_data = {
  contents : 'kind contents_list;
  signature : Signature.t option;
}

and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons :
      'kind Kind.manager contents * 'rest Kind.manager contents_list
      -> ('kind * 'rest) Kind.manager contents_list

and _ contents =
  | Preendorsement : consensus_content -> Kind.preendorsement contents
  | Endorsement : consensus_content -> Kind.endorsement contents
  | Dal_attestation :
      Dal_attestation_repr.operation
      -> Kind.dal_attestation contents
  | Seed_nonce_revelation : {
      level : Raw_level_repr.t;
      nonce : Seed_repr.nonce;
    }
      -> Kind.seed_nonce_revelation contents
  | Vdf_revelation : {
      solution : Seed_repr.vdf_solution;
    }
      -> Kind.vdf_revelation contents
  | Double_preendorsement_evidence : {
      op1 : Kind.preendorsement operation;
      op2 : Kind.preendorsement operation;
    }
      -> Kind.double_preendorsement_evidence contents
  | Double_endorsement_evidence : {
      op1 : Kind.endorsement operation;
      op2 : Kind.endorsement operation;
    }
      -> Kind.double_endorsement_evidence contents
  | Double_baking_evidence : {
      bh1 : Block_header_repr.t;
      bh2 : Block_header_repr.t;
    }
      -> Kind.double_baking_evidence contents
  | Activate_account : {
      id : Ed25519.Public_key_hash.t;
      activation_code : Blinded_public_key_hash.activation_code;
    }
      -> Kind.activate_account contents
  | Proposals : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposals : Protocol_hash.t list;
    }
      -> Kind.proposals contents
  | Ballot : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposal : Protocol_hash.t;
      ballot : Vote_repr.ballot;
    }
      -> Kind.ballot contents
  | Drain_delegate : {
      consensus_key : Signature.Public_key_hash.t;
      delegate : Signature.Public_key_hash.t;
      destination : Signature.Public_key_hash.t;
    }
      -> Kind.drain_delegate contents
  | Failing_noop : string -> Kind.failing_noop contents
  | Manager_operation : {
      source : Signature.public_key_hash;
      fee : Tez_repr.tez;
      counter : Manager_counter_repr.t;
      operation : 'kind manager_operation;
      gas_limit : Gas_limit_repr.Arith.integral;
      storage_limit : Z.t;
    }
      -> 'kind Kind.manager contents

and _ manager_operation =
  | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
  | Transaction : {
      amount : Tez_repr.tez;
      parameters : Script_repr.lazy_expr;
      entrypoint : Entrypoint_repr.t;
      destination : Contract_repr.t;
    }
      -> Kind.transaction manager_operation
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script_repr.t;
      credit : Tez_repr.tez;
    }
      -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation
  | Register_global_constant : {
      value : Script_repr.lazy_expr;
    }
      -> Kind.register_global_constant manager_operation
  | Increase_paid_storage : {
      amount_in_bytes : Z.t;
      destination : Contract_hash.t;
    }
      -> Kind.increase_paid_storage manager_operation
  | Update_consensus_key :
      Signature.Public_key.t
      -> Kind.update_consensus_key manager_operation
  | Transfer_ticket : {
      contents : Script_repr.lazy_expr;
      ty : Script_repr.lazy_expr;
      ticketer : Contract_repr.t;
      amount : Ticket_amount.t;
      destination : Contract_repr.t;
      entrypoint : Entrypoint_repr.t;
    }
      -> Kind.transfer_ticket manager_operation
  | Dal_publish_slot_header :
      Dal_operations_repr.Publish_slot_header.t
      -> Kind.dal_publish_slot_header manager_operation
  | Sc_rollup_originate : {
      kind : Sc_rollups.Kind.t;
      boot_sector : string;
      parameters_ty : Script_repr.lazy_expr;
    }
      -> Kind.sc_rollup_originate manager_operation
  | Sc_rollup_add_messages : {
      messages : string list;
    }
      -> Kind.sc_rollup_add_messages manager_operation
  | Sc_rollup_cement : {
      rollup : Sc_rollup_repr.t;
    }
      -> Kind.sc_rollup_cement manager_operation
  | Sc_rollup_publish : {
      rollup : Sc_rollup_repr.t;
      commitment : Sc_rollup_commitment_repr.t;
    }
      -> Kind.sc_rollup_publish manager_operation
  | Sc_rollup_refute : {
      rollup : Sc_rollup_repr.t;
      opponent : Sc_rollup_repr.Staker.t;
      refutation : Sc_rollup_game_repr.refutation;
    }
      -> Kind.sc_rollup_refute manager_operation
  | Sc_rollup_timeout : {
      rollup : Sc_rollup_repr.t;
      stakers : Sc_rollup_game_repr.Index.t;
    }
      -> Kind.sc_rollup_timeout manager_operation
  | Sc_rollup_execute_outbox_message : {
      rollup : Sc_rollup_repr.t;
      cemented_commitment : Sc_rollup_commitment_repr.Hash.t;
      output_proof : string;
    }
      -> Kind.sc_rollup_execute_outbox_message manager_operation
  | Sc_rollup_recover_bond : {
      sc_rollup : Sc_rollup_repr.t;
      staker : Signature.public_key_hash;
    }
      -> Kind.sc_rollup_recover_bond manager_operation
  | Zk_rollup_origination : {
      public_parameters : Plonk.public_parameters;
      circuits_info : [`Public | `Private | `Fee] Zk_rollup_account_repr.SMap.t;
      init_state : Zk_rollup_state_repr.t;
      nb_ops : int;
    }
      -> Kind.zk_rollup_origination manager_operation
  | Zk_rollup_publish : {
      zk_rollup : Zk_rollup_repr.t;
      ops : (Zk_rollup_operation_repr.t * Zk_rollup_ticket_repr.t option) list;
    }
      -> Kind.zk_rollup_publish manager_operation
  | Zk_rollup_update : {
      zk_rollup : Zk_rollup_repr.t;
      update : Zk_rollup_update_repr.t;
    }
      -> Kind.zk_rollup_update manager_operation

let manager_kind : type kind. kind manager_operation -> kind Kind.manager =
  function
  | Reveal _ -> Kind.Reveal_manager_kind
  | Transaction _ -> Kind.Transaction_manager_kind
  | Origination _ -> Kind.Origination_manager_kind
  | Delegation _ -> Kind.Delegation_manager_kind
  | Register_global_constant _ -> Kind.Register_global_constant_manager_kind
  | Increase_paid_storage _ -> Kind.Increase_paid_storage_manager_kind
  | Update_consensus_key _ -> Kind.Update_consensus_key_manager_kind
  | Transfer_ticket _ -> Kind.Transfer_ticket_manager_kind
  | Dal_publish_slot_header _ -> Kind.Dal_publish_slot_header_manager_kind
  | Sc_rollup_originate _ -> Kind.Sc_rollup_originate_manager_kind
  | Sc_rollup_add_messages _ -> Kind.Sc_rollup_add_messages_manager_kind
  | Sc_rollup_cement _ -> Kind.Sc_rollup_cement_manager_kind
  | Sc_rollup_publish _ -> Kind.Sc_rollup_publish_manager_kind
  | Sc_rollup_refute _ -> Kind.Sc_rollup_refute_manager_kind
  | Sc_rollup_timeout _ -> Kind.Sc_rollup_timeout_manager_kind
  | Sc_rollup_execute_outbox_message _ ->
      Kind.Sc_rollup_execute_outbox_message_manager_kind
  | Sc_rollup_recover_bond _ -> Kind.Sc_rollup_recover_bond_manager_kind
  | Zk_rollup_origination _ -> Kind.Zk_rollup_origination_manager_kind
  | Zk_rollup_publish _ -> Kind.Zk_rollup_publish_manager_kind
  | Zk_rollup_update _ -> Kind.Zk_rollup_update_manager_kind

type packed_manager_operation =
  | Manager : 'kind manager_operation -> packed_manager_operation

type packed_contents = Contents : 'kind contents -> packed_contents

type packed_contents_list =
  | Contents_list : 'kind contents_list -> packed_contents_list

type packed_protocol_data =
  | Operation_data : 'kind protocol_data -> packed_protocol_data

type packed_operation = {
  shell : Operation.shell_header;
  protocol_data : packed_protocol_data;
}

let pack ({shell; protocol_data} : _ operation) : packed_operation =
  {shell; protocol_data = Operation_data protocol_data}

let rec contents_list_to_list : type a. a contents_list -> _ = function
  | Single o -> [Contents o]
  | Cons (o, os) -> Contents o :: contents_list_to_list os

let to_list = function Contents_list l -> contents_list_to_list l

(* This first version of of_list has the type (_, string) result expected by
   the conv_with_guard combinator of Data_encoding. For a more conventional
   return type see [of_list] below. *)
let of_list_internal contents =
  let rec of_list_internal acc = function
    | [] -> Ok acc
    | Contents o :: os -> (
        match (o, acc) with
        | ( Manager_operation _,
            Contents_list (Single (Manager_operation _) as rest) ) ->
            (of_list_internal [@tailcall]) (Contents_list (Cons (o, rest))) os
        | Manager_operation _, Contents_list (Cons _ as rest) ->
            (of_list_internal [@tailcall]) (Contents_list (Cons (o, rest))) os
        | _ ->
            Error
              "Operation list of length > 1 should only contain manager \
               operations.")
  in
  match List.rev contents with
  | [] -> Error "Operation lists should not be empty."
  | Contents o :: os -> of_list_internal (Contents_list (Single o)) os

type error += Contents_list_error of string (* `Permanent *)

let of_list l =
  match of_list_internal l with
  | Ok contents -> Ok contents
  | Error s -> error @@ Contents_list_error s

let tx_rollup_operation_tag_offset = 150

let tx_rollup_operation_origination_tag = tx_rollup_operation_tag_offset + 0

let tx_rollup_operation_submit_batch_tag = tx_rollup_operation_tag_offset + 1

let tx_rollup_operation_commit_tag = tx_rollup_operation_tag_offset + 2

let tx_rollup_operation_return_bond_tag = tx_rollup_operation_tag_offset + 3

let tx_rollup_operation_finalize_commitment_tag =
  tx_rollup_operation_tag_offset + 4

let tx_rollup_operation_remove_commitment_tag =
  tx_rollup_operation_tag_offset + 5

let tx_rollup_operation_rejection_tag = tx_rollup_operation_tag_offset + 6

let tx_rollup_operation_dispatch_tickets_tag =
  tx_rollup_operation_tag_offset + 7

(** The following operation tags cannot be used again, it is checked
    at compilation time. *)
let tx_rollup_forbidden_operation_tags =
  [
    tx_rollup_operation_origination_tag;
    tx_rollup_operation_submit_batch_tag;
    tx_rollup_operation_commit_tag;
    tx_rollup_operation_return_bond_tag;
    tx_rollup_operation_finalize_commitment_tag;
    tx_rollup_operation_remove_commitment_tag;
    tx_rollup_operation_rejection_tag;
    tx_rollup_operation_dispatch_tickets_tag;
  ]

let transfer_ticket_tag = tx_rollup_operation_tag_offset + 8

let sc_rollup_operation_tag_offset = 200

let sc_rollup_operation_origination_tag = sc_rollup_operation_tag_offset + 0

let sc_rollup_operation_add_message_tag = sc_rollup_operation_tag_offset + 1

let sc_rollup_operation_cement_tag = sc_rollup_operation_tag_offset + 2

let sc_rollup_operation_publish_tag = sc_rollup_operation_tag_offset + 3

let sc_rollup_operation_refute_tag = sc_rollup_operation_tag_offset + 4

let sc_rollup_operation_timeout_tag = sc_rollup_operation_tag_offset + 5

let sc_rollup_execute_outbox_message_tag = sc_rollup_operation_tag_offset + 6

let sc_rollup_operation_recover_bond_tag = sc_rollup_operation_tag_offset + 7

let dal_offset = 230

let dal_publish_slot_header_tag = dal_offset + 0

let zk_rollup_operation_tag_offset = 250

let zk_rollup_operation_create_tag = zk_rollup_operation_tag_offset + 0

let zk_rollup_operation_publish_tag = zk_rollup_operation_tag_offset + 1

let zk_rollup_operation_update_tag = zk_rollup_operation_tag_offset + 2

module Encoding = struct
  open Data_encoding

  (** These tags can not be used yet for operations. *)
  let reserved_tag t =
    (* These tags are reserved for future extensions: [fd] - [ff]. *)
    Compare.Int.(t >= 0xfd)
    || (* These tags were used by old operations.
          The operations have been removed in protocol proposal N, it can
          be unblocked in the future (e.g. proposal O, P etc.). *)
    List.exists (Compare.Int.equal t) tx_rollup_forbidden_operation_tags

  let signature_prefix_tag = 0xff

  let () = assert (reserved_tag signature_prefix_tag)

  let case tag name args proj inj =
    case
      tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  module Manager_operations = struct
    type 'kind case =
      | MCase : {
          tag : int;
          name : string;
          encoding : 'a Data_encoding.t;
          select : packed_manager_operation -> 'kind manager_operation option;
          proj : 'kind manager_operation -> 'a;
          inj : 'a -> 'kind manager_operation;
        }
          -> 'kind case

    let reveal_case =
      MCase
        {
          tag = 0;
          name = "reveal";
          encoding = obj1 (req "public_key" Signature.Public_key.encoding);
          select = (function Manager (Reveal _ as op) -> Some op | _ -> None);
          proj = (function Reveal pkh -> pkh);
          inj = (fun pkh -> Reveal pkh);
        }

    let transaction_case =
      MCase
        {
          tag = 1;
          name = "transaction";
          encoding =
            obj3
              (req "amount" Tez_repr.encoding)
              (req "destination" Contract_repr.encoding)
              (opt
                 "parameters"
                 (obj2
                    (req "entrypoint" Entrypoint_repr.smart_encoding)
                    (req "value" Script_repr.lazy_expr_encoding)));
          select =
            (function Manager (Transaction _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Transaction {amount; destination; parameters; entrypoint} ->
                let parameters =
                  if
                    Script_repr.is_unit_parameter parameters
                    && Entrypoint_repr.is_default entrypoint
                  then None
                  else Some (entrypoint, parameters)
                in
                (amount, destination, parameters));
          inj =
            (fun (amount, destination, parameters) ->
              let entrypoint, parameters =
                match parameters with
                | None -> (Entrypoint_repr.default, Script_repr.unit_parameter)
                | Some (entrypoint, value) -> (entrypoint, value)
              in
              Transaction {amount; destination; parameters; entrypoint});
        }

    let origination_case =
      MCase
        {
          tag = 2;
          name = "origination";
          encoding =
            obj3
              (req "balance" Tez_repr.encoding)
              (opt "delegate" Signature.Public_key_hash.encoding)
              (req "script" Script_repr.encoding);
          select =
            (function Manager (Origination _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Origination {credit; delegate; script} ->
                (credit, delegate, script));
          inj =
            (fun (credit, delegate, script) ->
              Origination {credit; delegate; script});
        }

    let delegation_case =
      MCase
        {
          tag = 3;
          name = "delegation";
          encoding = obj1 (opt "delegate" Signature.Public_key_hash.encoding);
          select =
            (function Manager (Delegation _ as op) -> Some op | _ -> None);
          proj = (function Delegation key -> key);
          inj = (fun key -> Delegation key);
        }

    let register_global_constant_case =
      MCase
        {
          tag = 4;
          name = "register_global_constant";
          encoding = obj1 (req "value" Script_repr.lazy_expr_encoding);
          select =
            (function
            | Manager (Register_global_constant _ as op) -> Some op | _ -> None);
          proj = (function Register_global_constant {value} -> value);
          inj = (fun value -> Register_global_constant {value});
        }

    (* Tag 5 was for Set_deposits_limit. *)

    let increase_paid_storage_case =
      MCase
        {
          tag = 9;
          name = "increase_paid_storage";
          encoding =
            obj2
              (req "amount" Data_encoding.z)
              (req "destination" Contract_repr.originated_encoding);
          select =
            (function
            | Manager (Increase_paid_storage _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Increase_paid_storage {amount_in_bytes; destination} ->
                (amount_in_bytes, destination));
          inj =
            (fun (amount_in_bytes, destination) ->
              Increase_paid_storage {amount_in_bytes; destination});
        }

    let update_consensus_key_tag = 6

    let update_consensus_key_case =
      MCase
        {
          tag = update_consensus_key_tag;
          name = "update_consensus_key";
          encoding = obj1 (req "pk" Signature.Public_key.encoding);
          select =
            (function
            | Manager (Update_consensus_key _ as op) -> Some op | _ -> None);
          proj = (function Update_consensus_key consensus_pk -> consensus_pk);
          inj = (fun consensus_pk -> Update_consensus_key consensus_pk);
        }

    let transfer_ticket_case =
      MCase
        {
          tag = transfer_ticket_tag;
          name = "transfer_ticket";
          encoding =
            obj6
              (req "ticket_contents" Script_repr.lazy_expr_encoding)
              (req "ticket_ty" Script_repr.lazy_expr_encoding)
              (req "ticket_ticketer" Contract_repr.encoding)
              (req "ticket_amount" Ticket_amount.encoding)
              (req "destination" Contract_repr.encoding)
              (req "entrypoint" Entrypoint_repr.simple_encoding);
          select =
            (function
            | Manager (Transfer_ticket _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Transfer_ticket
                {contents; ty; ticketer; amount; destination; entrypoint} ->
                (contents, ty, ticketer, amount, destination, entrypoint));
          inj =
            (fun (contents, ty, ticketer, amount, destination, entrypoint) ->
              Transfer_ticket
                {contents; ty; ticketer; amount; destination; entrypoint});
        }

    let zk_rollup_origination_case =
      MCase
        {
          tag = zk_rollup_operation_create_tag;
          name = "zk_rollup_origination";
          encoding =
            obj4
              (req "public_parameters" Plonk.public_parameters_encoding)
              (req
                 "circuits_info"
                 Zk_rollup_account_repr.circuits_info_encoding)
              (req "init_state" Zk_rollup_state_repr.encoding)
              (* TODO https://gitlab.com/tezos/tezos/-/issues/3655
                 Encoding of non-negative [nb_ops] for origination *)
              (req "nb_ops" int31);
          select =
            (function
            | Manager (Zk_rollup_origination _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Zk_rollup_origination
                {public_parameters; circuits_info; init_state; nb_ops} ->
                (public_parameters, circuits_info, init_state, nb_ops));
          inj =
            (fun (public_parameters, circuits_info, init_state, nb_ops) ->
              Zk_rollup_origination
                {public_parameters; circuits_info; init_state; nb_ops});
        }

    let zk_rollup_publish_case =
      MCase
        {
          tag = zk_rollup_operation_publish_tag;
          name = "zk_rollup_publish";
          encoding =
            obj2
              (req "zk_rollup" Zk_rollup_repr.Address.encoding)
              (req "op"
              @@ Data_encoding.list
                   (tup2
                      Zk_rollup_operation_repr.encoding
                      (option Zk_rollup_ticket_repr.encoding)));
          select =
            (function
            | Manager (Zk_rollup_publish _ as op) -> Some op | _ -> None);
          proj =
            (function Zk_rollup_publish {zk_rollup; ops} -> (zk_rollup, ops));
          inj = (fun (zk_rollup, ops) -> Zk_rollup_publish {zk_rollup; ops});
        }

    let zk_rollup_update_case =
      MCase
        {
          tag = zk_rollup_operation_update_tag;
          name = "zk_rollup_update";
          encoding =
            obj2
              (req "zk_rollup" Zk_rollup_repr.Address.encoding)
              (req "update" Zk_rollup_update_repr.encoding);
          select =
            (function
            | Manager (Zk_rollup_update _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Zk_rollup_update {zk_rollup; update} -> (zk_rollup, update));
          inj =
            (fun (zk_rollup, update) -> Zk_rollup_update {zk_rollup; update});
        }

    let sc_rollup_originate_case =
      MCase
        {
          tag = sc_rollup_operation_origination_tag;
          name = "smart_rollup_originate";
          encoding =
            obj3
              (req "pvm_kind" Sc_rollups.Kind.encoding)
              (req "kernel" (string Hex))
              (req "parameters_ty" Script_repr.lazy_expr_encoding);
          select =
            (function
            | Manager (Sc_rollup_originate _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Sc_rollup_originate {kind; boot_sector; parameters_ty} ->
                (kind, boot_sector, parameters_ty));
          inj =
            (fun (kind, boot_sector, parameters_ty) ->
              Sc_rollup_originate {kind; boot_sector; parameters_ty});
        }

    let dal_publish_slot_header_case =
      MCase
        {
          tag = dal_publish_slot_header_tag;
          name = "dal_publish_slot_header";
          encoding =
            obj1
              (req
                 "slot_header"
                 Dal_operations_repr.Publish_slot_header.encoding);
          select =
            (function
            | Manager (Dal_publish_slot_header _ as op) -> Some op | _ -> None);
          proj = (function Dal_publish_slot_header slot_header -> slot_header);
          inj = (fun slot_header -> Dal_publish_slot_header slot_header);
        }

    let sc_rollup_add_messages_case =
      MCase
        {
          tag = sc_rollup_operation_add_message_tag;
          name = "smart_rollup_add_messages";
          encoding = obj1 (req "message" (list (string Hex)));
          select =
            (function
            | Manager (Sc_rollup_add_messages _ as op) -> Some op | _ -> None);
          proj = (function Sc_rollup_add_messages {messages} -> messages);
          inj = (fun messages -> Sc_rollup_add_messages {messages});
        }

    let sc_rollup_cement_case =
      MCase
        {
          tag = sc_rollup_operation_cement_tag;
          name = "smart_rollup_cement";
          encoding = obj1 (req "rollup" Sc_rollup_repr.encoding);
          select =
            (function
            | Manager (Sc_rollup_cement _ as op) -> Some op | _ -> None);
          proj = (function Sc_rollup_cement {rollup} -> rollup);
          inj = (fun rollup -> Sc_rollup_cement {rollup});
        }

    let sc_rollup_publish_case =
      MCase
        {
          tag = sc_rollup_operation_publish_tag;
          name = "smart_rollup_publish";
          encoding =
            obj2
              (req "rollup" Sc_rollup_repr.encoding)
              (req "commitment" Sc_rollup_commitment_repr.encoding);
          select =
            (function
            | Manager (Sc_rollup_publish _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Sc_rollup_publish {rollup; commitment} -> (rollup, commitment));
          inj =
            (fun (rollup, commitment) -> Sc_rollup_publish {rollup; commitment});
        }

    let sc_rollup_refute_case =
      MCase
        {
          tag = sc_rollup_operation_refute_tag;
          name = "smart_rollup_refute";
          encoding =
            obj3
              (req "rollup" Sc_rollup_repr.encoding)
              (req "opponent" Sc_rollup_repr.Staker.encoding)
              (req "refutation" Sc_rollup_game_repr.refutation_encoding);
          select =
            (function
            | Manager (Sc_rollup_refute _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Sc_rollup_refute {rollup; opponent; refutation} ->
                (rollup, opponent, refutation));
          inj =
            (fun (rollup, opponent, refutation) ->
              Sc_rollup_refute {rollup; opponent; refutation});
        }

    let sc_rollup_timeout_case =
      MCase
        {
          tag = sc_rollup_operation_timeout_tag;
          name = "smart_rollup_timeout";
          encoding =
            obj2
              (req "rollup" Sc_rollup_repr.encoding)
              (req "stakers" Sc_rollup_game_repr.Index.encoding);
          select =
            (function
            | Manager (Sc_rollup_timeout _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Sc_rollup_timeout {rollup; stakers} -> (rollup, stakers));
          inj = (fun (rollup, stakers) -> Sc_rollup_timeout {rollup; stakers});
        }

    let sc_rollup_execute_outbox_message_case =
      MCase
        {
          tag = sc_rollup_execute_outbox_message_tag;
          name = "smart_rollup_execute_outbox_message";
          encoding =
            obj3
              (req "rollup" Sc_rollup_repr.encoding)
              (req
                 "cemented_commitment"
                 Sc_rollup_commitment_repr.Hash.encoding)
              (req "output_proof" (string Hex));
          select =
            (function
            | Manager (Sc_rollup_execute_outbox_message _ as op) -> Some op
            | _ -> None);
          proj =
            (function
            | Sc_rollup_execute_outbox_message
                {rollup; cemented_commitment; output_proof} ->
                (rollup, cemented_commitment, output_proof));
          inj =
            (fun (rollup, cemented_commitment, output_proof) ->
              Sc_rollup_execute_outbox_message
                {rollup; cemented_commitment; output_proof});
        }

    let sc_rollup_recover_bond_case =
      MCase
        {
          tag = sc_rollup_operation_recover_bond_tag;
          name = "smart_rollup_recover_bond";
          encoding =
            obj2
              (req "rollup" Sc_rollup_repr.Address.encoding)
              (req "staker" Signature.Public_key_hash.encoding);
          select =
            (function
            | Manager (Sc_rollup_recover_bond _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Sc_rollup_recover_bond {sc_rollup; staker} -> (sc_rollup, staker));
          inj =
            (fun (sc_rollup, staker) ->
              Sc_rollup_recover_bond {sc_rollup; staker});
        }
  end

  type 'b case =
    | Case : {
        tag : int;
        name : string;
        encoding : 'a Data_encoding.t;
        select : packed_contents -> 'b contents option;
        proj : 'b contents -> 'a;
        inj : 'a -> 'b contents;
      }
        -> 'b case

  let preendorsement_case =
    Case
      {
        tag = 20;
        name = "preendorsement";
        encoding = consensus_content_encoding;
        select =
          (function Contents (Preendorsement _ as op) -> Some op | _ -> None);
        proj = (fun (Preendorsement preendorsement) -> preendorsement);
        inj = (fun preendorsement -> Preendorsement preendorsement);
      }

  let preattestation_case =
    Case
      {
        tag = 20;
        name = "preattestation";
        encoding = consensus_content_encoding;
        select =
          (function Contents (Preendorsement _ as op) -> Some op | _ -> None);
        proj = (fun (Preendorsement preendorsement) -> preendorsement);
        inj = (fun preendorsement -> Preendorsement preendorsement);
      }

  let preendorsement_encoding =
    let make (Case {tag; name; encoding; select = _; proj; inj}) =
      case (Tag tag) name encoding (fun o -> Some (proj o)) (fun x -> inj x)
    in
    let to_list : Kind.preendorsement contents_list -> _ = function
      | Single o -> o
    in
    let of_list : Kind.preendorsement contents -> _ = function
      | o -> Single o
    in
    def "inlined.preendorsement"
    @@ conv
         (fun ({shell; protocol_data = {contents; signature}} : _ operation) ->
           (shell, (contents, signature)))
         (fun (shell, (contents, signature)) : _ operation ->
           {shell; protocol_data = {contents; signature}})
         (merge_objs
            Operation.shell_header_encoding
            (obj2
               (req
                  "operations"
                  (conv to_list of_list
                  @@ def "inlined.preendorsement.contents"
                  @@ union [make preendorsement_case]))
               (varopt "signature" Signature.encoding)))

  let preattestation_encoding =
    let make (Case {tag; name; encoding; select = _; proj; inj}) =
      case (Tag tag) name encoding (fun o -> Some (proj o)) (fun x -> inj x)
    in
    let to_list : Kind.preendorsement contents_list -> _ = function
      | Single o -> o
    in
    let of_list : Kind.preendorsement contents -> _ = function
      | o -> Single o
    in
    def "inlined.preattestation"
    @@ conv
         (fun ({shell; protocol_data = {contents; signature}} : _ operation) ->
           (shell, (contents, signature)))
         (fun (shell, (contents, signature)) : _ operation ->
           {shell; protocol_data = {contents; signature}})
         (merge_objs
            Operation.shell_header_encoding
            (obj2
               (req
                  "operations"
                  (conv to_list of_list
                  @@ def "inlined.preattestation.contents"
                  @@ union [make preattestation_case]))
               (varopt "signature" Signature.encoding)))

  let endorsement_encoding =
    obj4
      (req "slot" Slot_repr.encoding)
      (req "level" Raw_level_repr.encoding)
      (req "round" Round_repr.encoding)
      (req "block_payload_hash" Block_payload_hash.encoding)

  let endorsement_case =
    Case
      {
        tag = 21;
        name = "endorsement";
        encoding = endorsement_encoding;
        select =
          (function Contents (Endorsement _ as op) -> Some op | _ -> None);
        proj =
          (fun (Endorsement consensus_content) ->
            ( consensus_content.slot,
              consensus_content.level,
              consensus_content.round,
              consensus_content.block_payload_hash ));
        inj =
          (fun (slot, level, round, block_payload_hash) ->
            Endorsement {slot; level; round; block_payload_hash});
      }

  let attestation_case =
    Case
      {
        tag = 21;
        name = "attestation";
        encoding = endorsement_encoding;
        select =
          (function Contents (Endorsement _ as op) -> Some op | _ -> None);
        proj =
          (fun (Endorsement consensus_content) ->
            ( consensus_content.slot,
              consensus_content.level,
              consensus_content.round,
              consensus_content.block_payload_hash ));
        inj =
          (fun (slot, level, round, block_payload_hash) ->
            Endorsement {slot; level; round; block_payload_hash});
      }

  let endorsement_encoding =
    let make (Case {tag; name; encoding; select = _; proj; inj}) =
      case (Tag tag) name encoding (fun o -> Some (proj o)) (fun x -> inj x)
    in
    let to_list : Kind.endorsement contents_list -> _ = fun (Single o) -> o in
    let of_list : Kind.endorsement contents -> _ = fun o -> Single o in
    def "inlined.endorsement"
    @@ conv
         (fun ({shell; protocol_data = {contents; signature}} : _ operation) ->
           (shell, (contents, signature)))
         (fun (shell, (contents, signature)) : _ operation ->
           {shell; protocol_data = {contents; signature}})
         (merge_objs
            Operation.shell_header_encoding
            (obj2
               (req
                  "operations"
                  (conv to_list of_list
                  @@ def "inlined.endorsement_mempool.contents"
                  @@ union [make endorsement_case]))
               (varopt "signature" Signature.encoding)))

  let attestation_encoding =
    let make (Case {tag; name; encoding; select = _; proj; inj}) =
      case (Tag tag) name encoding (fun o -> Some (proj o)) (fun x -> inj x)
    in
    let to_list : Kind.endorsement contents_list -> _ = fun (Single o) -> o in
    let of_list : Kind.endorsement contents -> _ = fun o -> Single o in
    def "inlined.attestation"
    @@ conv
         (fun ({shell; protocol_data = {contents; signature}} : _ operation) ->
           (shell, (contents, signature)))
         (fun (shell, (contents, signature)) : _ operation ->
           {shell; protocol_data = {contents; signature}})
         (merge_objs
            Operation.shell_header_encoding
            (obj2
               (req
                  "operations"
                  (conv to_list of_list
                  @@ def "inlined.attestation_mempool.contents"
                  @@ union [make attestation_case]))
               (varopt "signature" Signature.encoding)))

  let dal_attestation_encoding =
    obj3
      (req "attestor" Signature.Public_key_hash.encoding)
      (req "attestation" Dal_attestation_repr.encoding)
      (req "level" Raw_level_repr.encoding)

  let dal_attestation_case =
    Case
      {
        tag = 22;
        name = "dal_attestation";
        encoding = dal_attestation_encoding;
        select =
          (function Contents (Dal_attestation _ as op) -> Some op | _ -> None);
        proj =
          (fun (Dal_attestation
                 Dal_attestation_repr.{attestor; attestation; level}) ->
            (attestor, attestation, level));
        inj =
          (fun (attestor, attestation, level) ->
            Dal_attestation Dal_attestation_repr.{attestor; attestation; level});
      }

  let seed_nonce_revelation_case =
    Case
      {
        tag = 1;
        name = "seed_nonce_revelation";
        encoding =
          obj2
            (req "level" Raw_level_repr.encoding)
            (req "nonce" Seed_repr.nonce_encoding);
        select =
          (function
          | Contents (Seed_nonce_revelation _ as op) -> Some op | _ -> None);
        proj = (fun (Seed_nonce_revelation {level; nonce}) -> (level, nonce));
        inj = (fun (level, nonce) -> Seed_nonce_revelation {level; nonce});
      }

  let vdf_revelation_case =
    Case
      {
        tag = 8;
        name = "vdf_revelation";
        encoding = obj1 (req "solution" Seed_repr.vdf_solution_encoding);
        select =
          (function Contents (Vdf_revelation _ as op) -> Some op | _ -> None);
        proj = (function Vdf_revelation {solution} -> solution);
        inj = (fun solution -> Vdf_revelation {solution});
      }

  let double_preendorsement_evidence_case :
      Kind.double_preendorsement_evidence case =
    Case
      {
        tag = 7;
        name = "double_preendorsement_evidence";
        encoding =
          obj2
            (req "op1" (dynamic_size preendorsement_encoding))
            (req "op2" (dynamic_size preendorsement_encoding));
        select =
          (function
          | Contents (Double_preendorsement_evidence _ as op) -> Some op
          | _ -> None);
        proj = (fun (Double_preendorsement_evidence {op1; op2}) -> (op1, op2));
        inj = (fun (op1, op2) -> Double_preendorsement_evidence {op1; op2});
      }

  let double_preattestation_evidence_case :
      Kind.double_preendorsement_evidence case =
    Case
      {
        tag = 7;
        name = "double_preattestation_evidence";
        encoding =
          obj2
            (req "op1" (dynamic_size preattestation_encoding))
            (req "op2" (dynamic_size preattestation_encoding));
        select =
          (function
          | Contents (Double_preendorsement_evidence _ as op) -> Some op
          | _ -> None);
        proj = (fun (Double_preendorsement_evidence {op1; op2}) -> (op1, op2));
        inj = (fun (op1, op2) -> Double_preendorsement_evidence {op1; op2});
      }

  let double_endorsement_evidence_case : Kind.double_endorsement_evidence case =
    Case
      {
        tag = 2;
        name = "double_endorsement_evidence";
        encoding =
          obj2
            (req "op1" (dynamic_size endorsement_encoding))
            (req "op2" (dynamic_size endorsement_encoding));
        select =
          (function
          | Contents (Double_endorsement_evidence _ as op) -> Some op
          | _ -> None);
        proj = (fun (Double_endorsement_evidence {op1; op2}) -> (op1, op2));
        inj = (fun (op1, op2) -> Double_endorsement_evidence {op1; op2});
      }

  let double_attestation_evidence_case : Kind.double_endorsement_evidence case =
    Case
      {
        tag = 2;
        name = "double_attestation_evidence";
        encoding =
          obj2
            (req "op1" (dynamic_size attestation_encoding))
            (req "op2" (dynamic_size attestation_encoding));
        select =
          (function
          | Contents (Double_endorsement_evidence _ as op) -> Some op
          | _ -> None);
        proj = (fun (Double_endorsement_evidence {op1; op2}) -> (op1, op2));
        inj = (fun (op1, op2) -> Double_endorsement_evidence {op1; op2});
      }

  let double_baking_evidence_case =
    Case
      {
        tag = 3;
        name = "double_baking_evidence";
        encoding =
          obj2
            (req "bh1" (dynamic_size Block_header_repr.encoding))
            (req "bh2" (dynamic_size Block_header_repr.encoding));
        select =
          (function
          | Contents (Double_baking_evidence _ as op) -> Some op | _ -> None);
        proj = (fun (Double_baking_evidence {bh1; bh2}) -> (bh1, bh2));
        inj = (fun (bh1, bh2) -> Double_baking_evidence {bh1; bh2});
      }

  let activate_account_case =
    Case
      {
        tag = 4;
        name = "activate_account";
        encoding =
          obj2
            (req "pkh" Ed25519.Public_key_hash.encoding)
            (req "secret" Blinded_public_key_hash.activation_code_encoding);
        select =
          (function
          | Contents (Activate_account _ as op) -> Some op | _ -> None);
        proj =
          (fun (Activate_account {id; activation_code}) ->
            (id, activation_code));
        inj =
          (fun (id, activation_code) -> Activate_account {id; activation_code});
      }

  let proposals_case =
    Case
      {
        tag = 5;
        name = "proposals";
        encoding =
          obj3
            (req "source" Signature.Public_key_hash.encoding)
            (req "period" int32)
            (req
               "proposals"
               (list
                  ~max_length:Constants_repr.max_proposals_per_delegate
                  Protocol_hash.encoding));
        select =
          (function Contents (Proposals _ as op) -> Some op | _ -> None);
        proj =
          (fun (Proposals {source; period; proposals}) ->
            (source, period, proposals));
        inj =
          (fun (source, period, proposals) ->
            Proposals {source; period; proposals});
      }

  let ballot_case =
    Case
      {
        tag = 6;
        name = "ballot";
        encoding =
          obj4
            (req "source" Signature.Public_key_hash.encoding)
            (req "period" int32)
            (req "proposal" Protocol_hash.encoding)
            (req "ballot" Vote_repr.ballot_encoding);
        select = (function Contents (Ballot _ as op) -> Some op | _ -> None);
        proj =
          (function
          | Ballot {source; period; proposal; ballot} ->
              (source, period, proposal, ballot));
        inj =
          (fun (source, period, proposal, ballot) ->
            Ballot {source; period; proposal; ballot});
      }

  let drain_delegate_case =
    Case
      {
        tag = 9;
        name = "drain_delegate";
        encoding =
          obj3
            (req "consensus_key" Signature.Public_key_hash.encoding)
            (req "delegate" Signature.Public_key_hash.encoding)
            (req "destination" Signature.Public_key_hash.encoding);
        select =
          (function Contents (Drain_delegate _ as op) -> Some op | _ -> None);
        proj =
          (function
          | Drain_delegate {consensus_key; delegate; destination} ->
              (consensus_key, delegate, destination));
        inj =
          (fun (consensus_key, delegate, destination) ->
            Drain_delegate {consensus_key; delegate; destination});
      }

  let failing_noop_case =
    Case
      {
        tag = 17;
        name = "failing_noop";
        encoding = obj1 (req "arbitrary" (string Hex));
        select =
          (function Contents (Failing_noop _ as op) -> Some op | _ -> None);
        proj = (function Failing_noop message -> message);
        inj = (function message -> Failing_noop message);
      }

  let manager_encoding =
    obj5
      (req "source" Signature.Public_key_hash.encoding)
      (req "fee" Tez_repr.encoding)
      (req "counter" Manager_counter_repr.encoding_for_operation)
      (req "gas_limit" (check_size 10 Gas_limit_repr.Arith.n_integral_encoding))
      (req "storage_limit" (check_size 10 n))

  let extract : type kind. kind Kind.manager contents -> _ = function
    | Manager_operation
        {source; fee; counter; gas_limit; storage_limit; operation = _} ->
        (source, fee, counter, gas_limit, storage_limit)

  let rebuild (source, fee, counter, gas_limit, storage_limit) operation =
    Manager_operation
      {source; fee; counter; gas_limit; storage_limit; operation}

  let make_manager_case tag (type kind)
      (Manager_operations.MCase mcase : kind Manager_operations.case) =
    Case
      {
        tag;
        name = mcase.name;
        encoding = merge_objs manager_encoding mcase.encoding;
        select =
          (function
          | Contents (Manager_operation ({operation; _} as op)) -> (
              match mcase.select (Manager operation) with
              | None -> None
              | Some operation -> Some (Manager_operation {op with operation}))
          | _ -> None);
        proj =
          (function
          | Manager_operation {operation; _} as op ->
              (extract op, mcase.proj operation));
        inj = (fun (op, contents) -> rebuild op (mcase.inj contents));
      }

  let reveal_case = make_manager_case 107 Manager_operations.reveal_case

  let transaction_case =
    make_manager_case 108 Manager_operations.transaction_case

  let origination_case =
    make_manager_case 109 Manager_operations.origination_case

  let delegation_case = make_manager_case 110 Manager_operations.delegation_case

  let register_global_constant_case =
    make_manager_case 111 Manager_operations.register_global_constant_case

  (* 112 was for Set_deposits_limit. *)

  let increase_paid_storage_case =
    make_manager_case 113 Manager_operations.increase_paid_storage_case

  let update_consensus_key_case =
    make_manager_case 114 Manager_operations.update_consensus_key_case

  let transfer_ticket_case =
    make_manager_case
      transfer_ticket_tag
      Manager_operations.transfer_ticket_case

  let dal_publish_slot_header_case =
    make_manager_case
      dal_publish_slot_header_tag
      Manager_operations.dal_publish_slot_header_case

  let sc_rollup_originate_case =
    make_manager_case
      sc_rollup_operation_origination_tag
      Manager_operations.sc_rollup_originate_case

  let sc_rollup_add_messages_case =
    make_manager_case
      sc_rollup_operation_add_message_tag
      Manager_operations.sc_rollup_add_messages_case

  let sc_rollup_cement_case =
    make_manager_case
      sc_rollup_operation_cement_tag
      Manager_operations.sc_rollup_cement_case

  let sc_rollup_publish_case =
    make_manager_case
      sc_rollup_operation_publish_tag
      Manager_operations.sc_rollup_publish_case

  let sc_rollup_refute_case =
    make_manager_case
      sc_rollup_operation_refute_tag
      Manager_operations.sc_rollup_refute_case

  let sc_rollup_timeout_case =
    make_manager_case
      sc_rollup_operation_timeout_tag
      Manager_operations.sc_rollup_timeout_case

  let sc_rollup_execute_outbox_message_case =
    make_manager_case
      sc_rollup_execute_outbox_message_tag
      Manager_operations.sc_rollup_execute_outbox_message_case

  let sc_rollup_recover_bond_case =
    make_manager_case
      sc_rollup_operation_recover_bond_tag
      Manager_operations.sc_rollup_recover_bond_case

  let zk_rollup_origination_case =
    make_manager_case
      zk_rollup_operation_create_tag
      Manager_operations.zk_rollup_origination_case

  let zk_rollup_publish_case =
    make_manager_case
      zk_rollup_operation_publish_tag
      Manager_operations.zk_rollup_publish_case

  let zk_rollup_update_case =
    make_manager_case
      zk_rollup_operation_update_tag
      Manager_operations.zk_rollup_update_case

  type packed_case = PCase : 'b case -> packed_case

  let common_cases =
    [
      PCase dal_attestation_case;
      PCase seed_nonce_revelation_case;
      PCase vdf_revelation_case;
      PCase double_baking_evidence_case;
      PCase activate_account_case;
      PCase proposals_case;
      PCase ballot_case;
      PCase reveal_case;
      PCase transaction_case;
      PCase origination_case;
      PCase delegation_case;
      PCase increase_paid_storage_case;
      PCase update_consensus_key_case;
      PCase drain_delegate_case;
      PCase failing_noop_case;
      PCase register_global_constant_case;
      PCase transfer_ticket_case;
      PCase dal_publish_slot_header_case;
      PCase sc_rollup_originate_case;
      PCase sc_rollup_add_messages_case;
      PCase sc_rollup_cement_case;
      PCase sc_rollup_publish_case;
      PCase sc_rollup_refute_case;
      PCase sc_rollup_timeout_case;
      PCase sc_rollup_execute_outbox_message_case;
      PCase sc_rollup_recover_bond_case;
      PCase zk_rollup_origination_case;
      PCase zk_rollup_publish_case;
      PCase zk_rollup_update_case;
    ]

  let contents_cases =
    PCase preattestation_case :: PCase attestation_case
    :: PCase double_preattestation_evidence_case
    :: PCase double_attestation_evidence_case :: common_cases

  let contents_cases_with_legacy_attestation_name =
    PCase preendorsement_case :: PCase endorsement_case
    :: PCase double_preendorsement_evidence_case
    :: PCase double_endorsement_evidence_case :: common_cases

  let contents_encoding =
    let make (PCase (Case {tag; name; encoding; select; proj; inj})) =
      assert (not @@ reserved_tag tag) ;
      case
        (Tag tag)
        name
        encoding
        (fun o -> match select o with None -> None | Some o -> Some (proj o))
        (fun x -> Contents (inj x))
    in
    def "operation.alpha.contents" @@ union (List.map make contents_cases)

  let contents_encoding_with_legacy_attestation_name =
    let make (PCase (Case {tag; name; encoding; select; proj; inj})) =
      assert (not @@ reserved_tag tag) ;
      case
        (Tag tag)
        name
        encoding
        (fun o -> match select o with None -> None | Some o -> Some (proj o))
        (fun x -> Contents (inj x))
    in
    def "operation_with_legacy_attestation_name.alpha.contents"
    @@ union (List.map make contents_cases_with_legacy_attestation_name)

  let contents_list_encoding =
    conv_with_guard to_list of_list_internal (Variable.list contents_encoding)

  let contents_list_encoding_with_legacy_attestation_name =
    conv_with_guard
      to_list
      of_list_internal
      (Variable.list contents_encoding_with_legacy_attestation_name)

  let protocol_data_json_encoding =
    conv
      (fun (Operation_data {contents; signature}) ->
        (Contents_list contents, signature))
      (fun (Contents_list contents, signature) ->
        Operation_data {contents; signature})
      (obj2
         (req "contents" (dynamic_size contents_list_encoding))
         (opt "signature" Signature.encoding))

  let protocol_data_json_encoding_with_legacy_attestation_name =
    conv
      (fun (Operation_data {contents; signature}) ->
        (Contents_list contents, signature))
      (fun (Contents_list contents, signature) ->
        Operation_data {contents; signature})
      (obj2
         (req
            "contents"
            (dynamic_size contents_list_encoding_with_legacy_attestation_name))
         (opt "signature" Signature.encoding))

  type contents_or_signature_prefix =
    | Actual_contents of packed_contents
    | Signature_prefix of Signature.prefix

  let contents_or_signature_prefix_encoding =
    let make_contents (PCase (Case {tag; name; encoding; select; proj; inj})) =
      assert (not @@ reserved_tag tag) ;
      case
        (Tag tag)
        name
        encoding
        (function
          | Actual_contents o -> (
              match select o with None -> None | Some o -> Some (proj o))
          | _ -> None)
        (fun x -> Actual_contents (Contents (inj x)))
    in
    def "operation.alpha.contents_or_signature_prefix"
    @@ union
    @@ case
         (Tag signature_prefix_tag)
         "signature_prefix"
         (obj1 (req "signature_prefix" Signature.prefix_encoding))
         (function Signature_prefix prefix -> Some prefix | _ -> None)
         (fun prefix -> Signature_prefix prefix)
       (* The case signature_prefix is added to the operation's contents so that
          we can store the prefix of BLS signatures without breaking the
          encoding of operations. *)
       :: List.map make_contents contents_cases

  let of_contents_and_signature_prefix contents_and_prefix =
    let open Result_syntax in
    let rec loop acc = function
      | [] -> Ok acc
      | Signature_prefix _ :: _ -> Error "Signature prefix must appear last"
      | Actual_contents (Contents o) :: os -> (
          match (o, acc) with
          | ( Manager_operation _,
              Contents_list (Single (Manager_operation _) as rest) ) ->
              (loop [@tailcall]) (Contents_list (Cons (o, rest))) os
          | Manager_operation _, Contents_list (Cons _ as rest) ->
              (loop [@tailcall]) (Contents_list (Cons (o, rest))) os
          | _ ->
              Error
                "Operation list of length > 1 should only contain manager \
                 operations.")
    in
    let rev_contents, prefix =
      match List.rev contents_and_prefix with
      | Signature_prefix prefix :: rev_contents -> (rev_contents, Some prefix)
      | rev_contents -> (rev_contents, None)
    in
    let+ packed_contents =
      match rev_contents with
      | [] -> Error "Operation lists should not be empty."
      | Signature_prefix _ :: _ -> Error "Signature prefix must appear last"
      | Actual_contents (Contents o) :: os -> loop (Contents_list (Single o)) os
    in
    (packed_contents, prefix)

  let protocol_data_binary_encoding =
    conv_with_guard
      (fun (Operation_data {contents; signature}) ->
        let contents_list =
          List.map (fun c -> Actual_contents c)
          @@ to_list (Contents_list contents)
        in
        let contents_and_signature_prefix, sig_suffix =
          match signature with
          | None -> (contents_list, Signature.(to_bytes zero))
          | Some signature -> (
              let {Signature.prefix; suffix} =
                Signature.split_signature signature
              in
              match prefix with
              | None -> (contents_list, suffix)
              | Some prefix ->
                  (contents_list @ [Signature_prefix prefix], suffix))
        in
        (contents_and_signature_prefix, sig_suffix))
      (fun (contents_and_signature_prefix, suffix) ->
        let open Result_syntax in
        let* Contents_list contents, prefix =
          of_contents_and_signature_prefix contents_and_signature_prefix
        in
        let+ signature =
          Result.of_option ~error:"Invalid signature"
          @@ Signature.of_splitted {Signature.prefix; suffix}
        in
        let signature =
          match prefix with
          | None ->
              if Signature.(signature = zero) then None else Some signature
          | Some _ -> Some signature
        in
        Operation_data {contents; signature})
      (obj2
         (req
            "contents_and_signature_prefix"
            (Variable.list contents_or_signature_prefix_encoding))
         (req "signature_suffix" (Fixed.bytes Hex 64)))

  (* The binary and JSON encodings are different for protocol data, because we
     have to fit BLS signatures (which are 96 bytes long) in a backward
     compatible manner with fixed size signatures of 64 bytes.

     The JSON encoding is the same as in the previous protocols.

     To support BLS signatures, we extract the prefix of the signature and fit
     it inside the field [contents] while keeping the 64 bytes suffix in the
     same place as the other signature kinds (i.e. at the end).

     For instance the binary protocol data for a transfer operation signed by a
     Ed25519 key would look like:

     +----------------+------------+
     |  Transaction   | signature  |
     +----+------+----+------------+
     | 6C |  ... | 00 | (64 bytes) |
     +----+------+----+------------+

     The same transfer signed by a BLS key would be instead:

     +----------------+----------------------------+-------------------+
     |  Transaction   |      signature prefix      | signature suffix  |
     +----+------+----+----+----+------------------+-------------------+
     | 6C |  ... | 00 | ff | 03 | (first 32 bytes) | (last 64 bytes)   |
     +----+------+----+----+----+------------------+-------------------+

     Which can also be viewed with an equivalent schema:

     +----------------+----+---------------+--------------------------+
     |  Transaction   | ff | signature tag |        signature         |
     +----+------+----+----+---------------+--------------------------+
     | 6C |  ... | 00 | ff |   03 (BLS)    | (96 bytes BLS signature) |
     +----+------+----+----+---------------+--------------------------+

     NOTE: BLS only supports the tagged format and Ed25519, Secp256k1 and P256
     signatures only support the untagged one. The latter restriction is only
     here to guarantee unicity of the binary representation for signatures.
  *)
  let protocol_data_encoding =
    def "operation.alpha.contents_and_signature"
    @@ splitted
         ~json:protocol_data_json_encoding
         ~binary:protocol_data_binary_encoding

  let protocol_data_encoding_with_legacy_attestation_name =
    def "operation_with_legacy_attestation_name.alpha.contents_and_signature"
    @@ splitted
         ~json:protocol_data_json_encoding_with_legacy_attestation_name
         ~binary:protocol_data_binary_encoding

  let operation_encoding =
    conv
      (fun {shell; protocol_data} -> (shell, protocol_data))
      (fun (shell, protocol_data) -> {shell; protocol_data})
      (merge_objs Operation.shell_header_encoding protocol_data_encoding)

  let operation_encoding_with_legacy_attestation_name =
    conv
      (fun {shell; protocol_data} -> (shell, protocol_data))
      (fun (shell, protocol_data) -> {shell; protocol_data})
      (merge_objs
         Operation.shell_header_encoding
         protocol_data_encoding_with_legacy_attestation_name)

  let unsigned_operation_encoding =
    def "operation.alpha.unsigned_operation"
    @@ merge_objs
         Operation.shell_header_encoding
         (obj1 (req "contents" contents_list_encoding))

  let unsigned_operation_encoding_with_legacy_attestation_name =
    def "operation_with_legacy_attestation_name.alpha.unsigned_operation"
    @@ merge_objs
         Operation.shell_header_encoding
         (obj1
            (req "contents" contents_list_encoding_with_legacy_attestation_name))
end

let encoding = Encoding.operation_encoding

let encoding_with_legacy_attestation_name =
  Encoding.operation_encoding_with_legacy_attestation_name

let contents_encoding = Encoding.contents_encoding

let contents_encoding_with_legacy_attestation_name =
  Encoding.contents_encoding_with_legacy_attestation_name

let contents_list_encoding = Encoding.contents_list_encoding

let contents_list_encoding_with_legacy_attestation_name =
  Encoding.contents_list_encoding_with_legacy_attestation_name

let protocol_data_encoding = Encoding.protocol_data_encoding

let protocol_data_encoding_with_legacy_attestation_name =
  Encoding.protocol_data_encoding_with_legacy_attestation_name

let unsigned_operation_encoding = Encoding.unsigned_operation_encoding

let unsigned_operation_encoding_with_legacy_attestation_name =
  Encoding.unsigned_operation_encoding_with_legacy_attestation_name

let raw ({shell; protocol_data} : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data protocol_data)
  in
  {Operation.shell; proto}

(** Each operation belongs to a validation pass that is an integer
   abstracting its priority in a block. Except Failing_noop. *)

let consensus_pass = 0

let voting_pass = 1

let anonymous_pass = 2

let manager_pass = 3

(** [acceptable_pass op] returns either the validation_pass of [op]
   when defines and None when [op] is [Failing_noop]. *)
let acceptable_pass (op : packed_operation) =
  let (Operation_data protocol_data) = op.protocol_data in
  match protocol_data.contents with
  | Single (Failing_noop _) -> None
  | Single (Preendorsement _) -> Some consensus_pass
  | Single (Endorsement _) -> Some consensus_pass
  | Single (Dal_attestation _) -> Some consensus_pass
  | Single (Proposals _) -> Some voting_pass
  | Single (Ballot _) -> Some voting_pass
  | Single (Seed_nonce_revelation _) -> Some anonymous_pass
  | Single (Vdf_revelation _) -> Some anonymous_pass
  | Single (Double_endorsement_evidence _) -> Some anonymous_pass
  | Single (Double_preendorsement_evidence _) -> Some anonymous_pass
  | Single (Double_baking_evidence _) -> Some anonymous_pass
  | Single (Activate_account _) -> Some anonymous_pass
  | Single (Drain_delegate _) -> Some anonymous_pass
  | Single (Manager_operation _) -> Some manager_pass
  | Cons (Manager_operation _, _ops) -> Some manager_pass

(** [compare_by_passes] orders two operations in the reverse order of
   their acceptable passes. *)
let compare_by_passes op1 op2 =
  match (acceptable_pass op1, acceptable_pass op2) with
  | Some op1_pass, Some op2_pass -> Compare.Int.compare op2_pass op1_pass
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0

type error += Invalid_signature (* `Permanent *)

type error += Missing_signature (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"operation.invalid_signature"
    ~title:"Invalid operation signature"
    ~description:
      "The operation signature is ill-formed or has been made with the wrong \
       public key"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation signature is invalid")
    Data_encoding.unit
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature) ;
  register_error_kind
    `Permanent
    ~id:"operation.missing_signature"
    ~title:"Missing operation signature"
    ~description:
      "The operation is of a kind that must be signed, but the signature is \
       missing"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation requires a signature")
    Data_encoding.unit
    (function Missing_signature -> Some () | _ -> None)
    (fun () -> Missing_signature) ;
  register_error_kind
    `Permanent
    ~id:"operation.contents_list_error"
    ~title:"Invalid list of operation contents."
    ~description:
      "An operation contents list has an unexpected shape; it should be either \
       a single operation or a non-empty list of manager operations"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "An operation contents list has an unexpected shape: %s"
        s)
    Data_encoding.(obj1 (req "message" (string Hex)))
    (function Contents_list_error s -> Some s | _ -> None)
    (fun s -> Contents_list_error s)

let serialize_unsigned_operation (type kind)
    ({shell; protocol_data} : kind operation) : bytes =
  Data_encoding.Binary.to_bytes_exn
    unsigned_operation_encoding
    (shell, Contents_list protocol_data.contents)

let unsigned_operation_length (type kind)
    ({shell; protocol_data} : kind operation) : int =
  Data_encoding.Binary.length
    unsigned_operation_encoding
    (shell, Contents_list protocol_data.contents)

let check_signature (type kind) key chain_id (op : kind operation) =
  let serialized_operation = serialize_unsigned_operation op in
  let check ~watermark signature =
    if Signature.check ~watermark key signature serialized_operation then Ok ()
    else error Invalid_signature
  in
  match op.protocol_data.signature with
  | None -> error Missing_signature
  | Some signature ->
      let watermark =
        match op.protocol_data.contents with
        | Single (Preendorsement _) -> to_watermark (Preendorsement chain_id)
        | Single (Endorsement _) -> to_watermark (Endorsement chain_id)
        | Single (Dal_attestation _) -> to_watermark (Dal_attestation chain_id)
        | Single
            ( Failing_noop _ | Proposals _ | Ballot _ | Seed_nonce_revelation _
            | Vdf_revelation _ | Double_endorsement_evidence _
            | Double_preendorsement_evidence _ | Double_baking_evidence _
            | Activate_account _ | Drain_delegate _ | Manager_operation _ ) ->
            Generic_operation
        | Cons (Manager_operation _, _ops) -> Generic_operation
      in
      check ~watermark signature

let hash_raw = Operation.hash

let hash (o : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data o.protocol_data)
  in
  Operation.hash {shell = o.shell; proto}

let hash_packed (o : packed_operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn protocol_data_encoding o.protocol_data
  in
  Operation.hash {shell = o.shell; proto}

type ('a, 'b) eq = Eq : ('a, 'a) eq

let equal_manager_operation_kind :
    type a b. a manager_operation -> b manager_operation -> (a, b) eq option =
 fun op1 op2 ->
  match (op1, op2) with
  | Reveal _, Reveal _ -> Some Eq
  | Reveal _, _ -> None
  | Transaction _, Transaction _ -> Some Eq
  | Transaction _, _ -> None
  | Origination _, Origination _ -> Some Eq
  | Origination _, _ -> None
  | Delegation _, Delegation _ -> Some Eq
  | Delegation _, _ -> None
  | Register_global_constant _, Register_global_constant _ -> Some Eq
  | Register_global_constant _, _ -> None
  | Increase_paid_storage _, Increase_paid_storage _ -> Some Eq
  | Increase_paid_storage _, _ -> None
  | Update_consensus_key _, Update_consensus_key _ -> Some Eq
  | Update_consensus_key _, _ -> None
  | Transfer_ticket _, Transfer_ticket _ -> Some Eq
  | Transfer_ticket _, _ -> None
  | Dal_publish_slot_header _, Dal_publish_slot_header _ -> Some Eq
  | Dal_publish_slot_header _, _ -> None
  | Sc_rollup_originate _, Sc_rollup_originate _ -> Some Eq
  | Sc_rollup_originate _, _ -> None
  | Sc_rollup_add_messages _, Sc_rollup_add_messages _ -> Some Eq
  | Sc_rollup_add_messages _, _ -> None
  | Sc_rollup_cement _, Sc_rollup_cement _ -> Some Eq
  | Sc_rollup_cement _, _ -> None
  | Sc_rollup_publish _, Sc_rollup_publish _ -> Some Eq
  | Sc_rollup_publish _, _ -> None
  | Sc_rollup_refute _, Sc_rollup_refute _ -> Some Eq
  | Sc_rollup_refute _, _ -> None
  | Sc_rollup_timeout _, Sc_rollup_timeout _ -> Some Eq
  | Sc_rollup_timeout _, _ -> None
  | Sc_rollup_execute_outbox_message _, Sc_rollup_execute_outbox_message _ ->
      Some Eq
  | Sc_rollup_execute_outbox_message _, _ -> None
  | Sc_rollup_recover_bond _, Sc_rollup_recover_bond _ -> Some Eq
  | Sc_rollup_recover_bond _, _ -> None
  | Zk_rollup_origination _, Zk_rollup_origination _ -> Some Eq
  | Zk_rollup_origination _, _ -> None
  | Zk_rollup_publish _, Zk_rollup_publish _ -> Some Eq
  | Zk_rollup_publish _, _ -> None
  | Zk_rollup_update _, Zk_rollup_update _ -> Some Eq
  | Zk_rollup_update _, _ -> None

let equal_contents_kind : type a b. a contents -> b contents -> (a, b) eq option
    =
 fun op1 op2 ->
  match (op1, op2) with
  | Preendorsement _, Preendorsement _ -> Some Eq
  | Preendorsement _, _ -> None
  | Endorsement _, Endorsement _ -> Some Eq
  | Endorsement _, _ -> None
  | Dal_attestation _, Dal_attestation _ -> Some Eq
  | Dal_attestation _, _ -> None
  | Seed_nonce_revelation _, Seed_nonce_revelation _ -> Some Eq
  | Seed_nonce_revelation _, _ -> None
  | Vdf_revelation _, Vdf_revelation _ -> Some Eq
  | Vdf_revelation _, _ -> None
  | Double_endorsement_evidence _, Double_endorsement_evidence _ -> Some Eq
  | Double_endorsement_evidence _, _ -> None
  | Double_preendorsement_evidence _, Double_preendorsement_evidence _ ->
      Some Eq
  | Double_preendorsement_evidence _, _ -> None
  | Double_baking_evidence _, Double_baking_evidence _ -> Some Eq
  | Double_baking_evidence _, _ -> None
  | Activate_account _, Activate_account _ -> Some Eq
  | Activate_account _, _ -> None
  | Proposals _, Proposals _ -> Some Eq
  | Proposals _, _ -> None
  | Ballot _, Ballot _ -> Some Eq
  | Ballot _, _ -> None
  | Drain_delegate _, Drain_delegate _ -> Some Eq
  | Drain_delegate _, _ -> None
  | Failing_noop _, Failing_noop _ -> Some Eq
  | Failing_noop _, _ -> None
  | Manager_operation op1, Manager_operation op2 -> (
      match equal_manager_operation_kind op1.operation op2.operation with
      | None -> None
      | Some Eq -> Some Eq)
  | Manager_operation _, _ -> None

let rec equal_contents_kind_list :
    type a b. a contents_list -> b contents_list -> (a, b) eq option =
 fun op1 op2 ->
  match (op1, op2) with
  | Single op1, Single op2 -> equal_contents_kind op1 op2
  | Single _, Cons _ -> None
  | Cons _, Single _ -> None
  | Cons (op1, ops1), Cons (op2, ops2) -> (
      match equal_contents_kind op1 op2 with
      | None -> None
      | Some Eq -> (
          match equal_contents_kind_list ops1 ops2 with
          | None -> None
          | Some Eq -> Some Eq))

let equal : type a b. a operation -> b operation -> (a, b) eq option =
 fun op1 op2 ->
  if not (Operation_hash.equal (hash op1) (hash op2)) then None
  else
    equal_contents_kind_list
      op1.protocol_data.contents
      op2.protocol_data.contents

(** {2 Comparing operations} *)

(** Precondition: both operations are [valid]. Hence, it is possible
   to compare them without any state representation. *)

(** {3 Operation passes} *)

type consensus_pass_type

type voting_pass_type

type anonymous_pass_type

type manager_pass_type

type noop_pass_type

type _ pass =
  | Consensus : consensus_pass_type pass
  | Voting : voting_pass_type pass
  | Anonymous : anonymous_pass_type pass
  | Manager : manager_pass_type pass
  | Noop : noop_pass_type pass

(** Pass comparison. *)
let compare_inner_pass : type a b. a pass -> b pass -> int =
 fun pass1 pass2 ->
  match (pass1, pass2) with
  | Consensus, (Voting | Anonymous | Manager | Noop) -> 1
  | (Voting | Anonymous | Manager | Noop), Consensus -> -1
  | Voting, (Anonymous | Manager | Noop) -> 1
  | (Anonymous | Manager | Noop), Voting -> -1
  | Anonymous, (Manager | Noop) -> 1
  | (Manager | Noop), Anonymous -> -1
  | Manager, Noop -> 1
  | Noop, Manager -> -1
  | Consensus, Consensus
  | Voting, Voting
  | Anonymous, Anonymous
  | Manager, Manager
  | Noop, Noop ->
      0

(** {3 Operation weights} *)

(** [round_infos] is the pair of a [level] convert into {!int32} and
   [round] convert into an {!int}.

   By convention, if the [round] is from an operation round that
   failed to convert in a {!int}, the value of [round] is (-1). *)
type round_infos = {level : int32; round : int}

(** [endorsement_infos] is the pair of a {!round_infos} and a [slot]
   convert into an {!int}. *)
type endorsement_infos = {round : round_infos; slot : int}

(** [double_baking_infos] is the pair of a {!round_infos} and a
    {!block_header} hash. *)
type double_baking_infos = {round : round_infos; bh_hash : Block_hash.t}

(** Compute a {!round_infos} from a {consensus_content} of a valid
   operation. Hence, the [round] must convert in {!int}.

    Precondition: [c] comes from a valid operation. The [round] from a
   valid operation should succeed to convert in {!int}. Hence, for the
   unreachable path where the convertion failed, we put (-1) as
   [round] value. *)
let round_infos_from_consensus_content (c : consensus_content) =
  let level = Raw_level_repr.to_int32 c.level in
  match Round_repr.to_int c.round with
  | Ok round -> {level; round}
  | Error _ -> {level; round = -1}

(** Compute a {!endorsement_infos} from a {!consensus_content}. It is
   used to compute the weight of {!Endorsement} and {!Preendorsement}.

    Precondition: [c] comes from a valid operation. The {!Endorsement}
   or {!Preendorsement} is valid, so its [round] must succeed to
   convert into an {!int}. Hence, for the unreachable path where the
   convertion fails, we put (-1) as [round] value (see
   {!round_infos_from_consensus_content}). *)
let endorsement_infos_from_consensus_content (c : consensus_content) =
  let slot = Slot_repr.to_int c.slot in
  let round = round_infos_from_consensus_content c in
  {round; slot}

(** Compute a {!double_baking_infos} and a {!Block_header_repr.hash}
   from a {!Block_header_repr.t}. It is used to compute the weight of
   a {!Double_baking_evidence}.

   Precondition: [bh] comes from a valid operation. The
   {!Double_baking_envidence} is valid, so its fitness from its first
   denounced block header must succeed, and the round from this
   fitness must convert in a {!int}. Hence, for the unreachable paths
   where either the convertion fails or the fitness is not
   retrievable, we put (-1) as [round] value. *)
let consensus_infos_and_hash_from_block_header (bh : Block_header_repr.t) =
  let level = bh.shell.level in
  let bh_hash = Block_header_repr.hash bh in
  let round =
    match Fitness_repr.from_raw bh.shell.fitness with
    | Ok bh_fitness -> (
        match Round_repr.to_int (Fitness_repr.round bh_fitness) with
        | Ok round -> {level; round}
        | Error _ -> {level; round = -1})
    | Error _ -> {level; round = -1}
  in
  {round; bh_hash}

(** The weight of an operation.

   Given an operation, its [weight] carries on static information that
   is used to compare it to an operation of the same pass.
    Operation weight are defined by validation pass.

    The [weight] of an {!Endorsement} or {!Preendorsement} depends on
   its {!endorsement_infos}.

    The [weight] of a {!Dal_attestation} depends on the pair of
   the size of its bitset, {!Dal_attestation_repr.t}, and the
   signature of its attestor {! Signature.Public_key_hash.t}.

   The [weight] of a voting operation depends on the pair of its
   [period] and [source].

   The [weight] of a {!Vdf_revelation} depends on its [solution].

   The [weight] of a {!Seed_nonce_revelation} depends on its [level]
   converted in {!int32}.

    The [weight] of a {!Double_preendorsement} or
   {!Double_endorsement} depends on the [level] and [round] of their
   first denounciated operations. The [level] and [round] are wrapped
   in a {!round_infos}.

    The [weight] of a {!Double_baking} depends on the [level], [round]
   and [hash] of its first denounciated block_header. the [level] and
   [round] are wrapped in a {!double_baking_infos}.

    The [weight] of an {!Activate_account} depends on its public key
   hash.

    The [weight] of an {!Drain_delegate} depends on the public key
   hash of the delegate.

    The [weight] of {!Manager_operation} depends on its [fee] and
   [gas_limit] ratio expressed in {!Q.t}. *)
type _ weight =
  | Weight_endorsement : endorsement_infos -> consensus_pass_type weight
  | Weight_preendorsement : endorsement_infos -> consensus_pass_type weight
  | Weight_dal_attestation :
      (* attestor * num_attestations * level *)
      (Signature.Public_key_hash.t * int * int32)
      -> consensus_pass_type weight
  | Weight_proposals :
      int32 * Signature.Public_key_hash.t
      -> voting_pass_type weight
  | Weight_ballot :
      int32 * Signature.Public_key_hash.t
      -> voting_pass_type weight
  | Weight_seed_nonce_revelation : int32 -> anonymous_pass_type weight
  | Weight_vdf_revelation : Seed_repr.vdf_solution -> anonymous_pass_type weight
  | Weight_double_preendorsement : round_infos -> anonymous_pass_type weight
  | Weight_double_endorsement : round_infos -> anonymous_pass_type weight
  | Weight_double_baking : double_baking_infos -> anonymous_pass_type weight
  | Weight_activate_account :
      Ed25519.Public_key_hash.t
      -> anonymous_pass_type weight
  | Weight_drain_delegate :
      Signature.Public_key_hash.t
      -> anonymous_pass_type weight
  | Weight_manager : Q.t * Signature.public_key_hash -> manager_pass_type weight
  | Weight_noop : noop_pass_type weight

(** The weight of an operation is the pair of its pass and weight. *)
type operation_weight = W : 'pass pass * 'pass weight -> operation_weight

(** The {!weight} of a batch of {!Manager_operation} depends on the
   sum of all [fee] and the sum of all [gas_limit].

    Precondition: [op] is a valid manager operation: its sum
    of accumulated [fee] must succeed. Hence, in the unreachable path where
    the [fee] sum fails, we put [Tez_repr.zero] as its value. *)
let cumulate_fee_and_gas_of_manager :
    type kind.
    kind Kind.manager contents_list ->
    Tez_repr.t * Gas_limit_repr.Arith.integral =
 fun op ->
  let add_without_error acc y =
    match Tez_repr.(acc +? y) with
    | Ok v -> v
    | Error _ -> (* This cannot happen *) acc
  in
  let rec loop :
      type kind. 'a -> 'b -> kind Kind.manager contents_list -> 'a * 'b =
   fun fees_acc gas_limit_acc -> function
    | Single (Manager_operation {fee; gas_limit; _}) ->
        let total_fees = add_without_error fees_acc fee in
        let total_gas_limit =
          Gas_limit_repr.Arith.add gas_limit_acc gas_limit
        in
        (total_fees, total_gas_limit)
    | Cons (Manager_operation {fee; gas_limit; _}, manops) ->
        let fees_acc = add_without_error fees_acc fee in
        let gas_limit_acc = Gas_limit_repr.Arith.add gas_limit gas_limit_acc in
        loop fees_acc gas_limit_acc manops
  in
  loop Tez_repr.zero Gas_limit_repr.Arith.zero op

(** The {!weight} of a {!Manager_operation} as well as a batch of
   operations is the ratio in {!int64} between its [fee] and
   [gas_limit] as computed by
   {!cumulate_fee_and_gas_of_manager} converted in {!Q.t}.
   We assume that the manager operation valid, thus its gas limit can
   never be zero. We treat this case the same as gas_limit = 1 for the
   sake of simplicity.
*)
let weight_manager :
    type kind.
    kind Kind.manager contents_list -> Q.t * Signature.public_key_hash =
 fun op ->
  let fee, glimit = cumulate_fee_and_gas_of_manager op in
  let source =
    match op with
    | Cons (Manager_operation {source; _}, _) -> source
    | Single (Manager_operation {source; _}) -> source
  in
  let fee_f = Q.of_int64 (Tez_repr.to_mutez fee) in
  if Gas_limit_repr.Arith.(glimit = Gas_limit_repr.Arith.zero) then
    (fee_f, source)
  else
    let gas_f = Q.of_bigint (Gas_limit_repr.Arith.integral_to_z glimit) in
    (Q.(fee_f / gas_f), source)

(** Computing the {!operation_weight} of an operation. [weight_of
   (Failing_noop _)] is unreachable, for completness we define a
   Weight_noop which carrries no information. *)
let weight_of : packed_operation -> operation_weight =
 fun op ->
  let (Operation_data protocol_data) = op.protocol_data in
  match protocol_data.contents with
  | Single (Failing_noop _) -> W (Noop, Weight_noop)
  | Single (Preendorsement consensus_content) ->
      W
        ( Consensus,
          Weight_preendorsement
            (endorsement_infos_from_consensus_content consensus_content) )
  | Single (Endorsement consensus_content) ->
      W
        ( Consensus,
          Weight_endorsement
            (endorsement_infos_from_consensus_content consensus_content) )
  | Single (Dal_attestation Dal_attestation_repr.{attestor; attestation; level})
    ->
      W
        ( Consensus,
          Weight_dal_attestation
            ( attestor,
              Dal_attestation_repr.occupied_size_in_bits attestation,
              Raw_level_repr.to_int32 level ) )
  | Single (Proposals {period; source; _}) ->
      W (Voting, Weight_proposals (period, source))
  | Single (Ballot {period; source; _}) ->
      W (Voting, Weight_ballot (period, source))
  | Single (Seed_nonce_revelation {level; _}) ->
      W (Anonymous, Weight_seed_nonce_revelation (Raw_level_repr.to_int32 level))
  | Single (Vdf_revelation {solution}) ->
      W (Anonymous, Weight_vdf_revelation solution)
  | Single (Double_endorsement_evidence {op1; _}) -> (
      match op1.protocol_data.contents with
      | Single (Endorsement consensus_content) ->
          W
            ( Anonymous,
              Weight_double_endorsement
                (round_infos_from_consensus_content consensus_content) ))
  | Single (Double_preendorsement_evidence {op1; _}) -> (
      match op1.protocol_data.contents with
      | Single (Preendorsement consensus_content) ->
          W
            ( Anonymous,
              Weight_double_preendorsement
                (round_infos_from_consensus_content consensus_content) ))
  | Single (Double_baking_evidence {bh1; _}) ->
      let double_baking_infos =
        consensus_infos_and_hash_from_block_header bh1
      in
      W (Anonymous, Weight_double_baking double_baking_infos)
  | Single (Activate_account {id; _}) ->
      W (Anonymous, Weight_activate_account id)
  | Single (Drain_delegate {delegate; _}) ->
      W (Anonymous, Weight_drain_delegate delegate)
  | Single (Manager_operation _) as ops ->
      let manweight, src = weight_manager ops in
      W (Manager, Weight_manager (manweight, src))
  | Cons (Manager_operation _, _) as ops ->
      let manweight, src = weight_manager ops in
      W (Manager, Weight_manager (manweight, src))

(** {3 Comparisons of operations {!weight}} *)

(** {4 Helpers} *)

(** compare a pair of elements in lexicographic order. *)
let compare_pair_in_lexico_order ~cmp_fst ~cmp_snd (a1, b1) (a2, b2) =
  let resa = cmp_fst a1 a2 in
  if Compare.Int.(resa <> 0) then resa else cmp_snd b1 b2

(** compare in reverse order. *)
let compare_reverse (cmp : 'a -> 'a -> int) a b = cmp b a

(** {4 Comparison of {!consensus_infos}} *)

(** Two {!round_infos} compares as the pair of [level, round] in
   lexicographic order: the one with the greater [level] being the
   greater [round_infos]. When levels are the same, the one with the
   greater [round] being the better.

    The greater {!round_infos} is the farther to the current state
   when part of the weight of a valid consensus operation.

    The best {!round_infos} is the nearer to the current state when
   part of the weight of a valid denunciation.

    In both case, that is the greater according to the lexicographic
   order.

   Precondition: the {!round_infos} are from valid operation. They
   have been computed by either {!round_infos_from_consensus_content}
   or {!consensus_infos_and_hash_from_block_header}. Both input
   parameter from valid operations and put (-1) to the [round] in the
   unreachable path where the original round fails to convert in
   {!int}. *)
let compare_round_infos infos1 infos2 =
  compare_pair_in_lexico_order
    ~cmp_fst:Compare.Int32.compare
    ~cmp_snd:Compare.Int.compare
    (infos1.level, infos1.round)
    (infos2.level, infos2.round)

(** When comparing {!Endorsement} to {!Preendorsement} or
   {!Double_endorsement_evidence} to {!Double_preendorsement}, in case
   of {!round_infos} equality, the position is relevant to compute the
   order. *)
type prioritized_position = Nopos | Fstpos | Sndpos

(** Comparison of two {!round_infos} with priority in case of
   {!round_infos} equality. *)
let compare_round_infos_with_prioritized_position ~prioritized_position infos1
    infos2 =
  let cmp = compare_round_infos infos1 infos2 in
  if Compare.Int.(cmp <> 0) then cmp
  else match prioritized_position with Fstpos -> 1 | Sndpos -> -1 | Nopos -> 0

(** When comparing consensus operation with {!endorsement_infos}, in
   case of equality of their {!round_infos}, either they are of the
   same kind and their [slot] have to be compared in the reverse
   order, otherwise the {!Endorsement} is better and
   [prioritized_position] gives its position. *)
let compare_prioritized_position_or_slot ~prioritized_position =
  match prioritized_position with
  | Nopos -> compare_reverse Compare.Int.compare
  | Fstpos -> fun _ _ -> 1
  | Sndpos -> fun _ _ -> -1

(** Two {!endorsement_infos} are compared by their {!round_infos}.
   When their {!round_infos} are equal, they are compared according to
   their priority or their [slot], see
   {!compare_prioritized_position_or_slot} for more details. *)
let compare_endorsement_infos ~prioritized_position (infos1 : endorsement_infos)
    (infos2 : endorsement_infos) =
  compare_pair_in_lexico_order
    ~cmp_fst:compare_round_infos
    ~cmp_snd:(compare_prioritized_position_or_slot ~prioritized_position)
    (infos1.round, infos1.slot)
    (infos2.round, infos2.slot)

(** Two {!double_baking_infos} are compared as their {!round_infos}.
   When their {!round_infos} are equal, they are compared as the
   hashes of their first denounced block header. *)
let compare_baking_infos infos1 infos2 =
  compare_pair_in_lexico_order
    ~cmp_fst:compare_round_infos
    ~cmp_snd:Block_hash.compare
    (infos1.round, infos1.bh_hash)
    (infos2.round, infos2.bh_hash)

(** Two valid {!Dal_attestation} are compared in the
   lexicographic order of their pairs of bitsets size and attestor
   hash. *)
let compare_dal_attestation (attestor1, endorsements1, level1)
    (attestor2, endorsements2, level2) =
  compare_pair_in_lexico_order
    ~cmp_fst:
      (compare_pair_in_lexico_order
         ~cmp_fst:Compare.Int32.compare
         ~cmp_snd:Compare.Int.compare)
    ~cmp_snd:Signature.Public_key_hash.compare
    ((level1, endorsements1), attestor1)
    ((level2, endorsements2), attestor2)

(** {4 Comparison of valid operations of the same validation pass} *)

(** {5 Comparison of valid consensus operations} *)

(** Comparing consensus operations by their [weight] uses the
   comparison on {!endorsement_infos} for {!Endorsement} and
   {!Preendorsement}: see {!endorsement_infos} for more details.

    {!Dal_attestation} is smaller than the other kinds of
   consensus operations. Two valid {!Dal_attestation} are
   compared by {!compare_dal_attestation}. *)
let compare_consensus_weight w1 w2 =
  match (w1, w2) with
  | Weight_endorsement infos1, Weight_endorsement infos2 ->
      compare_endorsement_infos ~prioritized_position:Nopos infos1 infos2
  | Weight_preendorsement infos1, Weight_preendorsement infos2 ->
      compare_endorsement_infos ~prioritized_position:Nopos infos1 infos2
  | Weight_endorsement infos1, Weight_preendorsement infos2 ->
      compare_endorsement_infos ~prioritized_position:Fstpos infos1 infos2
  | Weight_preendorsement infos1, Weight_endorsement infos2 ->
      compare_endorsement_infos ~prioritized_position:Sndpos infos1 infos2
  | ( Weight_dal_attestation (attestor1, size1, lvl1),
      Weight_dal_attestation (attestor2, size2, lvl2) ) ->
      compare_dal_attestation (attestor1, size1, lvl1) (attestor2, size2, lvl2)
  | Weight_dal_attestation _, (Weight_endorsement _ | Weight_preendorsement _)
    ->
      -1
  | (Weight_endorsement _ | Weight_preendorsement _), Weight_dal_attestation _
    ->
      1

(** {5 Comparison of valid voting operations} *)

(** Two valid voting operations of the same kind are compared in the
   lexicographic order of their pair of [period] and [source]. When
   compared to each other, the {!Proposals} is better. *)
let compare_vote_weight w1 w2 =
  let cmp i1 source1 i2 source2 =
    compare_pair_in_lexico_order
      (i1, source1)
      (i2, source2)
      ~cmp_fst:Compare.Int32.compare
      ~cmp_snd:Signature.Public_key_hash.compare
  in
  match (w1, w2) with
  | Weight_proposals (i1, source1), Weight_proposals (i2, source2) ->
      cmp i1 source1 i2 source2
  | Weight_ballot (i1, source1), Weight_ballot (i2, source2) ->
      cmp i1 source1 i2 source2
  | Weight_ballot _, Weight_proposals _ -> -1
  | Weight_proposals _, Weight_ballot _ -> 1

(** {5 Comparison of valid anonymous operations} *)

(** Comparing two {!Double_endorsement_evidence}, or two
   {!Double_preendorsement_evidence}, or comparing them to each other
   is comparing their {!round_infos}, see {!compare_round_infos} for
   more details.

    Comparing two {!Double_baking_evidence} is comparing as their
   {!double_baking_infos}, see {!compare_double_baking_infos} for more
   details.

   Two {!Seed_nonce_revelation} are compared by their [level].

   Two {!Vdf_revelation} are compared by their [solution].

   Two {!Activate_account} are compared as their [id].

   When comparing different kind of anonymous operations, the order is
   as follows: {!Double_preendorsement_evidence} >
   {!Double_endorsement_evidence} > {!Double_baking_evidence} >
   {!Vdf_revelation} > {!Seed_nonce_revelation} > {!Activate_account}.
   *)
let compare_anonymous_weight w1 w2 =
  match (w1, w2) with
  | Weight_double_preendorsement infos1, Weight_double_preendorsement infos2 ->
      compare_round_infos infos1 infos2
  | Weight_double_preendorsement infos1, Weight_double_endorsement infos2 ->
      compare_round_infos_with_prioritized_position
        ~prioritized_position:Fstpos
        infos1
        infos2
  | Weight_double_endorsement infos1, Weight_double_preendorsement infos2 ->
      compare_round_infos_with_prioritized_position
        ~prioritized_position:Sndpos
        infos1
        infos2
  | Weight_double_endorsement infos1, Weight_double_endorsement infos2 ->
      compare_round_infos infos1 infos2
  | ( ( Weight_double_baking _ | Weight_seed_nonce_revelation _
      | Weight_vdf_revelation _ | Weight_activate_account _
      | Weight_drain_delegate _ ),
      (Weight_double_preendorsement _ | Weight_double_endorsement _) ) ->
      -1
  | ( (Weight_double_preendorsement _ | Weight_double_endorsement _),
      ( Weight_double_baking _ | Weight_seed_nonce_revelation _
      | Weight_vdf_revelation _ | Weight_activate_account _
      | Weight_drain_delegate _ ) ) ->
      1
  | Weight_double_baking infos1, Weight_double_baking infos2 ->
      compare_baking_infos infos1 infos2
  | ( ( Weight_seed_nonce_revelation _ | Weight_vdf_revelation _
      | Weight_activate_account _ | Weight_drain_delegate _ ),
      Weight_double_baking _ ) ->
      -1
  | ( Weight_double_baking _,
      ( Weight_seed_nonce_revelation _ | Weight_vdf_revelation _
      | Weight_activate_account _ | Weight_drain_delegate _ ) ) ->
      1
  | Weight_vdf_revelation solution1, Weight_vdf_revelation solution2 ->
      Seed_repr.compare_vdf_solution solution1 solution2
  | ( ( Weight_seed_nonce_revelation _ | Weight_activate_account _
      | Weight_drain_delegate _ ),
      Weight_vdf_revelation _ ) ->
      -1
  | ( Weight_vdf_revelation _,
      ( Weight_seed_nonce_revelation _ | Weight_activate_account _
      | Weight_drain_delegate _ ) ) ->
      1
  | Weight_seed_nonce_revelation l1, Weight_seed_nonce_revelation l2 ->
      Compare.Int32.compare l1 l2
  | ( (Weight_activate_account _ | Weight_drain_delegate _),
      Weight_seed_nonce_revelation _ ) ->
      -1
  | ( Weight_seed_nonce_revelation _,
      (Weight_activate_account _ | Weight_drain_delegate _) ) ->
      1
  | Weight_activate_account pkh1, Weight_activate_account pkh2 ->
      Ed25519.Public_key_hash.compare pkh1 pkh2
  | Weight_drain_delegate _, Weight_activate_account _ -> -1
  | Weight_activate_account _, Weight_drain_delegate _ -> 1
  | Weight_drain_delegate pkh1, Weight_drain_delegate pkh2 ->
      Signature.Public_key_hash.compare pkh1 pkh2

(** {5 Comparison of valid {!Manager_operation}} *)

(** Two {!Manager_operation} are compared in the lexicographic order
   of their pair of their [fee]/[gas] ratio -- as computed by
   {!weight_manager} -- and their [source]. *)
let compare_manager_weight weight1 weight2 =
  match (weight1, weight2) with
  | Weight_manager (manweight1, source1), Weight_manager (manweight2, source2)
    ->
      compare_pair_in_lexico_order
        (manweight1, source1)
        (manweight2, source2)
        ~cmp_fst:Compare.Q.compare
        ~cmp_snd:Signature.Public_key_hash.compare

(** Two {!operation_weight} are compared by their [pass], see
   {!compare_inner_pass} for more details. When they have the same
   [pass], they are compared by their [weight]. *)
let compare_operation_weight w1 w2 =
  match (w1, w2) with
  | W (Consensus, w1), W (Consensus, w2) -> compare_consensus_weight w1 w2
  | W (Voting, w1), W (Voting, w2) -> compare_vote_weight w1 w2
  | W (Anonymous, w1), W (Anonymous, w2) -> compare_anonymous_weight w1 w2
  | W (Manager, w1), W (Manager, w2) -> compare_manager_weight w1 w2
  | W (pass1, _), W (pass2, _) -> compare_inner_pass pass1 pass2

(** {3 Compare two valid operations} *)

(** Two valid operations are compared as their {!operation_weight},
    see {!compare_operation_weight} for more details.

    When they are equal according to their {!operation_weight} comparison, they
   compare as their hash.
   Hence, [compare] returns [0] only when the hashes of both operations are
   equal.

   Preconditions: [oph1] is the hash of [op1]; [oph2] the one of [op2]; and
   [op1] and [op2] are both valid. *)
let compare (oph1, op1) (oph2, op2) =
  let cmp_h = Operation_hash.(compare oph1 oph2) in
  if Compare.Int.(cmp_h = 0) then 0
  else
    let cmp = compare_operation_weight (weight_of op1) (weight_of op2) in
    if Compare.Int.(cmp = 0) then cmp_h else cmp

module Internal_for_benchmarking = struct
  let serialize_unsigned_operation = serialize_unsigned_operation
end
