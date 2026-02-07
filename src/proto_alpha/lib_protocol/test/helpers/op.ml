(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

open Protocol
open Alpha_context

type t = packed_operation

let pack_operation ctxt signature contents =
  let branch = Context.branch ctxt in
  Operation.pack
    ({shell = {branch}; protocol_data = {contents; signature}} : _ Operation.t)

let raw_sign (type kind) ctxt ?(watermark = Signature.Generic_operation)
    ?(companion_key : Delegate_services.companion_key option)
    (sk : Signature.secret_key) branch (contents : kind contents_list) =
  let open Lwt_result_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let encoding =
    if Constants.aggregate_attestation alpha_ctxt then
      (* Under the [aggregate_attestation] feature flag, operations signed with
         BLS keys use a dedicated serialization encoding, which differs only for
         attestations and preattestations. *)
      match sk with
      | Bls _ -> Operation.bls_mode_unsigned_encoding
      | _ -> Operation.unsigned_encoding
    else Operation.unsigned_encoding
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn encoding ({branch}, Contents_list contents)
  in
  match (contents, sk, companion_key) with
  | ( Single (Attestation {dal_content = Some {attestations = dal_content}; _}),
      (Bls _ : Signature.secret_key),
      Some companion_key ) -> (
      let* companion_signer =
        Account.find (Bls companion_key.companion_key_pkh)
      in
      let consensus_sig = Signature.sign ~watermark sk bytes in
      let companion_sig = Signature.sign ~watermark companion_signer.sk bytes in
      match
        ( Signature.Secret_key.to_public_key sk,
          companion_signer.pk,
          consensus_sig,
          companion_sig )
      with
      | Bls consensus_pk, Bls companion_pk, Bls consensus_sig, Bls companion_sig
        -> (
          let dal_dependent_bls_sig_opt =
            Alpha_context.Dal.Attestations.Dal_dependent_signing.aggregate_sig
              ~subgroup_check:false
              ~consensus_pk
              ~companion_pk
              ~consensus_sig
              ~companion_sig
              ~op:bytes
              dal_content
          in
          match dal_dependent_bls_sig_opt with
          | Some sign -> return (Bls sign : Signature.signature)
          | _ -> assert false)
      | _ -> assert false)
  | _ -> return (Signature.sign ~watermark sk bytes)

let sign ctxt ?(companion_key : Delegate_services.companion_key option)
    ?(watermark = Signature.Generic_operation) sk branch contents =
  let open Lwt_result_syntax in
  let* signature = raw_sign ctxt ~watermark ?companion_key sk branch contents in
  return
    ({shell = {branch}; protocol_data = {contents; signature = Some signature}}
      : _ Operation.t)

let create_proof sk =
  match (sk : Signature.secret_key) with
  | Bls sk -> Signature.Bls.of_bytes_opt (Signature.Bls.pop_prove sk)
  | _ -> None

(** Generates the block payload hash based on the hash [pred_hash] of
    the predecessor block and the hash of non-consensus operations of
    the current block [b]. *)
let mk_block_payload_hash (b : Block.t) =
  let ops = Block.Forge.classify_operations b.operations in
  let non_consensus_operations =
    List.concat (match List.tl ops with None -> [] | Some l -> l)
  in
  let payload_round = Block.get_payload_round b in
  let hashes = List.map Operation.hash_packed non_consensus_operations in
  Block_payload.hash
    ~predecessor_hash:b.header.shell.predecessor
    ~payload_round
    hashes

type attesting_slot = {slot : Slot.t; consensus_pkh : public_key_hash}

let attesting_slot_of_attester
    {Plugin.RPC.Validators.consensus_key; attestation_slot; _} =
  {slot = attestation_slot; consensus_pkh = consensus_key}

let get_attesting_slot ~attested_block =
  let open Lwt_result_syntax in
  let* attester = Context.get_attester (B attested_block) in
  return (attesting_slot_of_attester attester)

let get_attesting_slot_of_delegate ~manager_pkh ~attested_block =
  let open Lwt_result_syntax in
  let* attester = Context.get_attester ~manager_pkh (B attested_block) in
  return (attesting_slot_of_attester attester)

let get_delegate_of_attesting_slot ~attesting_slot ~attested_block =
  let open Lwt_result_syntax in
  let* attesters = Context.get_attesters (B attested_block) in
  let attester =
    List.find
      (fun (att : Context.attester) ->
        Signature.Public_key_hash.equal
          att.consensus_key
          attesting_slot.consensus_pkh)
      attesters
  in
  match attester with
  | None ->
      (* Should not happen, unless the attesting slot is malformed *)
      return attesting_slot.consensus_pkh
  | Some attester -> return attester.delegate

let get_different_attesting_slot ~consensus_pkh_to_avoid ~attested_block =
  let open Lwt_result_syntax in
  let* attesters = Context.get_attesters (B attested_block) in
  let attester =
    List.find_opt
      (fun {Plugin.RPC.Validators.consensus_key; _} ->
        not
          (Signature.Public_key_hash.equal consensus_key consensus_pkh_to_avoid))
      attesters
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (attesting_slot_of_attester attester)

let non_canonical_attesting_slot_of_attester {Context.consensus_key; rounds; _}
    =
  let open Lwt_result_wrap_syntax in
  let* slot =
    match rounds with
    | _ :: non_canonical_slot :: _ ->
        let*?@ round_int = Round.to_int non_canonical_slot in
        let*?@ slot = Slot.of_int round_int in
        return slot
    | _ -> Test.fail ~__LOC__ "Expected attester to have at least two slots"
  in
  return {slot; consensus_pkh = consensus_key}

let get_non_canonical_attesting_slot ~attested_block =
  let open Lwt_result_syntax in
  let* attester = Context.get_attester (B attested_block) in
  non_canonical_attesting_slot_of_attester attester

let default_committee ~attested_block =
  let open Lwt_result_syntax in
  let* attesters_with_bls_key =
    Context.get_attesters_with_bls_key (B attested_block)
  in
  return (List.map attesting_slot_of_attester attesters_with_bls_key)

let get_attesting_slot_with_bls_key ~attested_block =
  let open Lwt_result_syntax in
  let* attester = Context.get_attester_with_bls_key (B attested_block) in
  return (attesting_slot_of_attester attester)

let get_attesting_slot_with_non_bls_key ~attested_block =
  let open Lwt_result_syntax in
  let* attesters = Context.get_attesters (B attested_block) in
  let attester =
    List.find_opt
      (fun attester -> not (Context.attester_has_bls_key attester))
      attesters
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (attesting_slot_of_attester attester)

let attesting_slot_of_delegate_rights
    {RPC.Attestation_rights.consensus_key; first_slot; _} =
  {slot = first_slot; consensus_pkh = consensus_key}

let mk_consensus_content_signer_and_branch ?attesting_slot ?manager_pkh ?level
    ?round ?block_payload_hash ?branch attested_block =
  let open Lwt_result_wrap_syntax in
  let branch =
    match branch with
    | None -> attested_block.Block.header.shell.predecessor
    | Some branch -> branch
  in
  let* ({slot; consensus_pkh} as attesting_slot) =
    match (attesting_slot, manager_pkh) with
    | Some attesting_slot, None -> return attesting_slot
    | None, None -> get_attesting_slot ~attested_block
    | None, Some manager_pkh ->
        get_attesting_slot_of_delegate ~manager_pkh ~attested_block
    | Some _, Some _ ->
        Test.fail
          ~__LOC__
          "Cannot provide both ~attesting_slot and ~manager_pkh"
  in
  let* manager_pkh =
    match manager_pkh with
    | None -> get_delegate_of_attesting_slot ~attesting_slot ~attested_block
    | Some manager_pkh -> return manager_pkh
  in
  let* level =
    match level with
    | None ->
        let*? level = Context.get_level (B attested_block) in
        return level
    | Some level -> return level
  in
  let* round =
    match round with
    | None ->
        let*?@ round = Block.get_round attested_block in
        return round
    | Some round -> return round
  in
  let block_payload_hash =
    match block_payload_hash with
    | None -> mk_block_payload_hash attested_block
    | Some block_payload_hash -> block_payload_hash
  in
  let consensus_content = {slot; level; round; block_payload_hash} in
  let* signer = Account.find consensus_pkh in
  let* companion =
    Context.Delegate.companion_key (B attested_block) manager_pkh
  in
  return (consensus_content, signer.sk, companion.active_companion_key, branch)

let raw_attestation ?attesting_slot ?manager_pkh ?level ?round
    ?block_payload_hash ?dal_content ?branch attested_block =
  let open Lwt_result_syntax in
  let* consensus_content, signer, companion_key, branch =
    mk_consensus_content_signer_and_branch
      ?attesting_slot
      ?manager_pkh
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  (* Note: if the Dal content is not None, the signer is a tz4, and the
     active companion key is None, then this operation will fail, because
     the protocol cannot check the signature. It is intended here. *)
  let contents = Single (Attestation {consensus_content; dal_content}) in
  sign
    ~watermark:Operation.(to_watermark (Attestation Chain_id.zero))
    ?companion_key
    Context.(B attested_block)
    signer
    branch
    contents

let raw_aggregate attestations =
  let aggregate_content =
    List.fold_left
      (fun acc
           ({shell; protocol_data = {contents; signature}} : _ Operation.t)
         ->
        match (contents, signature) with
        | ( Single (Attestation {consensus_content; dal_content}),
            Some (Bls bls_sig) ) -> (
            let ({slot; _} : consensus_content) = consensus_content in
            match acc with
            | Some (shell, proposal, slots, signatures) ->
                Some
                  ( shell,
                    proposal,
                    (slot, dal_content) :: slots,
                    bls_sig :: signatures )
            | None ->
                let {level; round; block_payload_hash; _} = consensus_content in
                let proposal = {level; round; block_payload_hash} in
                Some (shell, proposal, [(slot, dal_content)], [bls_sig]))
        | _, _ -> acc)
      None
      attestations
  in
  let open Option_syntax in
  let* shell, consensus_content, committee, signatures = aggregate_content in
  let+ signature =
    Bls12_381_signature.MinPk.aggregate_signature_opt signatures
  in
  let contents =
    Single
      (Attestations_aggregate
         (* Reverse committee to preserve [attestations] order *)
         {consensus_content; committee = List.rev committee})
  in
  let protocol_data = {contents; signature = Some (Bls signature)} in
  ({shell; protocol_data} : Kind.attestations_aggregate operation)

let raw_aggregate_preattestations preattestations =
  let aggregate_content =
    List.fold_left
      (fun acc
           ({shell; protocol_data = {contents; signature}} : _ Operation.t)
         ->
        match (contents, signature) with
        | Single (Preattestation consensus_content), Some (Bls bls_sig) -> (
            let ({slot; _} : consensus_content) = consensus_content in
            match acc with
            | Some (shell, proposal, slots, signatures) ->
                Some (shell, proposal, slot :: slots, bls_sig :: signatures)
            | None ->
                let {level; round; block_payload_hash; _} = consensus_content in
                let proposal = {level; round; block_payload_hash} in
                Some (shell, proposal, [slot], [bls_sig]))
        | _, _ -> acc)
      None
      preattestations
  in
  let open Option_syntax in
  let* shell, consensus_content, committee, signatures = aggregate_content in
  let+ signature =
    Bls12_381_signature.MinPk.aggregate_signature_opt signatures
  in
  let contents =
    Single
      (Preattestations_aggregate
         (* Reverse committee to preserve [preattestations] order *)
         {consensus_content; committee = List.rev committee})
  in
  let protocol_data = {contents; signature = Some (Bls signature)} in
  ({shell; protocol_data} : Kind.preattestations_aggregate operation)

let attestation ?attesting_slot ?manager_pkh ?level ?round ?block_payload_hash
    ?dal_content ?branch attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_attestation
      ?attesting_slot
      ?manager_pkh
      ?level
      ?round
      ?block_payload_hash
      ?dal_content
      ?branch
      attested_block
  in
  return (Operation.pack op)

let raw_attestations_aggregate ?committee ?committee_with_dal ?level ?round
    ?block_payload_hash ?branch attested_block =
  let open Lwt_result_syntax in
  let* committee =
    match (committee, committee_with_dal) with
    | None, Some committee_with_dal -> return committee_with_dal
    | Some committee, None -> return @@ List.map (fun x -> (x, None)) committee
    | Some committee, Some committee_with_dal ->
        return @@ committee_with_dal @ List.map (fun x -> (x, None)) committee
    | None, None ->
        let* attesting_slots = default_committee ~attested_block in
        return
          (List.map
             (fun attesting_slot -> (attesting_slot, None))
             attesting_slots)
  in
  let* attestations =
    List.map_es
      (fun (attesting_slot, dal_content) ->
        raw_attestation
          ~attesting_slot
          ?dal_content
          ?level
          ?round
          ?block_payload_hash
          ?branch
          attested_block)
      committee
  in
  match raw_aggregate attestations with
  | Some aggregate_attestation -> return aggregate_attestation
  | None -> failwith "no Bls delegate found"

let attestations_aggregate ?committee ?committee_with_dal ?level ?round
    ?block_payload_hash ?branch attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_attestations_aggregate
      ?committee
      ?committee_with_dal
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  return (Operation.pack op)

let raw_preattestation ?attesting_slot ?manager_pkh ?level ?round
    ?block_payload_hash ?branch attested_block =
  let open Lwt_result_syntax in
  let* consensus_content, signer, _companion_key_opt, branch =
    mk_consensus_content_signer_and_branch
      ?attesting_slot
      ?manager_pkh
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  let contents = Single (Preattestation consensus_content) in
  sign
    ~watermark:Operation.(to_watermark (Preattestation Chain_id.zero))
    Context.(B attested_block)
    signer
    branch
    contents

let preattestation ?attesting_slot ?manager_pkh ?level ?round
    ?block_payload_hash ?branch attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_preattestation
      ?attesting_slot
      ?manager_pkh
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  return (Operation.pack op)

let raw_preattestations_aggregate ?committee ?level ?round ?block_payload_hash
    ?branch attested_block =
  let open Lwt_result_syntax in
  let* committee =
    match committee with
    | Some committee -> return committee
    | None -> default_committee ~attested_block
  in
  let* preattestations =
    List.map_es
      (fun attesting_slot ->
        raw_preattestation
          ~attesting_slot
          ?level
          ?round
          ?block_payload_hash
          ?branch
          attested_block)
      committee
  in
  match raw_aggregate_preattestations preattestations with
  | Some preattestations_aggregate -> return preattestations_aggregate
  | None -> failwith "no Bls delegate found"

let preattestations_aggregate ?committee ?level ?round ?block_payload_hash
    ?branch attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_preattestations_aggregate
      ?committee
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  return (Operation.pack op)

let sign ?watermark ctxt sk branch (Contents_list contents) =
  let open Lwt_result_syntax in
  let* op = sign ctxt ?watermark sk branch contents in
  return (Operation.pack op)

let batch_operations ?(recompute_counters = false) ~source ctxt
    (operations : packed_operation list) =
  let open Lwt_result_wrap_syntax in
  let operations =
    List.map
      (function
        | {Alpha_context.protocol_data = Operation_data {contents; _}; _} ->
            Operation.to_list (Contents_list contents))
      operations
    |> List.flatten
  in
  let* operations =
    if recompute_counters then
      let module CounterMap = Signature.Public_key_hash.Map in
      let* source_counter = Context.Contract.counter ctxt source in
      let pkh = Context.Contract.pkh in
      let counter_map =
        CounterMap.singleton (pkh source) (Manager_counter.succ source_counter)
      in
      let return_and_incr_counter map pkh =
        let* next_counter =
          CounterMap.find pkh map |> function
          | None ->
              let* c = Context.Contract.counter ctxt (Implicit pkh) in
              return (Manager_counter.succ c)
          | Some c -> return c
        in
        return
          ( next_counter,
            CounterMap.add pkh (Manager_counter.succ next_counter) map )
      in
      (* Update counters and transform into a contents_list *)
      let* _counter_map, rev_operations =
        List.fold_left_es
          (fun (counter_map, acc) -> function
            | Contents (Manager_operation m) ->
                let* counter, counter_map =
                  return_and_incr_counter counter_map m.source
                in
                return
                  ( counter_map,
                    Contents (Manager_operation {m with counter}) :: acc )
            | x -> return (counter_map, x :: acc))
          (counter_map, [])
          operations
      in
      return (List.rev rev_operations)
    else return operations
  in
  let* account = Context.Contract.manager ctxt source in
  let*?@ operations = Operation.of_list operations in
  sign ctxt account.sk (Context.branch ctxt) operations

type gas_limit = Max | High | Low | Zero | Custom_gas of Gas.Arith.integral

let default_low_gas_limit op pkh =
  let {shell; protocol_data = Operation_data protocol_data} = op in
  let op : _ operation = {shell; protocol_data} in
  let check_sig_gas =
    Operation_costs.check_signature_cost
      (Michelson_v1_gas.Cost_of.Interpreter.algo_of_public_key_hash pkh)
      op
  in
  let total_cost =
    Gas.(Michelson_v1_gas.Cost_of.manager_operation +@ check_sig_gas)
  in
  (* Some tests need milligas precision to distinguish failures in
     validation from failures in application but limits in the
     protocol are statically guaranteed to be integral values so we
     use Obj.magic to bypass them. *)
  (Obj.magic total_cost : Gas.Arith.integral)

let default_high_gas_limit =
  Gas.Arith.integral_of_int_exn
    (49_000 + Michelson_v1_gas.Internal_for_tests.int_cost_of_manager_operation)

let resolve_gas_limit ?(force_reveal = false) ctxt op source gas_limit =
  let open Lwt_result_syntax in
  let pkh = Context.Contract.pkh source in
  let* revealed = Context.Contract.is_manager_key_revealed ctxt source in
  match gas_limit with
  | Max ->
      let* c = Context.get_constants ctxt in
      return (c.parametric.hard_gas_limit_per_operation, None)
  | High -> return (default_high_gas_limit, None)
  | Low when force_reveal && not revealed ->
      (* If force reveal is set, the operation is a batch where the reveal is
         the first operation. This operation should have a gas_limit
         corresponding to the manager_operation constant (Low) + the cost of the
         signature checking. The second op should only have Low as gas_limit *)
      let op_gas : Gas.Arith.integral =
        Obj.magic Michelson_v1_gas.Cost_of.manager_operation
      in
      return (op_gas, Some (default_low_gas_limit op pkh))
  | Low -> return (default_low_gas_limit op pkh, None)
  | Zero -> return (Gas.Arith.zero, None)
  | Custom_gas x -> return (x, None)

let pp_gas_limit fmt = function
  | Max -> Format.fprintf fmt "Max"
  | High ->
      Format.fprintf fmt "High: %a" Gas.Arith.pp_integral default_high_gas_limit
  | Low -> Format.fprintf fmt "Low"
  | Zero -> Format.fprintf fmt "Zero: %a" Gas.Arith.pp_integral Gas.Arith.zero
  | Custom_gas x -> Format.fprintf fmt "Custom: %a" Gas.Arith.pp_integral x

let combine_operations ?public_key ?counter ?spurious_operation ~source ctxt
    (packed_operations : packed_operation list) =
  let open Lwt_result_wrap_syntax in
  assert (match packed_operations with [] -> false | _ :: _ -> true) ;
  (* Hypothesis : each operation must have the same branch (is this really true?) *)
  let {Tezos_base.Operation.branch} =
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd packed_operations).shell
  in
  assert (
    List.for_all
      (fun {shell = {Tezos_base.Operation.branch = b; _}; _} ->
        Block_hash.(branch = b))
      packed_operations) ;
  (* TODO? : check signatures consistency *)
  let unpacked_operations =
    List.map
      (function
        | {Alpha_context.protocol_data = Operation_data {contents; _}; _} -> (
            match Contents_list contents with
            | Contents_list (Single o) -> Contents o
            | Contents_list
                (Cons (Manager_operation {operation = Reveal _; _}, Single o))
              ->
                Contents o
            | _ -> (* TODO : decent error *) assert false))
      packed_operations
  in
  let* counter =
    match counter with
    | Some counter -> return counter
    | None -> Context.Contract.counter ctxt source
  in
  (* We increment the counter *)
  let counter = Manager_counter.succ counter in
  let* account = Context.Contract.manager ctxt source in
  let public_key = Option.value ~default:account.pk public_key in
  let* manager_op, counter =
    let+ is_revealed = Context.Contract.is_manager_key_revealed ctxt source in
    match is_revealed with
    | false ->
        let reveal_op =
          Manager_operation
            {
              source = Signature.Public_key.hash public_key;
              fee = Tez.zero;
              counter;
              operation = Reveal {public_key; proof = create_proof account.sk};
              gas_limit = default_high_gas_limit;
              storage_limit = Z.zero;
            }
        in
        (Some (Contents reveal_op), Manager_counter.succ counter)
    | true -> (None, counter)
  in
  (* Update counters and transform into a contents_list *)
  let counter, rev_operations =
    List.fold_left
      (fun (counter, acc) -> function
        | Contents (Manager_operation m) ->
            ( Manager_counter.succ counter,
              Contents (Manager_operation {m with counter}) :: acc )
        | x -> (counter, x :: acc))
      (counter, match manager_op with None -> [] | Some op -> [op])
      unpacked_operations
  in
  let operations = List.rev rev_operations in
  (* patch a random operation with a corrupted pkh *)
  let operations =
    match spurious_operation with
    | None -> operations
    | Some op ->
        let op =
          match op with
          | {protocol_data; shell = _} -> (
              match protocol_data with
              | Operation_data {contents; _} -> (
                  match contents with
                  | Cons _ -> assert false
                  | Single (Manager_operation m) ->
                      Alpha_context.Contents
                        (Manager_operation {m with counter})
                  | Single op -> Contents op))
        in
        (* Insert at the end *)
        operations @ [op]
  in
  let*?@ operations = Operation.of_list operations in
  sign ctxt account.sk (Context.branch ctxt) operations

let manager_operation_with_fixed_gas_limit ?(force_reveal = false) ?counter
    ~gas_limit ?(reveal_gas_limit = default_high_gas_limit) ?(fee = Tez.zero)
    ?storage_limit ?public_key ~source ctxt operation =
  let open Lwt_result_syntax in
  let* counter =
    match counter with
    | Some counter -> return counter
    | None -> Context.Contract.counter ctxt source
  in
  let* c = Context.get_constants ctxt in
  let storage_limit =
    Option.value
      ~default:c.parametric.hard_storage_limit_per_operation
      storage_limit
  in
  let* account = Context.Contract.manager ctxt source in
  let public_key = Option.value ~default:account.pk public_key in
  let counter = Manager_counter.succ counter in
  let+ revealed = Context.Contract.is_manager_key_revealed ctxt source in
  (* If the manager is revealed or we are not forcing reveals, we
     generate a singleton manager operation. *)
  if revealed || not force_reveal then
    let op =
      Manager_operation
        {
          source = Signature.Public_key.hash public_key;
          fee;
          counter;
          operation;
          gas_limit;
          storage_limit;
        }
    in
    Contents_list (Single op)
    (* Otherwise if the manager is unrevealed and we are
       force_revaling managers by default, we pre-attach a revelation
       for it. *)
  else
    let op_reveal =
      Manager_operation
        {
          source = Signature.Public_key.hash public_key;
          fee = Tez.zero;
          counter;
          operation = Reveal {public_key; proof = create_proof account.sk};
          gas_limit = reveal_gas_limit;
          storage_limit = Z.zero;
        }
    in
    let op =
      Manager_operation
        {
          source = Signature.Public_key.hash public_key;
          fee;
          counter = Manager_counter.succ counter;
          operation;
          gas_limit;
          storage_limit;
        }
    in
    Contents_list (Cons (op_reveal, Single op))

let manager_operation ?force_reveal ?counter ?fee ?(gas_limit = High)
    ?storage_limit ?public_key ~source ctxt operation =
  let open Lwt_result_syntax in
  let default_gas_limit = default_high_gas_limit in
  let* (Contents_list dummy_operation) =
    manager_operation_with_fixed_gas_limit
      ?force_reveal
      ?counter
      ~gas_limit:default_gas_limit
      ?fee
      ?storage_limit
      ?public_key
      ~source
      ctxt
      operation
  in
  let* gas_limit, reveal_gas_limit =
    resolve_gas_limit
      ?force_reveal
      ctxt
      {
        shell = {branch = Context.branch ctxt};
        protocol_data =
          Operation_data {contents = dummy_operation; signature = None};
      }
      source
      gas_limit
  in
  manager_operation_with_fixed_gas_limit
    ?force_reveal
    ?counter
    ~gas_limit
    ?reveal_gas_limit
    ?fee
    ?storage_limit
    ?public_key
    ~source
    ctxt
    operation

let revelation_with_fixed_gas_limit ?(fee = Tez.zero) ~gas_limit
    ?(storage_limit = Z.zero) ?counter ?(forge_pkh = None) ?forge_proof ctxt
    public_key =
  let open Lwt_result_syntax in
  (* If Some pkh is provided to ?forge_pkh we take that hash at face
     value, otherwise we honestly compute the hash from
     [public_key]. This is useful to test forging Reveal operations
     (cf. tezos!5182). *)
  let pkh =
    match forge_pkh with
    | Some pkh -> pkh
    | None -> Signature.Public_key.hash public_key
  in
  let source = Contract.Implicit pkh in
  let* counter =
    match counter with
    | None -> Context.Contract.counter ctxt source
    | Some ctr -> return ctr
  in
  let counter = Manager_counter.succ counter in
  let+ account = Context.Contract.manager ctxt source in
  let proof =
    match forge_proof with
    | Some proof -> proof
    | None -> create_proof account.sk
  in
  Manager_operation
    {
      source = pkh;
      fee;
      counter;
      operation = Reveal {public_key; proof};
      gas_limit;
      storage_limit;
    }

let revelation ?fee ?(gas_limit = High) ?storage_limit ?counter ?forge_pkh
    ?forge_proof ctxt public_key =
  let open Lwt_result_syntax in
  let* (Manager_operation {source; _} as dummy_operation) =
    revelation_with_fixed_gas_limit
      ?fee
      ~gas_limit:default_high_gas_limit
      ?storage_limit
      ?counter
      ?forge_pkh
      ?forge_proof
      ctxt
      public_key
  in
  let* gas_limit, _ =
    resolve_gas_limit
      ctxt
      {
        shell = {branch = Context.branch ctxt};
        protocol_data =
          Operation_data {contents = Single dummy_operation; signature = None};
      }
      (Contract.Implicit source)
      gas_limit
  in
  let* op =
    revelation_with_fixed_gas_limit
      ?fee
      ~gas_limit
      ?storage_limit
      ?counter
      ?forge_pkh
      ?forge_proof
      ctxt
      public_key
  in
  let sop = Contents_list (Single op) in
  let* account = Context.Contract.manager ctxt (Implicit source) in
  sign ctxt account.sk (Context.branch ctxt) sop

let failing_noop ctxt source arbitrary =
  let open Lwt_result_syntax in
  let op = Contents_list (Single (Failing_noop arbitrary)) in
  let* account = Account.find source in
  sign ctxt account.sk (Context.branch ctxt) op

let originated_contract_hash op =
  let nonce = Protocol.Origination_nonce.initial (Operation.hash_packed op) in
  Contract_hash.of_nonce nonce

let originated_contract op = Contract.Originated (originated_contract_hash op)

exception Impossible

let contract_origination_gen k ?force_reveal ?counter ?delegate ~script
    ?public_key ?credit ?fee ?gas_limit ?storage_limit ctxt source =
  let open Lwt_result_syntax in
  let* account = Context.Contract.manager ctxt source in
  let default_credit = Tez.of_mutez @@ Int64.of_int 1000001 in
  let default_credit =
    WithExceptions.Option.to_exn ~none:Impossible default_credit
  in
  let credit = Option.value ~default:default_credit credit in
  let operation = Origination {delegate; script; credit} in
  let* sop =
    manager_operation
      ?force_reveal
      ?counter
      ?public_key
      ?fee
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      operation
  in
  let* packed_operation = sign ctxt account.sk (Context.branch ctxt) sop in
  return (k packed_operation)

let contract_origination =
  contract_origination_gen (fun op -> (op, originated_contract op))

let contract_origination_hash =
  contract_origination_gen (fun op -> (op, originated_contract_hash op))

let register_global_constant ?force_reveal ?counter ?public_key ?fee ?gas_limit
    ?storage_limit ctxt ~source ~value =
  let open Lwt_result_syntax in
  let* account = Context.Contract.manager ctxt source in
  let operation = Register_global_constant {value} in
  let* sop =
    manager_operation
      ?force_reveal
      ?counter
      ?public_key
      ?fee
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      operation
  in
  sign ctxt account.sk (Context.branch ctxt) sop

let unsafe_transaction ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ?(parameters = Script.unit_parameter) ?(entrypoint = Entrypoint.default)
    ctxt (src : Contract.t) (destination : Contract.t) (amount : Tez.t) =
  let open Lwt_result_syntax in
  let top = Transaction {amount; parameters; destination; entrypoint} in
  let* sop =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      top
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) sop

let transaction ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ?parameters ?entrypoint ctxt (src : Contract.t) (dst : Contract.t)
    (amount : Tez.t) =
  unsafe_transaction
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    ?parameters
    ?entrypoint
    ctxt
    src
    dst
    amount

let delegation ?force_reveal ?fee ?gas_limit ?counter ?storage_limit ctxt source
    dst =
  let open Lwt_result_syntax in
  let top = Delegation dst in
  let* sop =
    manager_operation
      ?force_reveal
      ?fee
      ?counter
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      top
  in
  let* account = Context.Contract.manager ctxt source in
  sign ctxt account.sk (Context.branch ctxt) sop

let set_deposits_limit ?force_reveal ?fee ?gas_limit ?storage_limit ?counter
    ctxt source limit =
  let open Lwt_result_syntax in
  let top = Set_deposits_limit limit in
  let* sop =
    manager_operation
      ?force_reveal
      ?fee
      ?counter
      ?storage_limit
      ?gas_limit
      ~source
      ctxt
      top
  in
  let* account = Context.Contract.manager ctxt source in
  sign ctxt account.sk (Context.branch ctxt) sop

let increase_paid_storage ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ctxt ~source ~destination (amount : Z.t) =
  let open Lwt_result_syntax in
  let top = Increase_paid_storage {amount_in_bytes = amount; destination} in
  let* sop =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      top
  in
  let* account = Context.Contract.manager ctxt source in
  sign ctxt account.sk (Context.branch ctxt) sop

let activation ctxt (pkh : Signature.Public_key_hash.t) activation_code =
  let open Lwt_result_syntax in
  let+ id =
    match pkh with
    | Ed25519 edpkh -> return edpkh
    | _ ->
        failwith
          "Wrong public key hash : %a - Commitments must be activated with an \
           Signature.Ed25519 encrypted public key hash"
          Signature.Public_key_hash.pp
          pkh
  in
  let contents = Single (Activate_account {id; activation_code}) in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let double_consensus_operation (type a) ctxt (op1 : a Kind.consensus operation)
    op2 =
  let slot =
    match op1.protocol_data.contents with
    | Single
        (Preattestation consensus_content | Attestation {consensus_content; _})
      ->
        consensus_content.slot
    | Single (Preattestations_aggregate _ | Attestations_aggregate _) ->
        invalid_arg
          "Op.double_consensus_operation does not support aggregates yet"
  in
  let contents =
    Single (Double_consensus_operation_evidence {slot; op1; op2})
  in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let double_attestation = double_consensus_operation

let double_preattestation = double_consensus_operation

let double_baking ctxt bh1 bh2 =
  let contents = Single (Double_baking_evidence {bh1; bh2}) in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let dal_entrapment (type a) ctxt (attestation : a Kind.consensus operation)
    ~consensus_slot dal_slot_index (shard_with_proof : Dal.Shard_with_proof.t) =
  let contents =
    Single
      (Dal_entrapment_evidence
         {
           attestation;
           consensus_slot;
           slot_index = dal_slot_index;
           shard_with_proof;
         })
  in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let seed_nonce_revelation ctxt level nonce =
  {
    shell = {branch = Context.branch ctxt};
    protocol_data =
      Operation_data
        {
          contents = Single (Seed_nonce_revelation {level; nonce});
          signature = None;
        };
  }

let vdf_revelation ctxt solution =
  {
    shell = {branch = Context.branch ctxt};
    protocol_data =
      Operation_data
        {contents = Single (Vdf_revelation {solution}); signature = None};
  }

let get_period ?period ctxt =
  let open Lwt_result_syntax in
  match period with
  | Some period -> return period
  | None ->
      let* current_period = Context.Vote.get_current_period ctxt in
      return current_period.voting_period.index

let proposals_contents ctxt proposer ?period proposals =
  let open Lwt_result_syntax in
  let source = Context.Contract.pkh proposer in
  let* period = get_period ?period ctxt in
  return (Single (Proposals {source; period; proposals}))

let proposals ctxt proposer ?period proposals =
  let open Lwt_result_syntax in
  let* contents = proposals_contents ctxt proposer ?period proposals in
  let* account = Account.find (Context.Contract.pkh proposer) in
  sign ctxt account.sk (Context.branch ctxt) (Contents_list contents)

let ballot_contents ctxt voter ?period proposal ballot =
  let open Lwt_result_syntax in
  let source = Context.Contract.pkh voter in
  let* period = get_period ?period ctxt in
  return (Single (Ballot {source; period; proposal; ballot}))

let ballot ctxt voter ?period proposal ballot =
  let open Lwt_result_syntax in
  let* contents = ballot_contents ctxt voter ?period proposal ballot in
  let* account = Account.find (Context.Contract.pkh voter) in
  sign ctxt account.sk (Context.branch ctxt) (Contents_list contents)

let dummy_script =
  let open Micheline in
  Script.
    {
      code =
        lazy_expr
          (strip_locations
             (Seq
                ( (),
                  [
                    Prim ((), K_parameter, [Prim ((), T_unit, [], [])], []);
                    Prim ((), K_storage, [Prim ((), T_unit, [], [])], []);
                    Prim
                      ( (),
                        K_code,
                        [
                          Seq
                            ( (),
                              [
                                Prim ((), I_CDR, [], []);
                                Prim
                                  ( (),
                                    I_NIL,
                                    [Prim ((), T_operation, [], [])],
                                    [] );
                                Prim ((), I_PAIR, [], []);
                              ] );
                        ],
                        [] );
                  ] )));
      storage = lazy_expr (strip_locations (Prim ((), D_Unit, [], [])));
    }

let dummy_script_cost = Tez_helpers.of_mutez 9_500L

let transfer_ticket ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    ~(source : Contract.t) ~contents ~ty ~ticketer ~amount ~destination
    ~entrypoint =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      (Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint})
  in
  let* account = Context.Contract.manager ctxt source in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let originated_sc_rollup op =
  let packed = Operation.hash_packed op in
  let nonce = Origination_nonce.Internal_for_tests.initial packed in
  Sc_rollup.Internal_for_tests.originated_sc_rollup nonce

let sc_rollup_origination ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ?whitelist ctxt (src : Contract.t) kind ~boot_sector ~parameters_ty =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_originate {kind; boot_sector; parameters_ty; whitelist})
  in
  let* account = Context.Contract.manager ctxt src in
  let* op = sign ctxt account.sk (Context.branch ctxt) to_sign_op in
  let t = originated_sc_rollup op |> fun addr -> (op, addr) in
  return t

let sc_rollup_publish ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) rollup commitment =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_publish {rollup; commitment})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_cement ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) rollup =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_cement {rollup})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_execute_outbox_message ?counter ?fee ?gas_limit ?storage_limit
    ?force_reveal ctxt (src : Contract.t) rollup cemented_commitment
    ~output_proof =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_execute_outbox_message
         {rollup; cemented_commitment; output_proof})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_recover_bond ?counter ?fee ?gas_limit ?storage_limit ?force_reveal
    ctxt (source : Contract.t) (sc_rollup : Sc_rollup.t)
    (staker : public_key_hash) =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source
      ctxt
      (Sc_rollup_recover_bond {sc_rollup; staker})
  in
  let* account = Context.Contract.manager ctxt source in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_add_messages ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ctxt (src : Contract.t) messages =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_add_messages {messages})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_refute ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) rollup opponent refutation =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_refute {rollup; opponent; refutation})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let sc_rollup_timeout ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) rollup stakers =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Sc_rollup_timeout {rollup; stakers})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let dal_publish_commitment ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ctxt (src : Contract.t) slot_header =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Dal_publish_commitment slot_header)
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let originated_zk_rollup op =
  let packed = Operation.hash_packed op in
  let nonce = Origination_nonce.Internal_for_tests.initial packed in
  Zk_rollup.Internal_for_tests.originated_zk_rollup nonce

let zk_rollup_origination ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ctxt (src : Contract.t) ~public_parameters ~circuits_info ~init_state
    ~nb_ops =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Zk_rollup_origination
         {public_parameters; circuits_info; init_state; nb_ops})
  in
  let* account = Context.Contract.manager ctxt src in
  let* op = sign ctxt account.sk (Context.branch ctxt) to_sign_op in
  let addr = originated_zk_rollup op in
  return (op, addr)

let update_consensus_or_companion ~kind ?force_reveal ?counter ?fee ?gas_limit
    ?storage_limit ?proof_signer ?forge_proof ctxt (src : Contract.t) public_key
    =
  let open Lwt_result_syntax in
  let* proof =
    match proof_signer with
    | None -> return_none
    | Some account -> (
        let* account = Context.Contract.manager ctxt account in
        match account.sk with
        | Bls sk ->
            return_some
            @@ Signature.Bls.of_bytes_exn (Signature.Bls.pop_prove sk)
        | _ ->
            failwith
              "Can't forge an Update_consensus_key with a non-BLS proof of \
               possession.")
  in
  let proof = match forge_proof with Some proof -> proof | None -> proof in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Update_consensus_key {public_key; proof; kind})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let update_consensus_key = update_consensus_or_companion ~kind:Consensus

let update_companion_key = update_consensus_or_companion ~kind:Companion

let drain_delegate ctxt ~consensus_key ~delegate ~destination =
  let open Lwt_result_syntax in
  let contents =
    Single (Drain_delegate {consensus_key; delegate; destination})
  in
  let* account =
    Context.Contract.manager ctxt (Contract.Implicit consensus_key)
  in
  sign ctxt account.sk (Context.branch ctxt) (Contents_list contents)

let zk_rollup_publish ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) ~zk_rollup ~ops =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Zk_rollup_publish {zk_rollup; ops})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

let zk_rollup_update ?force_reveal ?counter ?fee ?gas_limit ?storage_limit ctxt
    (src : Contract.t) ~zk_rollup ~update =
  let open Lwt_result_syntax in
  let* to_sign_op =
    manager_operation
      ?force_reveal
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~source:src
      ctxt
      (Zk_rollup_update {zk_rollup; update})
  in
  let* account = Context.Contract.manager ctxt src in
  sign ctxt account.sk (Context.branch ctxt) to_sign_op

module Micheline = Micheline

type tested_mode = Application | Construction | Mempool

let show_mode = function
  | Application -> "Application"
  | Construction -> "Construction"
  | Mempool -> "Mempool"

let check_validation_and_application ~loc ?check_after ?error ~predecessor mode
    operation =
  let open Lwt_result_syntax in
  let check_res res =
    match (check_after, error) with
    | None, None ->
        (* assert success *)
        let*? (_ : Block.block_with_metadata) = res in
        return_unit
    | Some check_after, None ->
        let*? block_with_metadata = res in
        check_after block_with_metadata
    | None, Some error -> Assert.proto_error ~loc res error
    | Some _, Some _ ->
        Test.fail
          "Op.check_validation_and_application: cannot provide both \
           [check_after] and [error] (called from: %s)"
          loc
  in
  match mode with
  | Application ->
      let*! result =
        Block.bake_with_metadata ~baking_mode:Application ~operation predecessor
      in
      check_res result
  | Construction ->
      let*! result =
        Block.bake_with_metadata ~baking_mode:Baking ~operation predecessor
      in
      check_res result
  | Mempool ->
      let*! res =
        let* inc =
          Incremental.begin_construction ~mempool_mode:true predecessor
        in
        let* inc, op_receipt =
          Incremental.add_operation_with_metadata inc operation
        in
        (* Finalization doesn't do much in mempool mode, but some RPCs
           still call it, so we check that it doesn't fail unexpectedly. *)
        let* block, header_metadata =
          Incremental.finalize_block_with_metadata inc
        in
        return (block, (header_metadata, [op_receipt]))
      in
      check_res res

let check_validation_and_application_all_modes_different_outcomes ~loc
    ?check_after_application ?check_after_construction ?check_after_mempool
    ?application_error ?construction_error ?mempool_error ~predecessor operation
    =
  List.iter_es
    (fun (mode, check_after, error) ->
      check_validation_and_application
        ~loc:(Format.sprintf "%s (%s mode)" loc (show_mode mode))
        ?check_after
        ?error
        ~predecessor
        mode
        operation)
    [
      (Application, check_after_application, application_error);
      (Construction, check_after_construction, construction_error);
      (Mempool, check_after_mempool, mempool_error);
    ]

let check_validation_and_application_all_modes ~loc ?check_after ?error
    ~predecessor operation =
  check_validation_and_application_all_modes_different_outcomes
    ~loc
    ?check_after_application:check_after
    ?check_after_construction:check_after
    ?check_after_mempool:check_after
    ?application_error:error
    ?construction_error:error
    ?mempool_error:error
    ~predecessor
    operation

let get_op_signature op =
  let {shell = _; protocol_data = Operation_data {contents = _; signature}} =
    op
  in
  signature

let set_op_signature op new_signature =
  let {shell; protocol_data = Operation_data {contents; signature = _}} = op in
  {shell; protocol_data = Operation_data {contents; signature = new_signature}}

let copy_op_signature ~src ~dst =
  let signature = get_op_signature src in
  set_op_signature dst signature

let clst_deposit ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    (ctxt : Context.t) (src : Contract.t) (amount : Tez.t) =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ clst_hash = Contract.get_clst_contract_hash alpha_ctxt in
  unsafe_transaction
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    ~entrypoint:(Entrypoint.of_string_strict_exn "deposit")
    ctxt
    src
    (Contract.Originated clst_hash)
    amount

let clst_redeem ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    (ctxt : Context.t) (src : Contract.t) (amount : int64) =
  let open Lwt_result_wrap_syntax in
  let* alpha_ctxt = Context.get_alpha_ctxt ctxt in
  let*@ clst_hash = Contract.get_clst_contract_hash alpha_ctxt in
  let parameters =
    Alpha_context.Script.lazy_expr (Expr.from_string (Int64.to_string amount))
  in
  unsafe_transaction
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    ~entrypoint:(Entrypoint.of_string_strict_exn "redeem")
    ~parameters
    ctxt
    src
    (Contract.Originated clst_hash)
    Tez.zero
