(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let pack_operation ctxt signature contents =
  let branch = Context.branch ctxt in
  Operation.pack
    ({shell = {branch}; protocol_data = {contents; signature}} : _ Operation.t)

let sign ?(watermark = Signature.Generic_operation) sk branch contents =
  let unsigned =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding_with_legacy_attestation_name
      ({branch}, Contents_list contents)
  in
  let signature = Some (Signature.sign ~watermark sk unsigned) in
  ({shell = {branch}; protocol_data = {contents; signature}} : _ Operation.t)

(** Generates the block payload hash based on the hash [pred_hash] of
    the predecessor block and the hash of non-consensus operations of
    the current block [b]. *)
let mk_block_payload_hash payload_round (b : Block.t) =
  let ops = Block.Forge.classify_operations b.operations in
  let non_consensus_operations =
    List.concat (match List.tl ops with None -> [] | Some l -> l)
  in
  let hashes = List.map Operation.hash_packed non_consensus_operations in
  Block_payload.hash
    ~predecessor_hash:b.header.shell.predecessor
    ~payload_round
    hashes

let mk_consensus_content_signer_and_branch ?delegate ?slot ?level ?round
    ?block_payload_hash ?branch attested_block =
  let open Lwt_result_wrap_syntax in
  let branch =
    match branch with
    | None -> attested_block.Block.header.shell.predecessor
    | Some branch -> branch
  in
  let* delegate_pkh, slots =
    match delegate with
    | None -> Context.get_attester (B attested_block)
    | Some del -> (
        let* slots = Context.get_attester_slot (B attested_block) del in
        match slots with
        | None -> return (del, [])
        | Some slots -> return (del, slots))
  in
  let slot =
    match slot with None -> Stdlib.List.hd slots | Some slot -> slot
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
    | None -> mk_block_payload_hash round attested_block
    | Some block_payload_hash -> block_payload_hash
  in
  let consensus_content = {slot; level; round; block_payload_hash} in
  let* signer = Account.find delegate_pkh in
  return (consensus_content, signer.sk, branch)

let raw_attestation ?delegate ?slot ?level ?round ?block_payload_hash ?branch
    attested_block =
  let open Lwt_result_syntax in
  let* consensus_content, signer, branch =
    mk_consensus_content_signer_and_branch
      ?delegate
      ?slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  let op = Single (Attestation consensus_content) in
  return
    (sign
       ~watermark:Operation.(to_watermark (Attestation Chain_id.zero))
       signer
       branch
       op)

let attestation ?delegate ?slot ?level ?round ?block_payload_hash ?branch
    attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_attestation
      ?delegate
      ?slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  return (Operation.pack op)

let raw_preattestation ?delegate ?slot ?level ?round ?block_payload_hash ?branch
    attested_block =
  let open Lwt_result_syntax in
  let* consensus_content, signer, branch =
    mk_consensus_content_signer_and_branch
      ?delegate
      ?slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  let op = Single (Preattestation consensus_content) in
  return
    (sign
       ~watermark:Operation.(to_watermark (Preattestation Chain_id.zero))
       signer
       branch
       op)

let preattestation ?delegate ?slot ?level ?round ?block_payload_hash ?branch
    attested_block =
  let open Lwt_result_syntax in
  let* op =
    raw_preattestation
      ?delegate
      ?slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      attested_block
  in
  return (Operation.pack op)

let sign ?watermark sk ctxt (Contents_list contents) =
  Operation.pack (sign ?watermark sk ctxt contents)

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
      let* counter = Context.Contract.counter ctxt source in
      (* Update counters and transform into a contents_list *)
      let _, rev_operations =
        List.fold_left
          (fun (counter, acc) -> function
            | Contents (Manager_operation m) ->
                ( Manager_counter.succ counter,
                  Contents (Manager_operation {m with counter}) :: acc )
            | x -> (counter, x :: acc))
          (Manager_counter.succ counter, [])
          operations
      in
      return (List.rev rev_operations)
    else return operations
  in
  let* account = Context.Contract.manager ctxt source in
  let*?@ operations = Operation.of_list operations in
  return @@ sign account.sk (Context.branch ctxt) operations

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
              operation = Reveal public_key;
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
  return @@ sign account.sk (Context.branch ctxt) operations

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
          operation = Reveal public_key;
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
    ?(storage_limit = Z.zero) ?counter ?(forge_pkh = None) ctxt public_key =
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
  let+ counter =
    match counter with
    | None -> Context.Contract.counter ctxt source
    | Some ctr -> return ctr
  in
  let counter = Manager_counter.succ counter in
  Manager_operation
    {
      source = pkh;
      fee;
      counter;
      operation = Reveal public_key;
      gas_limit;
      storage_limit;
    }

let revelation ?fee ?(gas_limit = High) ?storage_limit ?counter ?forge_pkh ctxt
    public_key =
  let open Lwt_result_syntax in
  let* (Manager_operation {source; _} as dummy_operation) =
    revelation_with_fixed_gas_limit
      ?fee
      ~gas_limit:default_high_gas_limit
      ?storage_limit
      ?counter
      ?forge_pkh
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
      ctxt
      public_key
  in
  let sop = Contents_list (Single op) in
  let+ account = Context.Contract.manager ctxt (Implicit source) in
  sign account.sk (Context.branch ctxt) sop

let failing_noop ctxt source arbitrary =
  let open Lwt_result_syntax in
  let op = Contents_list (Single (Failing_noop arbitrary)) in
  let* account = Account.find source in
  return @@ sign account.sk (Context.branch ctxt) op

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
  let+ sop =
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
  k (sign account.sk (Context.branch ctxt) sop)

let contract_origination =
  contract_origination_gen (fun op -> (op, originated_contract op))

let contract_origination_hash =
  contract_origination_gen (fun op -> (op, originated_contract_hash op))

let register_global_constant ?force_reveal ?counter ?public_key ?fee ?gas_limit
    ?storage_limit ctxt ~source ~value =
  let open Lwt_result_syntax in
  let* account = Context.Contract.manager ctxt source in
  let operation = Register_global_constant {value} in
  let+ sop =
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
  sign account.sk (Context.branch ctxt) sop

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) sop

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
  let+ account = Context.Contract.manager ctxt source in
  sign account.sk (Context.branch ctxt) sop

let set_deposits_limit ?force_reveal ?fee ?gas_limit ?storage_limit ?counter
    ctxt source limit =
  let top = Set_deposits_limit limit in
  manager_operation
    ?force_reveal
    ?fee
    ?counter
    ?storage_limit
    ?gas_limit
    ~source
    ctxt
    top
  >>=? fun sop ->
  Context.Contract.manager ctxt source >|=? fun account ->
  sign account.sk (Context.branch ctxt) sop

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
  let+ account = Context.Contract.manager ctxt source in
  sign account.sk (Context.branch ctxt) sop

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

let double_attestation ctxt op1 op2 =
  let contents = Single (Double_attestation_evidence {op1; op2}) in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let double_preattestation ctxt op1 op2 =
  let contents = Single (Double_preattestation_evidence {op1; op2}) in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let double_baking ctxt bh1 bh2 =
  let contents = Single (Double_baking_evidence {bh1; bh2}) in
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
  return (sign account.sk (Context.branch ctxt) (Contents_list contents))

let ballot_contents ctxt voter ?period proposal ballot =
  let open Lwt_result_syntax in
  let source = Context.Contract.pkh voter in
  let* period = get_period ?period ctxt in
  return (Single (Ballot {source; period; proposal; ballot}))

let ballot ctxt voter ?period proposal ballot =
  let open Lwt_result_syntax in
  let* contents = ballot_contents ctxt voter ?period proposal ballot in
  let* account = Account.find (Context.Contract.pkh voter) in
  return (sign account.sk (Context.branch ctxt) (Contents_list contents))

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

let dummy_script_cost = Test_tez.of_mutez_exn 9_500L

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
  let+ account = Context.Contract.manager ctxt source in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let op = sign account.sk (Context.branch ctxt) to_sign_op in
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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt source in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

let dal_publish_slot_header ?force_reveal ?counter ?fee ?gas_limit
    ?storage_limit ctxt (src : Contract.t) slot_header =
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
      (Dal_publish_slot_header slot_header)
  in
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  let op = sign account.sk (Context.branch ctxt) to_sign_op in
  originated_zk_rollup op |> fun addr -> (op, addr)

let update_consensus_key ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ctxt (src : Contract.t) pkh =
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
      (Update_consensus_key pkh)
  in
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

let drain_delegate ctxt ~consensus_key ~delegate ~destination =
  let open Lwt_result_syntax in
  let contents =
    Single (Drain_delegate {consensus_key; delegate; destination})
  in
  let+ account =
    Context.Contract.manager ctxt (Contract.Implicit consensus_key)
  in
  sign account.sk (Context.branch ctxt) (Contents_list contents)

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op

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
  let+ account = Context.Contract.manager ctxt src in
  sign account.sk (Context.branch ctxt) to_sign_op
