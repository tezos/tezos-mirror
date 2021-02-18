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

let sign ?(watermark = Signature.Generic_operation) sk ctxt contents =
  let branch = Context.branch ctxt in
  let unsigned =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  let signature = Some (Signature.sign ~watermark sk unsigned) in
  ({shell = {branch}; protocol_data = {contents; signature}} : _ Operation.t)

let endorsement ?baker ?level ctxt ?(signing_context = ctxt) () =
  ( match baker with
  | None ->
      Context.get_endorser ctxt >|=? fun (baker, _slots) -> baker
  | Some baker ->
      return baker )
  >>=? fun baker ->
  Account.find_baker baker
  >>=? fun account ->
  Lwt.return
    ( ( match level with
      | None ->
          Context.get_level ctxt
      | Some level ->
          ok level )
    >|? fun level ->
    let op = Single (Endorsement {level}) in
    sign
      ~watermark:Signature.(Endorsement Chain_id.zero)
      account.key.sk
      signing_context
      op )

let endorsement_with_slot ?baker ?level ctxt ?(signing_context = ctxt) () =
  (match baker with None -> Context.get_endorser ctxt | Some v -> return v)
  >>=? fun (baker, slots) ->
  let slot = WithExceptions.Option.get ~loc:__LOC__ (List.hd slots) in
  Account.find_baker baker
  >>=? fun baker_account ->
  Lwt.return
    ( ( match level with
      | None ->
          Context.get_level ctxt
      | Some level ->
          ok level )
    >|? fun level ->
    let op = Single (Endorsement {level}) in
    let endorsement =
      sign
        ~watermark:Signature.(Endorsement Chain_id.zero)
        baker_account.key.sk
        signing_context
        op
    in
    ( {
        shell = endorsement.shell;
        protocol_data =
          {
            contents = Single (Endorsement_with_slot {endorsement; slot});
            signature = None;
          };
      }
      : Kind.endorsement_with_slot Operation.t ) )

let sign ?watermark sk ctxt (Contents_list contents) =
  Operation.pack (sign ?watermark sk ctxt contents)

let combine_operations ?public_key ?counter ?spurious_operation ~source ctxt
    (packed_operations : packed_operation list) =
  assert (List.length packed_operations > 0) ;
  (* Hypothesis : each operation must have the same branch (is this really true?) *)
  let {Tezos_base.Operation.branch} =
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd packed_operations).shell
  in
  assert (
    List.for_all
      (fun {shell = {Tezos_base.Operation.branch = b; _}; _} ->
        Block_hash.(branch = b))
      packed_operations ) ;
  (* TODO? : check signatures consistency *)
  let unpacked_operations =
    List.map
      (function
        | {Alpha_context.protocol_data = Operation_data {contents; _}; _} -> (
          match Contents_list contents with
          | Contents_list (Single o) ->
              Contents o
          | Contents_list
              (Cons (Manager_operation {operation = Reveal _; _}, Single o)) ->
              Contents o
          | _ ->
              (* TODO : decent error *) assert false ))
      packed_operations
  in
  ( match counter with
  | Some counter ->
      return counter
  | None ->
      Context.Contract.counter ctxt source )
  >>=? fun counter ->
  (* We increment the counter *)
  let counter = Z.succ counter in
  Context.Contract.find_account ctxt source
  >>=? fun account ->
  let public_key = Option.value ~default:account.pk public_key in
  Context.Contract.is_public_key_revealed ctxt source
  >|=? (function
         | false ->
             let reveal_op =
               Manager_operation
                 {
                   source = Signature.Public_key.hash public_key;
                   fee = Tez.zero;
                   counter;
                   operation = Reveal public_key;
                   gas_limit = Gas.Arith.integral_of_int_exn 10_000;
                   storage_limit = Z.zero;
                 }
             in
             (Some (Contents reveal_op), Z.succ counter)
         | true ->
             (None, counter))
  >|=? fun (manager_op, counter) ->
  (* Update counters and transform into a contents_list *)
  let operations =
    List.fold_left
      (fun (counter, acc) -> function Contents (Manager_operation m) ->
            ( Z.succ counter,
              Contents (Manager_operation {m with counter}) :: acc ) | x ->
            (counter, x :: acc))
      (counter, match manager_op with None -> [] | Some op -> [op])
      unpacked_operations
    |> snd |> List.rev
  in
  (* patch a random operation with a corrupted pkh *)
  let operations =
    match spurious_operation with
    | None ->
        operations
    | Some op -> (
        let op =
          match op with
          | {protocol_data; shell = _} -> (
            match protocol_data with
            | Operation_data {contents; _} -> (
              match contents with
              | Cons _ ->
                  assert false
              | Single op ->
                  Alpha_context.Contents op ) )
        in
        (* Select where to insert spurious op *)
        let legit_ops = List.length operations in
        let index = Random.int legit_ops in
        match List.split_n index operations with
        | (preserved_prefix, preserved_suffix) ->
            preserved_prefix @ (op :: preserved_suffix) )
  in
  let operations = Operation.of_list operations in
  sign account.sk ctxt operations

let manager_operation ?counter ?(fee = Tez.zero) ?gas_limit ?storage_limit
    ?public_key ~source ctxt operation =
  ( match counter with
  | Some counter ->
      return counter
  | None ->
      Context.Contract.counter ctxt source )
  >>=? fun counter ->
  Context.get_constants ctxt
  >>=? fun c ->
  let gas_limit =
    let default = c.parametric.hard_gas_limit_per_operation in
    Option.value ~default gas_limit
  in
  let storage_limit =
    Option.value
      ~default:c.parametric.hard_storage_limit_per_operation
      storage_limit
  in
  Context.Contract.find_account ctxt source
  >>=? fun account ->
  let public_key = Option.value ~default:account.pk public_key in
  let counter = Z.succ counter in
  Context.Contract.is_public_key_revealed ctxt source
  >|=? function
  | true ->
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
  | false ->
      let op_reveal =
        Manager_operation
          {
            source = Signature.Public_key.hash public_key;
            fee = Tez.zero;
            counter;
            operation = Reveal public_key;
            gas_limit = Gas.Arith.integral_of_int_exn 10000;
            storage_limit = Z.zero;
          }
      in
      let op =
        Manager_operation
          {
            source = Signature.Public_key.hash public_key;
            fee;
            counter = Z.succ counter;
            operation;
            gas_limit;
            storage_limit;
          }
      in
      Contents_list (Cons (op_reveal, Single op))

let revelation ?(fee = Tez.zero) ctxt public_key =
  let pkh = Signature.Public_key.hash public_key in
  let source = Contract.implicit_contract pkh in
  Context.Contract.counter ctxt source
  >>=? fun counter ->
  Context.Contract.find_account ctxt source
  >|=? fun account ->
  let counter = Z.succ counter in
  let sop =
    Contents_list
      (Single
         (Manager_operation
            {
              source = Signature.Public_key.hash public_key;
              fee;
              counter;
              operation = Reveal public_key;
              gas_limit = Gas.Arith.integral_of_int_exn 10000;
              storage_limit = Z.zero;
            }))
  in
  sign account.sk ctxt sop

let failing_noop ctxt source arbitrary =
  let op = Contents_list (Single (Failing_noop arbitrary)) in
  Account.find source >>=? fun account -> return @@ sign account.sk ctxt op

let originated_contract op =
  let nonce = Contract.initial_origination_nonce (Operation.hash_packed op) in
  Contract.originated_contract nonce

exception Impossible

let origination ?counter ?delegate ~script ?(preorigination = None) ?public_key
    ?credit ?fee ?gas_limit ?storage_limit ctxt source =
  Context.Contract.find_account ctxt source
  >>=? fun account ->
  let default_credit = Tez.of_mutez @@ Int64.of_int 1000001 in
  let default_credit =
    WithExceptions.Option.to_exn ~none:Impossible default_credit
  in
  let credit = Option.value ~default:default_credit credit in
  let operation = Origination {delegate; script; credit; preorigination} in
  manager_operation
    ?counter
    ?public_key
    ?fee
    ?gas_limit
    ?storage_limit
    ~source
    ctxt
    operation
  >|=? fun sop ->
  let op = sign account.sk ctxt sop in
  (op, originated_contract op)

let baker_contract op =
  let nonce = Contract.initial_origination_nonce (Operation.hash_packed op) in
  Contract.baker_from_nonce nonce

let baker_registration ?counter ~consensus_key ~threshold ~owner_keys ?credit
    ?fee ?gas_limit ?storage_limit ctxt source =
  Context.Contract.find_account ctxt source
  >>=? fun account ->
  let default_credit = Tez.of_mutez @@ Int64.of_int 1000001 in
  let default_credit =
    WithExceptions.Option.to_exn ~none:Impossible default_credit
  in
  let credit = Option.value ~default:default_credit credit in
  let operation =
    Baker_registration {credit; consensus_key; threshold; owner_keys}
  in
  let public_key = Some account.pk in
  manager_operation
    ?counter
    ?public_key
    ?fee
    ?gas_limit
    ?storage_limit
    ~source
    ctxt
    operation
  >>=? fun sop ->
  let op = sign account.sk ctxt sop in
  let baker_hash = baker_contract op in
  let account =
    Account.
      {
        key = {pkh = account.pkh; pk = account.pk; sk = account.sk};
        baker = baker_hash;
      }
  in
  Account.add_baker account ;
  return (op, baker_hash)

let miss_signed_endorsement ?level ctxt =
  (match level with None -> Context.get_level ctxt | Some level -> ok level)
  >>?= fun level ->
  Context.get_endorser ctxt
  >>=? fun (real_baker, _slots) ->
  let baker = Account.find_alternate_baker real_baker in
  endorsement ~baker ~level ctxt ()

let transaction ?counter ?fee ?gas_limit ?storage_limit
    ?(parameters = Script.unit_parameter) ?(entrypoint = "default") ctxt
    (src : Contract.t) (dst : Contract.t) (amount : Tez.t) =
  let top = Transaction {amount; parameters; destination = dst; entrypoint} in
  (* for bakers, send the transaction from the consensus key *)
  ( match Contract.is_baker src with
  | None ->
      return src
  | Some baker ->
      Context.Baker.consensus_key ctxt baker
      >>=? fun consensus_key ->
      return
        (Contract.implicit_contract @@ Signature.Public_key.hash consensus_key)
  )
  >>=? fun src ->
  manager_operation
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    ~source:src
    ctxt
    top
  >>=? fun sop ->
  Context.Contract.find_account ctxt src
  >|=? fun account -> sign account.sk ctxt sop

let get_baker_contract_info ctxt contract =
  Context.Contract.storage ctxt contract
  >>=? Client_proto_multisig.multisig_contract_information_of_storage contract

let baker_action ?counter ?fee ?gas_limit ?storage_limit ctxt ~action
    (source : Contract.t) (baker : Baker_hash.t) =
  let contract = Contract.baker_contract baker in
  get_baker_contract_info ctxt contract
  >>=? fun info ->
  let payload =
    Client_proto_baker.mk_payload ~stored_counter:info.counter ~action
  in
  (* Make parameters bytes, used to create a signature *)
  let bytes =
    Client_proto_baker.mk_bytes_to_sign
      ~chain_id:Chain_id.zero
      ~payload
      contract
  in
  Account.find_baker baker
  >>=? fun account ->
  (* Sign the parameter bytes with the owner key *)
  let signature = Signature.sign account.key.sk bytes in
  (* Turn action into transaction parameters *)
  let parameters =
    Client_proto_baker.mk_singlesig_script_param ~payload ~signature
    |> Script.lazy_expr
  in
  let top =
    Transaction
      {
        amount = Tez.zero;
        parameters;
        destination = contract;
        entrypoint = Client_proto_baker.generic_entrypoint;
      }
  in
  manager_operation ?counter ?fee ?gas_limit ?storage_limit ~source ctxt top
  >>=? fun sop ->
  Context.Contract.find_account ctxt source
  >>=? fun manager -> return @@ sign manager.sk ctxt sop

let delegation ?fee ctxt source dst =
  let top = Delegation dst in
  manager_operation ?fee ~source ctxt top
  >>=? fun sop ->
  Context.Contract.find_account ctxt source
  >|=? fun account -> sign account.sk ctxt sop

let activation ctxt (pkh : Signature.Public_key_hash.t) activation_code =
  ( match pkh with
  | Ed25519 edpkh ->
      return edpkh
  | _ ->
      failwith
        "Wrong public key hash : %a - Commitments must be activated with an \
         Ed25519 encrypted public key hash"
        Signature.Public_key_hash.pp
        pkh )
  >|=? fun id ->
  let contents = Single (Activate_account {id; activation_code}) in
  let branch = Context.branch ctxt in
  {
    shell = {branch};
    protocol_data = Operation_data {contents; signature = None};
  }

let double_endorsement ctxt op1 op2 ~slot =
  let contents = Single (Double_endorsement_evidence {op1; op2; slot}) in
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

let proposals ctxt baker proposals =
  Context.Baker.consensus_key ctxt baker
  >>=? fun source ->
  let source = Signature.Public_key.hash source in
  Context.Vote.get_current_period ctxt
  >>=? fun {voting_period = {index; _}; _} ->
  let op = Proposals {source; period = index; proposals} in
  Account.find source
  >|=? fun account -> sign account.sk ctxt (Contents_list (Single op))

let ballot ctxt baker proposal ballot =
  Context.Baker.consensus_key ctxt baker
  >>=? fun source ->
  let source = Signature.Public_key.hash source in
  Context.Vote.get_current_period ctxt
  >>=? fun {voting_period = {index; _}; _} ->
  let op = Ballot {source; period = index; proposal; ballot} in
  Account.find source
  >|=? fun account -> sign account.sk ctxt (Contents_list (Single op))

let dummy_script =
  let open Micheline in
  Script.
    {
      code =
        lazy_expr
          (strip_locations
             (Seq
                ( 0,
                  [ Prim (0, K_parameter, [Prim (0, T_unit, [], [])], []);
                    Prim (0, K_storage, [Prim (0, T_unit, [], [])], []);
                    Prim
                      ( 0,
                        K_code,
                        [ Seq
                            ( 0,
                              [ Prim (0, I_CDR, [], []);
                                Prim
                                  ( 0,
                                    I_NIL,
                                    [Prim (0, T_operation, [], [])],
                                    [] );
                                Prim (0, I_PAIR, [], []) ] ) ],
                        [] ) ] )));
      storage = lazy_expr (strip_locations (Prim (0, D_Unit, [], [])));
    }

let dummy_script_cost = Test_tez.Tez.of_mutez_exn 9_500L
