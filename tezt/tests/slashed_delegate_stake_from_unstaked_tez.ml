(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Protocol / staking (stake from unstake vs. slashing)
   Invocation:   dune exec tezt/tests/main.exe -- \
                   --file slashed_delegate_stake_from_unstaked_tez.ml
   Subject:      A delegate and a co-staker unstake during the same cycle, so
                 both requests are served by a single aggregated unstaked
                 balance, and a double baking by the delegate in the previous
                 cycle burns part of it. From then until the requests unfreeze,
                 the delegate must not be able to stake: were it allowed to,
                 it would take its request out of that balance at the full
                 un-slashed amount and leave it short of the co-staker's
                 post-slash claim, so the co-staker's [finalize unstake] would
                 fail with a tez subtraction underflow and its funds would be
                 stuck.

                 The protocol's own [can_stake_from_unstake] guard does not
                 cover the whole window: it ignores slashes older than
                 [slashable_deposits_period + 1] cycles, which turns it
                 permissive one cycle before the requests become finalizable.
                 The Ushuaia plugin closes that cycle, refusing the stake
                 operation both at simulation ([lib_plugin/RPC.ml]) and at
                 block validation ([lib_plugin/block_validation.ml]).

                 This test pins that behaviour down: the stake is refused
                 across the whole window -- including when injected straight
                 into the mempool, bypassing simulation -- nothing moves while
                 it is refused, and once the requests unfreeze both parties
                 withdraw exactly their post-slash claims, which together
                 account for that balance. It therefore only passes on a
                 protocol whose plugin carries the fix.

                 The test runs on the sandbox parameters as they are: the cycle
                 plan and every expected amount are derived from the protocol
                 constants it reads from the node, so nothing has to be
                 overridden and a change of default cannot silently invalidate
                 an assertion. [stake_from_unstake_slashed_pot_poc.ml] holds
                 the attack this guards against, on the parameters of the
                 original report.
*)

(* [Constants_repr.slashing_delay = denunciation_period = 1]. This one is not
   parametric, so it cannot be read from the node like the others, and the cycle
   arithmetic below depends on it. *)
let slashing_delay = 1

(* [Percentage.one_hundred_percent]: percentage constants are per ten
   thousand. *)
let one_hundred_percent = 10_000

(* ------------------------------------------------------------------------- *)
(* Amounts                                                                   *)
(* ------------------------------------------------------------------------- *)

(* The co-staker unstakes most of its stake, the delegate a little, in the same
   cycle: the two requests then share one aggregated unstaked balance. *)
let costaker_stake = Tez.of_int 10_000

let costaker_unstake = Tez.of_int 9_000

let delegate_unstake = Tez.of_int 500

(* The co-staker's external stake must stay within the delegate's
   [limit_of_staking_over_baking] (set to 5 below), so the delegate needs at
   least a fifth of it as own frozen deposits. Bootstrap delegates are
   auto-frozen a twentieth of their balance at genesis, which is far above
   this. *)
let delegate_minimum_own_stake = Tez.of_int 2_000

(* ------------------------------------------------------------------------- *)
(* Helpers                                                                   *)
(* ------------------------------------------------------------------------- *)

let log_step counter msg =
  let color = Log.Color.(bold ++ FG.blue) in
  let prefix = "step" ^ string_of_int counter in
  Log.info ~color ~prefix "%s" msg

let current_level client =
  Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()

(* Bake with [keys] until the head is the first block of [target_cycle].

   [Client.bake_until_cycle] is not used: it targets level
   [target_cycle * blocks_per_cycle], which is the last level of the cycle
   before [target_cycle] (cycle [c] spans levels [c * blocks_per_cycle + 1] to
   [(c + 1) * blocks_per_cycle]), so it would leave the head one level short of
   the cycle this test wants to act in. *)
let bake_until_cycle ~blocks_per_cycle ~keys ~target_cycle client =
  let* RPC.{cycle; _} = current_level client in
  if target_cycle <= cycle then
    Test.fail
      "bake_until_cycle: head is already in cycle %d, cannot reach cycle %d"
      cycle
      target_cycle ;
  let target_level = (target_cycle * blocks_per_cycle) + 1 in
  let* () = Client.bake_until_level ~target_level ~keys client in
  let* RPC.{cycle; cycle_position; _} = current_level client in
  Check.((cycle = target_cycle) int)
    ~error_msg:"expected the head to be in cycle %R, got cycle %L" ;
  Check.((cycle_position = 0) int)
    ~error_msg:
      "expected the head to be the first block of the cycle, got position %L" ;
  unit

(* Mutez are JSON strings in the protocol encodings, but some RPCs decode
   them as integers: accept both. *)
let as_mutez json =
  match JSON.as_string_opt json with
  | Some s -> int_of_string s
  | None -> JSON.as_int json

(* The unstaked frozen deposits of ([delegate], [cycle]): the single balance
   that holds every unstake request made towards [delegate] during [cycle],
   the delegate's own as well as its stakers'. *)
let aggregated_unstaked_balance client ~delegate ~cycle =
  let* info =
    Client.RPC.call client @@ RPC.get_chain_block_context_delegate delegate
  in
  match
    List.find_opt
      (fun entry -> JSON.(entry |-> "cycle" |> as_int) = cycle)
      JSON.(info |-> "total_unstaked_per_cycle" |> as_list)
  with
  | None ->
      Test.fail
        "the delegate reports no aggregated unstaked balance for cycle %d"
        cycle
  | Some entry -> return (as_mutez JSON.(entry |-> "deposit"))

let check_aggregated_unstaked_balance client ~delegate ~cycle ~expected ~msg =
  let* unstaked_balance = aggregated_unstaked_balance client ~delegate ~cycle in
  Check.((unstaked_balance = expected) int)
    ~error_msg:
      (sf
         "%s: the aggregated unstaked balance of cycle %d is %%L mutez, \
          expected %%R"
         msg
         cycle) ;
  Log.info
    "aggregated unstaked balance of cycle %d = %d mutez (%s)"
    cycle
    unstaked_balance
    msg ;
  unit

(* The cycles of the unstake requests of [contract] that are not finalizable
   yet. *)
let unfinalizable_request_cycles client ~contract =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_unstake_requests contract
  in
  return
    JSON.(
      json |-> "unfinalizable" |-> "requests" |> as_list
      |> List.map (fun request -> request |-> "cycle" |> as_int))

(* Any well-formed context hash: the forged header is never validated as a
   block, only hashed and signature-checked. *)
let arbitrary_context_hash =
  "CoUeJrcPBj3T3iJL3PY4jZHnmZa5rRZ87VQPdSBNBcwZRMWJGh9j"

(* [Validate.check_double_baking_evidence] requires the two denounced headers
   to share their level and round, to hash in increasing order, and to be
   validly signed by the delegate that owns the baking rights at that level and
   round. It does not require either of them to be a valid block. So a genuine
   block of [culprit] plus a copy of it with a corrupted context hash, re-signed
   with [culprit]'s own key, is accepted evidence of a double bake, and it
   exercises the full node validation path. On a real network the same evidence
   comes from an actual equivocation. *)
let forge_double_baking_evidence ~protocol ~culprit ~level client =
  let block = string_of_int level in
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block ()
  in
  (* [/header] returns the block header proper plus [hash], [chain_id] and
     [protocol]; the operation carries the header alone. *)
  let bh1 =
    JSON.filter_object header (fun name _ ->
        not (List.mem name ["hash"; "chain_id"; "protocol"]))
  in
  let corrupt_context json =
    JSON.update
      "context"
      (fun _ -> JSON.annotate ~origin:__LOC__ (`String arbitrary_context_hash))
      json
  in
  let bh2_unsigned =
    JSON.filter_object (corrupt_context bh1) (fun name _ -> name <> "signature")
  in
  let encoding_name suffix =
    sf "%s.block_header%s" (Protocol.encoding_prefix protocol) suffix
  in
  let* unsigned_hex =
    let* hex =
      Codec.encode
        ~name:(encoding_name ".unsigned")
        (JSON.unannotate bh2_unsigned)
    in
    return (String.trim hex)
  in
  (* [octez-client sign block] outputs the signature in hexadecimal, so it can
     be appended to the unsigned header to obtain the signed one. *)
  let* signature =
    let* signature =
      Client.sign_block client unsigned_hex ~delegate:culprit.Account.alias
    in
    return (String.trim signature)
  in
  let signed_hex = unsigned_hex ^ signature in
  let* bh2 = Codec.decode ~name:(encoding_name "") signed_hex in
  let* hex1 =
    let* hex = Codec.encode ~name:(encoding_name "") (JSON.unannotate bh1) in
    return (String.trim hex)
  in
  (* [Block_header.hash] is the Blake2b hash of the serialised header. *)
  let block_hash hex =
    Tezos_crypto.Hashed.Block_hash.hash_bytes [Hex.to_bytes (`Hex hex)]
  in
  let hash1 = block_hash hex1 and hash2 = block_hash signed_hex in
  (* Guards against a drift between the encoding used here and the header the
     node actually hashed. *)
  Check.(
    (Tezos_crypto.Hashed.Block_hash.to_b58check hash1
    = JSON.(header |-> "hash" |> as_string))
      string)
    ~error_msg:"recomputed the block hash as %L, the node reports %R" ;
  (* The protocol rejects the evidence unless [Block_hash.(hash1 < hash2)], so
     that a single double bake cannot yield two distinct evidences. *)
  let bh1, bh2 =
    if Tezos_crypto.Hashed.Block_hash.compare hash1 hash2 < 0 then (bh1, bh2)
    else (bh2, bh1)
  in
  return
    (`O
       [
         ("kind", `String "double_baking_evidence");
         ("bh1", JSON.unannotate bh1);
         ("bh2", JSON.unannotate bh2);
       ])

let inject_double_baking_evidence ~protocol ~culprit ~level client =
  let* evidence =
    forge_double_baking_evidence ~protocol ~culprit ~level client
  in
  let* branch = Client.RPC.call client @@ RPC.get_chain_block_hash () in
  let operation = Operation.make ~branch ~kind:Anonymous (`A [evidence]) in
  let* (`OpHash oph) = Operation.inject operation client in
  return oph

(* [validation_pass] is 2 for anonymous operations and 3 for manager ones. *)
let is_operation_in_block ~validation_pass operations oph =
  JSON.(operations |=> validation_pass |> as_list)
  |> List.exists (fun op -> JSON.(op |-> "hash" |> as_string) = oph)

(* A [stake] operation is a self-transfer to the [stake] entrypoint. Building it
   here rather than through [octez-client stake] is what lets the test inject it
   without the client's simulation getting a say. The gas limit is above what
   the operation consumes ([Manager.make]'s default for a transfer is too low),
   and
   the fee above the mempool's minimum for that gas. *)
let stake_operation ~(delegate : Account.key) ~amount =
  Operation.Manager.(
    make ~source:delegate ~fee:2_000 ~gas_limit:5_000
    @@ call
         ~dest:delegate.public_key_hash
         ~amount:(Tez.to_mutez amount)
         ~entrypoint:"stake"
         ())

(* ------------------------------------------------------------------------- *)
(* The test                                                                  *)
(* ------------------------------------------------------------------------- *)

let slashed_delegate_cannot_stake_from_unstake =
  Protocol.register_test
    ~__FILE__
    ~title:
      "a slashed delegate cannot stake from its unfinalizable unstake requests"
    ~tags:
      [
        Tag.layer1;
        Tag.slow;
        "staking";
        "unstake";
        "slashing";
        "double";
        "baking";
        "denunciation";
      ]
      (* Pinned to PsUshuai, the protocol whose plugin carries the guard. *)
    ~supports:(Protocol.Between_protocols (025, 025))
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  (* The delegate that gets denounced, and the co-staker staking towards it. *)
  let slashed_delegate = Constant.bootstrap2 in
  let bakers =
    List.map
      (fun (key : Account.key) -> key.alias)
      [
        Constant.bootstrap1;
        Constant.bootstrap2;
        Constant.bootstrap3;
        Constant.bootstrap4;
        Constant.bootstrap5;
      ]
  in
  (* [record_denunciation] forbids the culprit from taking part in consensus as
     soon as the evidence is applied, so it must be left out of the baking keys
     from that point on. *)
  let bakers_without_slashed_delegate =
    List.filter (fun alias -> alias <> slashed_delegate.alias) bakers
  in

  log_step 1 "Start a sandbox on the default parameters" ;
  let* node = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client in

  (* Everything below is derived from the constants the node reports, so the
     test needs no parameter override and cannot drift from the sandbox
     defaults. *)
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let constant name = JSON.(constants |-> name |> as_int) in
  let blocks_per_cycle = constant "blocks_per_cycle" in
  let consensus_rights_delay = constant "consensus_rights_delay" in
  let parameters_activation_delay =
    constant "delegate_parameters_activation_delay"
  in
  let slash_percentage =
    constant "percentage_of_frozen_deposits_slashed_per_double_baking"
  in
  let bake_until_cycle = bake_until_cycle ~blocks_per_cycle in
  (* [Constants_storage.slashable_deposits_period = consensus_rights_delay] and
     [Constants_repr.Derived.unstake_finalization_delay
        = consensus_rights_delay + slashing_delay]. *)
  let slashable_deposits_period = consensus_rights_delay in
  let unstake_finalization_delay = consensus_rights_delay + slashing_delay in

  (* Cycle plan. Staking parameters set in cycle [c] activate in cycle
     [c + delegate_parameters_activation_delay + 1], which is the earliest the
     co-staker can stake; the misbehaviour then needs a couple of cycles of
     margin so that the stake is frozen when the baking rights of the
     misbehaviour level are computed. *)
  let staking_cycle = parameters_activation_delay + 1 in
  let misbehaviour_cycle = staking_cycle + 2 in
  let request_cycle = misbehaviour_cycle + 1 in
  (* Denunciations of cycle [m] are applied at the end of the first cycle
     strictly greater than [m], so the burn lands at the end of [request_cycle]
     and is observable from the next cycle on. *)
  let slash_observed_cycle = request_cycle + 1 in
  (* Where the protocol's own guard stops covering the slash: it only rejects a
     stake-from-unstake while the slashing history holds a cycle
     >= [current - (slashable_deposits_period + 1)]. *)
  let protocol_guard_permissive_cycle =
    misbehaviour_cycle + slashing_delay + slashable_deposits_period + 1
  in
  (* Where the plugin filter stops covering it: it rejects a delegate's stake
     while the history holds a cycle >= [current - (unstake_finalization_delay +
     1)]. *)
  let last_filtered_cycle =
    misbehaviour_cycle + unstake_finalization_delay + 1
  in
  (* A request made in cycle [c] becomes finalizable in cycle
     [c + unstake_finalization_delay + 1]. *)
  let finalization_cycle = request_cycle + unstake_finalization_delay + 1 in

  (* Amounts. The burn on the aggregated balance rounds the slashed part down
     ([compute_punishing_amount] uses [mul_percentage ~rounding:`Down]), while
     each claim rounds it up ([apply_slashes] uses [~rounding:`Up]). *)
  let burn amount = amount * slash_percentage / one_hundred_percent in
  let claim amount =
    amount
    - ((amount * slash_percentage) + one_hundred_percent - 1)
      / one_hundred_percent
  in
  let unstaked_after_requests =
    Tez.to_mutez delegate_unstake + Tez.to_mutez costaker_unstake
  in
  let unstaked_after_slash =
    unstaked_after_requests - burn unstaked_after_requests
  in
  let costaker_claim = claim (Tez.to_mutez costaker_unstake) in
  let delegate_claim = claim (Tez.to_mutez delegate_unstake) in
  (* Whatever the two claims leave behind, once both are paid. Zero unless the
     rounding of the burn and of the claims disagree. *)
  let dust = unstaked_after_slash - costaker_claim - delegate_claim in

  Log.info
    "constants: blocks_per_cycle = %d, consensus_rights_delay = %d, \
     slashing_delay = %d, double baking slash = %d/%d"
    blocks_per_cycle
    consensus_rights_delay
    slashing_delay
    slash_percentage
    one_hundred_percent ;
  Log.info
    "plan: stake in cycle %d, misbehave in %d, unstake in %d, burn seen in %d, \
     filter closed through %d (protocol guard permissive from %d), requests \
     finalizable in %d"
    staking_cycle
    misbehaviour_cycle
    request_cycle
    slash_observed_cycle
    last_filtered_cycle
    protocol_guard_permissive_cycle
    finalization_cycle ;

  (* The cycle the protocol's own guard stops covering is inside the window
     where the requests are still frozen: that gap is what the plugin filter
     exists for, and the test would be vacuous without it. *)
  Check.((protocol_guard_permissive_cycle < finalization_cycle) int)
    ~error_msg:
      "the scenario needs the protocol guard to turn permissive (cycle %L) \
       strictly before the requests become finalizable (cycle %R)" ;
  (* The filter must release exactly when the funds unfreeze: no gap to
     exploit, and no cycle where a delegate is locked out for nothing. *)
  Check.((last_filtered_cycle = finalization_cycle - 1) int)
    ~error_msg:
      "the plugin filter should stay closed up to the cycle before the \
       requests become finalizable: last filtered %L, expected %R" ;
  (* The accounting identity the scenario protects: the balance covers both
     post-slash claims. *)
  Check.((dust >= 0) int)
    ~error_msg:
      "the burnt aggregated unstaked balance falls short of the two claims by \
       %L mutez" ;

  log_step
    2
    (sf
       "Register the delegate (%s) and the co-staker staking towards it"
       slashed_delegate.alias) ;
  (* [edge_of_baking_over_staking = 1] hands the whole staking reward to the
     baker, so the stakers' share of the frozen deposits never grows: the
     pseudotoken exchange rate stays exactly 1 and the amounts stay exact. *)
  let* () =
    Client.set_delegate_parameters
      ~delegate:slashed_delegate.alias
      ~limit:"5"
      ~edge:"1"
      client
  in
  let* () = Client.bake_for_and_wait ~keys:bakers client in
  (* The delegate already has frozen deposits from the genesis auto-freeze, and
     they are what the co-staker's external stake is measured against. The test
     deliberately does not stake as the delegate here: a delegate self-stake is
     exactly the operation a shell-side filter may refuse, and such a refusal is
     silent when it happens at block construction, so relying on one during
     setup would let the scenario proceed on wrong assumptions. *)
  let* delegate_own_frozen =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         slashed_delegate.public_key_hash
  in
  Check.((delegate_own_frozen >= Tez.to_mutez delegate_minimum_own_stake) int)
    ~error_msg:
      "the delegate has %L mutez of own frozen deposits, at least %R are \
       needed for the co-staker's external stake to fit under \
       limit_of_staking_over_baking" ;
  let* costaker = Client.gen_and_show_keys ~alias:"costaker" client in
  let* () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:(Tez.of_int 20_000)
      ~giver:Constant.bootstrap1.alias
      ~receiver:costaker.alias
      client
  in
  let* () = Client.bake_for_and_wait ~keys:bakers client in
  let* () =
    Client.set_delegate
      ~src:costaker.alias
      ~delegate:slashed_delegate.public_key_hash
      client
  in
  let* () = Client.bake_for_and_wait ~keys:bakers client in
  let* () = bake_until_cycle ~keys:bakers ~target_cycle:staking_cycle client in
  let* () = Client.stake costaker_stake ~staker:costaker.alias client in
  let* () = Client.bake_for_and_wait ~keys:bakers client in
  let* staked_balance =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         costaker.public_key_hash
  in
  Check.((staked_balance = Tez.to_mutez costaker_stake) int)
    ~error_msg:"the co-staker staked %L mutez towards the delegate, expected %R" ;

  log_step
    3
    (sf
       "Bake up to cycle %d and have the delegate bake the block that will be \
        double baked"
       misbehaviour_cycle) ;
  let* () =
    bake_until_cycle ~keys:bakers ~target_cycle:misbehaviour_cycle client
  in
  (* One block baked by the delegate alone: whichever round it owns at that
     level, it is the baking rights owner there, which is what the denunciation
     checks. *)
  let* () = Client.bake_for_and_wait ~keys:[slashed_delegate.alias] client in
  let* misbehaviour_level = Client.level client in
  let* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata_raw ()
  in
  Check.(
    (JSON.(metadata |-> "baker" |> as_string) = slashed_delegate.public_key_hash)
      string)
    ~error_msg:
      "the block to be double baked was baked by %L, expected the culprit %R" ;
  Log.info
    "the delegate will be denounced for double baking at level %d (cycle %d)"
    misbehaviour_level
    misbehaviour_cycle ;

  log_step
    4
    (sf
       "At cycle %d, the delegate unstakes %s and the co-staker unstakes %s so \
        that both requests land in the same aggregated unstaked balance"
       request_cycle
       (Tez.to_string delegate_unstake)
       (Tez.to_string costaker_unstake)) ;
  let* () = bake_until_cycle ~keys:bakers ~target_cycle:request_cycle client in
  let* () =
    Client.unstake delegate_unstake ~staker:slashed_delegate.alias client
  in
  let* () = Client.unstake costaker_unstake ~staker:costaker.alias client in
  let* () = Client.bake_for_and_wait ~keys:bakers client in
  let* () =
    check_aggregated_unstaked_balance
      client
      ~delegate:slashed_delegate.public_key_hash
      ~cycle:request_cycle
      ~expected:unstaked_after_requests
      ~msg:"both unstake requests share one aggregated unstaked balance"
  in

  log_step
    5
    (sf
       "Inject double baking evidence against the delegate for cycle %d"
       misbehaviour_cycle) ;
  let* oph =
    inject_double_baking_evidence
      ~protocol
      ~culprit:slashed_delegate
      ~level:misbehaviour_level
      client
  in
  let* () =
    Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
  in
  let* operations =
    Client.RPC.call client @@ RPC.get_chain_block_operations ()
  in
  if not (is_operation_in_block ~validation_pass:2 operations oph) then
    Test.fail "the double baking evidence %s was not included in the block" oph ;
  Log.info
    "denunciation %s included; the delegate is now forbidden from baking"
    oph ;

  log_step
    6
    (sf
       "At cycle %d the denunciation is applied and the aggregated unstaked \
        balance is burnt"
       slash_observed_cycle) ;
  let* () =
    bake_until_cycle
      ~keys:bakers_without_slashed_delegate
      ~target_cycle:slash_observed_cycle
      client
  in
  let* () =
    check_aggregated_unstaked_balance
      client
      ~delegate:slashed_delegate.public_key_hash
      ~cycle:request_cycle
      ~expected:unstaked_after_slash
      ~msg:"after the burn"
  in

  log_step
    7
    (sf
       "From cycle %d to cycle %d, the delegate's stake is refused: the slash \
        is still unfinalizable"
       slash_observed_cycle
       last_filtered_cycle) ;
  (* The delegate tries to stake exactly its outstanding request. Were it
     accepted, the request would be consumed at its full un-slashed value and
     the balance would drop below what the co-staker is owed. Nothing may move:
     not that balance, not the request, not the liquid balance. *)
  let check_stake_refused () =
    let* RPC.{cycle; _} = current_level client in
    let* unstaked_balance_before =
      aggregated_unstaked_balance
        client
        ~delegate:slashed_delegate.public_key_hash
        ~cycle:request_cycle
    in
    let* balance_before =
      Client.get_balance_for ~account:slashed_delegate.alias client
    in
    let* () =
      Client.spawn_stake delegate_unstake ~staker:slashed_delegate.alias client
      |> Process.check_error
           ~msg:(rex "A delegate cannot stake while it has been slashed")
    in
    (* The operation is refused at simulation, so nothing is injected; bake
       regardless, so that a lingering mempool entry would still show up. *)
    let* () =
      Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
    in
    let* cycles =
      unfinalizable_request_cycles
        client
        ~contract:slashed_delegate.public_key_hash
    in
    if not (List.mem request_cycle cycles) then
      Test.fail
        "at cycle %d, the delegate's unstake request of cycle %d disappeared \
         even though the stake operation was refused"
        cycle
        request_cycle ;
    let* unstaked_balance_after =
      aggregated_unstaked_balance
        client
        ~delegate:slashed_delegate.public_key_hash
        ~cycle:request_cycle
    in
    Check.((unstaked_balance_after = unstaked_balance_before) int)
      ~error_msg:
        "the aggregated unstaked balance moved from %R to %L while the stake \
         was refused" ;
    let* balance_after =
      Client.get_balance_for ~account:slashed_delegate.alias client
    in
    Check.((balance_after = balance_before) Tez.typ)
      ~error_msg:
        "the delegate's liquid balance moved from %R to %L while the stake was \
         refused" ;
    Log.info
      "cycle %d: stake refused, aggregated unstaked balance still %d mutez, \
       request still outstanding"
      cycle
      unstaked_balance_after ;
    unit
  in
  (* Refused as soon as the slash is applied, while the protocol's own guard
     still covers this cycle too. *)
  let* () = check_stake_refused () in
  (* And still refused at [protocol_guard_permissive_cycle], where the protocol
     guard has gone permissive and the plugin filter is the only thing left:
     this is the cycle that used to drain the balance. *)
  let* () =
    bake_until_cycle
      ~keys:bakers_without_slashed_delegate
      ~target_cycle:protocol_guard_permissive_cycle
      client
  in
  let* () = check_stake_refused () in

  (* Refusing at simulation only protects a client that simulates. Inject the
     same stake straight into the mempool, bypassing the client altogether: the
     mempool has no such check and accepts it, and the block must still come out
     without it. This is the block-validation half of the guard, and the half
     that actually protects the chain. *)
  let* balance_before_direct_injection =
    Client.get_balance_for ~account:slashed_delegate.alias client
  in
  let* (`OpHash direct_oph) =
    Operation.Manager.inject
      [stake_operation ~delegate:slashed_delegate ~amount:delegate_unstake]
      client
  in
  let* mempool = Mempool.get_mempool client in
  if not (List.mem direct_oph mempool.Mempool.validated) then
    Test.fail
      "the directly injected stake %s is not in the mempool: the test cannot \
       show that block validation is what excludes it"
      direct_oph ;
  Log.info
    "stake %s accepted by the mempool, now baking a block that must exclude it"
    direct_oph ;
  let* () =
    Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
  in
  let* operations =
    Client.RPC.call client @@ RPC.get_chain_block_operations ()
  in
  if is_operation_in_block ~validation_pass:3 operations direct_oph then
    Test.fail
      "the block includes the directly injected stake %s: block validation \
       must exclude it even when simulation is bypassed"
      direct_oph ;
  let* cycles =
    unfinalizable_request_cycles
      client
      ~contract:slashed_delegate.public_key_hash
  in
  if not (List.mem request_cycle cycles) then
    Test.fail
      "the delegate's unstake request of cycle %d disappeared after the \
       directly injected stake"
      request_cycle ;
  let* () =
    check_aggregated_unstaked_balance
      client
      ~delegate:slashed_delegate.public_key_hash
      ~cycle:request_cycle
      ~expected:unstaked_after_slash
      ~msg:"after a directly injected stake was excluded from the block"
  in
  let* balance_after_direct_injection =
    Client.get_balance_for ~account:slashed_delegate.alias client
  in
  Check.(
    (balance_after_direct_injection = balance_before_direct_injection) Tez.typ)
    ~error_msg:
      "the delegate's liquid balance moved from %R to %L, but the excluded \
       operation should not even have paid its fee" ;
  (* The refused operation stays in the mempool, where its counter would clash
     with the delegate's later operations. Ban it so the rest of the test speaks
     for itself. *)
  let* _ =
    Client.RPC.call client
    @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String direct_oph)) ()
  in

  log_step
    8
    (sf
       "At cycle %d the requests unfreeze: the co-staker withdraws %d mutez \
        and the delegate %d mutez"
       finalization_cycle
       costaker_claim
       delegate_claim) ;
  let* () =
    bake_until_cycle
      ~keys:bakers_without_slashed_delegate
      ~target_cycle:finalization_cycle
      client
  in
  let finalizable_balance who =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_unstaked_finalizable_balance who
  in
  (* Each party is owed its request minus the slash, and the aggregated
     unstaked balance covers both. *)
  let* costaker_finalizable = finalizable_balance costaker.public_key_hash in
  Check.((costaker_finalizable = costaker_claim) int)
    ~error_msg:"the co-staker's finalizable balance is %L mutez, expected %R" ;
  let* delegate_finalizable =
    finalizable_balance slashed_delegate.public_key_hash
  in
  Check.((delegate_finalizable = delegate_claim) int)
    ~error_msg:"the delegate's finalizable balance is %L mutez, expected %R" ;

  (* The co-staker withdraws. The balance delta is exact; the liquid balance
     only has to grow, since the operation pays a fee. *)
  let* costaker_balance_before =
    Client.get_balance_for ~account:costaker.alias client
  in
  let* () = Client.finalize_unstake ~staker:costaker.alias client in
  let* () =
    Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
  in
  let* costaker_balance_after =
    Client.get_balance_for ~account:costaker.alias client
  in
  Check.(
    (Tez.to_mutez costaker_balance_after > Tez.to_mutez costaker_balance_before)
      int)
    ~error_msg:"the co-staker's liquid balance did not grow: %L is not above %R" ;
  let* costaker_finalizable = finalizable_balance costaker.public_key_hash in
  Check.((costaker_finalizable = 0) int)
    ~error_msg:"the co-staker still has %L mutez to finalize, expected %R" ;
  let* () =
    check_aggregated_unstaked_balance
      client
      ~delegate:slashed_delegate.public_key_hash
      ~cycle:request_cycle
      ~expected:(delegate_claim + dust)
      ~msg:
        "after the co-staker withdrew its claim, the aggregated unstaked \
         balance is exactly the delegate's claim"
  in

  (* The delegate withdraws what is left, which is exactly its own claim. *)
  let* () = Client.finalize_unstake ~staker:slashed_delegate.alias client in
  let* () =
    Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
  in
  let* delegate_finalizable =
    finalizable_balance slashed_delegate.public_key_hash
  in
  Check.((delegate_finalizable = 0) int)
    ~error_msg:"the delegate still has %L mutez to finalize, expected %R" ;
  let* () =
    check_aggregated_unstaked_balance
      client
      ~delegate:slashed_delegate.public_key_hash
      ~cycle:request_cycle
      ~expected:dust
      ~msg:"both claims paid"
  in

  (* The filter is a window, not a ban: with the requests gone and the slash out
     of the window, the delegate can stake again -- from its liquid balance. *)
  let* staked_before =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         slashed_delegate.public_key_hash
  in
  let* () =
    Client.stake delegate_unstake ~staker:slashed_delegate.alias client
  in
  let* () =
    Client.bake_for_and_wait ~keys:bakers_without_slashed_delegate client
  in
  let* staked_after =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_staked_balance
         slashed_delegate.public_key_hash
  in
  Check.((staked_after - staked_before = Tez.to_mutez delegate_unstake) int)
    ~error_msg:
      "the delegate's staked balance grew by %L mutez after staking again, \
       expected %R" ;

  Log.info
    "SUMMARY: the delegate's stake was refused at every cycle from %d to %d, \
     including when injected past the client, so the aggregated unstaked \
     balance stayed at %d mutez and paid both claims in full (co-staker %d, \
     delegate %d, %d left); the delegate could stake again as soon as the \
     requests unfroze."
    slash_observed_cycle
    last_filtered_cycle
    unstaked_after_slash
    costaker_claim
    delegate_claim
    dust ;
  unit

let register ~protocols = slashed_delegate_cannot_stake_from_unstake protocols
