(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type info = {
  balance : Tez.t;
  frozen_balance : Tez.t;
  frozen_balance_by_cycle : Baker.frozen_balance Cycle.Map.t;
  staking_balance : Tez.t;
  delegated_contracts : Contract_repr.t list;
  delegated_balance : Tez.t;
  deactivated : bool;
  grace_period : Cycle.t;
  consensus_key : Signature.Public_key.t;
  voting_power : int32;
  proof_levels : Raw_level.LSet.t;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun { balance;
           frozen_balance;
           frozen_balance_by_cycle;
           staking_balance;
           delegated_contracts;
           delegated_balance;
           deactivated;
           grace_period;
           consensus_key;
           voting_power;
           proof_levels } ->
      ( ( balance,
          frozen_balance,
          frozen_balance_by_cycle,
          staking_balance,
          delegated_contracts,
          delegated_balance,
          deactivated,
          grace_period,
          consensus_key,
          voting_power ),
        proof_levels ))
    (fun ( ( balance,
             frozen_balance,
             frozen_balance_by_cycle,
             staking_balance,
             delegated_contracts,
             delegated_balance,
             deactivated,
             grace_period,
             consensus_key,
             voting_power ),
           proof_levels ) ->
      {
        balance;
        frozen_balance;
        frozen_balance_by_cycle;
        staking_balance;
        delegated_contracts;
        delegated_balance;
        deactivated;
        grace_period;
        consensus_key;
        voting_power;
        proof_levels;
      })
    (merge_objs
       (obj10
          (req "balance" Tez.encoding)
          (req "frozen_balance" Tez.encoding)
          (req "frozen_balance_by_cycle" Baker.frozen_balance_by_cycle_encoding)
          (req "staking_balance" Tez.encoding)
          (req "delegated_contracts" (list Contract_repr.encoding))
          (req "delegated_balance" Tez.encoding)
          (req "deactivated" bool)
          (req "grace_period" Cycle.encoding)
          (req "consensus_key" Signature.Public_key.encoding)
          (req "voting_power" int32))
       (obj1 (req "proof_levels" Raw_level.LSet.encoding)))

module S = struct
  let path = RPC_path.(open_root / "context" / "bakers")

  open Data_encoding

  type list_query = {active : bool; inactive : bool}

  let list_query : list_query RPC_query.t =
    let open RPC_query in
    query (fun active inactive -> {active; inactive})
    |+ flag "active" (fun t -> t.active)
    |+ flag "inactive" (fun t -> t.inactive)
    |> seal

  type consensus_key_query = {offset : int32; level : int32 option}

  let consensus_key_query : consensus_key_query RPC_query.t =
    let open RPC_query in
    query (fun offset level -> {offset; level})
    |+ field "offset" RPC_arg.int32 0l (fun t -> t.offset)
    |+ opt_field "level" RPC_arg.int32 (fun t -> t.level)
    |> seal

  let list_baker =
    RPC_service.get_service
      ~description:"Lists all registered bakers."
      ~query:list_query
      ~output:(list Baker_hash.encoding)
      path

  let path = RPC_path.(path /: Baker_hash.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:"Everything about a baker."
      ~query:RPC_query.empty
      ~output:info_encoding
      path

  let balance =
    RPC_service.get_service
      ~description:
        "Returns the full balance of a given baker, including the frozen \
         balances."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "balance")

  let frozen_balance =
    RPC_service.get_service
      ~description:
        "Returns the total frozen balances of a given baker, this includes \
         the frozen deposits, rewards and fees."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "frozen_balance")

  let frozen_balance_by_cycle =
    RPC_service.get_service
      ~description:
        "Returns the frozen balances of a given baker, indexed by the cycle \
         by which it will be unfrozen."
      ~query:RPC_query.empty
      ~output:Baker.frozen_balance_by_cycle_encoding
      RPC_path.(path / "frozen_balance_by_cycle")

  let staking_balance =
    RPC_service.get_service
      ~description:
        "Returns the total amount of tokens delegated to a given baker. This \
         includes the balances of all the contracts that delegate to it, but \
         also the balance of the baker itself and its frozen fees and \
         deposits. The rewards do not count in the delegated balance until \
         they are unfrozen."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "staking_balance")

  let delegated_contracts =
    RPC_service.get_service
      ~description:
        "Returns the list of contracts that delegate to a given baker."
      ~query:RPC_query.empty
      ~output:(list Contract_repr.encoding)
      RPC_path.(path / "delegated_contracts")

  let delegated_balance =
    RPC_service.get_service
      ~description:
        "Returns the balances of all the contracts that delegate to a given \
         baker. This excludes the baker's own balance and its frozen balances."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "delegated_balance")

  let deactivated =
    RPC_service.get_service
      ~description:
        "Tells whether the baker is currently tagged as deactivated or not."
      ~query:RPC_query.empty
      ~output:bool
      RPC_path.(path / "deactivated")

  let grace_period =
    RPC_service.get_service
      ~description:
        "Returns the cycle by the end of which the baker might be deactivated \
         if she fails to execute any baker action. A deactivated baker might \
         be reactivated (without loosing any rolls) with a call to baker \
         script. For deactivated bakers, this value contains the cycle by \
         which they were deactivated."
      ~query:RPC_query.empty
      ~output:Cycle.encoding
      RPC_path.(path / "grace_period")

  let voting_power =
    RPC_service.get_service
      ~description:
        "The number of rolls in the vote listings for a given baker."
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "voting_power")

  let consensus_key =
    RPC_service.get_service
      ~description:
        "Get baker's consensus key that is being used for baking and endorsing."
      ~query:consensus_key_query
      ~output:Signature.Public_key.encoding
      RPC_path.(path / "consensus_key")

  let proof_levels =
    RPC_service.get_service
      ~description:
        "Returns the list of levels where a proof has been made against a \
         given delegate."
      ~query:RPC_query.empty
      ~output:Raw_level.LSet.encoding
      RPC_path.(path / "proof_levels")
end

let register () =
  let open Services_registration in
  register0 S.list_baker (fun ctxt q () ->
      Baker.list ctxt
      >>= fun bakers ->
      match q with
      | {active = true; inactive = false} ->
          filter_map_s
            (fun baker ->
              Baker.deactivated ctxt baker
              >>=? function true -> return_none | false -> return_some baker)
            bakers
      | {active = false; inactive = true} ->
          filter_map_s
            (fun baker ->
              Baker.deactivated ctxt baker
              >>=? function false -> return_none | true -> return_some baker)
            bakers
      | _ ->
          return bakers) ;
  register1 S.info (fun ctxt baker () () ->
      Baker.full_balance ctxt baker
      >>=? fun balance ->
      Baker.frozen_balance ctxt baker
      >>=? fun frozen_balance ->
      Baker.frozen_balance_by_cycle ctxt baker
      >>= fun frozen_balance_by_cycle ->
      Baker.staking_balance ctxt baker
      >>=? fun staking_balance ->
      Baker.delegated_contracts ctxt baker
      >>= fun delegated_contracts ->
      Baker.delegated_balance ctxt baker
      >>=? fun delegated_balance ->
      Baker.deactivated ctxt baker
      >>=? fun deactivated ->
      Baker.grace_period ctxt baker
      >>=? fun grace_period ->
      Baker.get_consensus_key ctxt baker
      >>=? fun consensus_key ->
      Vote.get_voting_power_free ctxt baker
      >>=? fun voting_power ->
      Baker.Proof.all ctxt baker
      >>=? fun proof_levels ->
      return
        {
          balance;
          frozen_balance;
          frozen_balance_by_cycle;
          staking_balance;
          delegated_contracts;
          delegated_balance;
          deactivated;
          grace_period;
          consensus_key;
          voting_power;
          proof_levels;
        }) ;
  register1 S.balance (fun ctxt baker () () -> Baker.full_balance ctxt baker) ;
  register1 S.frozen_balance (fun ctxt baker () () ->
      Baker.frozen_balance ctxt baker) ;
  register1 S.frozen_balance_by_cycle (fun ctxt baker () () ->
      Baker.frozen_balance_by_cycle ctxt baker >>= return) ;
  register1 S.staking_balance (fun ctxt baker () () ->
      Baker.staking_balance ctxt baker) ;
  register1 S.delegated_contracts (fun ctxt baker () () ->
      Baker.delegated_contracts ctxt baker >>= return) ;
  register1 S.delegated_balance (fun ctxt baker () () ->
      Baker.delegated_balance ctxt baker) ;
  register1 S.deactivated (fun ctxt baker () () ->
      Baker.deactivated ctxt baker) ;
  register1 S.grace_period (fun ctxt baker () () ->
      Baker.grace_period ctxt baker) ;
  register1 S.consensus_key (fun ctxt baker q () ->
      ( match q.level with
      | None ->
          return_none
      | Some level ->
          Lwt.return @@ Raw_level.of_int32 level
          >>=? fun level -> return_some level )
      >>=? fun level ->
      Baker.get_consensus_key ctxt baker ?level ~offset:q.offset) ;
  register1 S.voting_power (fun ctxt baker () () ->
      Vote.get_voting_power_free ctxt baker) ;
  register1 S.proof_levels (fun ctxt baker () () -> Baker.Proof.all ctxt baker)

let list ctxt block ?(active = true) ?(inactive = false) () =
  RPC_context.make_call0 S.list_baker ctxt block {active; inactive} ()

let info ctxt block baker =
  RPC_context.make_call1 S.info ctxt block baker () ()

let balance ctxt block baker =
  RPC_context.make_call1 S.balance ctxt block baker () ()

let frozen_balance ctxt block baker =
  RPC_context.make_call1 S.frozen_balance ctxt block baker () ()

let frozen_balance_by_cycle ctxt block baker =
  RPC_context.make_call1 S.frozen_balance_by_cycle ctxt block baker () ()

let staking_balance ctxt block baker =
  RPC_context.make_call1 S.staking_balance ctxt block baker () ()

let delegated_contracts ctxt block baker =
  RPC_context.make_call1 S.delegated_contracts ctxt block baker () ()

let delegated_balance ctxt block baker =
  RPC_context.make_call1 S.delegated_balance ctxt block baker () ()

let deactivated ctxt block baker =
  RPC_context.make_call1 S.deactivated ctxt block baker () ()

let grace_period ctxt block baker =
  RPC_context.make_call1 S.grace_period ctxt block baker () ()

let consensus_key ctxt ?level ?(offset = 0l) block baker =
  let level = Option.map Raw_level.to_int32 level in
  RPC_context.make_call1 S.consensus_key ctxt block baker {offset; level} ()

let voting_power ctxt block baker =
  RPC_context.make_call1 S.voting_power ctxt block baker () ()

let proof_levels ctxt block baker =
  RPC_context.make_call1 S.proof_levels ctxt block baker () ()

let requested_levels ~default ctxt cycles levels =
  match (levels, cycles) with
  | ([], []) ->
      ok [default]
  | (levels, cycles) ->
      (* explicitly fail when requested levels or cycle are in the past...
         or too far in the future... *)
      let levels =
        List.sort_uniq
          Level.compare
          (List.concat
             ( List.map (Level.from_raw ctxt) levels
             :: List.map (Level.levels_in_cycle ctxt) cycles ))
      in
      map
        (fun level ->
          let current_level = Level.current ctxt in
          if Level.(level <= current_level) then ok (level, None)
          else
            Baking.earlier_predecessor_timestamp ctxt level
            >|? fun timestamp -> (level, Some timestamp))
        levels

module Baking_rights = struct
  type t = {
    level : Raw_level.t;
    baker : baker_hash;
    priority : int;
    timestamp : Timestamp.t option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; baker; priority; timestamp} ->
        (level, baker, priority, timestamp))
      (fun (level, baker, priority, timestamp) ->
        {level; baker; priority; timestamp})
      (obj4
         (req "level" Raw_level.encoding)
         (req "baker" Baker_hash.encoding)
         (req "priority" uint16)
         (opt "estimated_time" Timestamp.encoding))

  module S = struct
    open Data_encoding

    let custom_root = RPC_path.(open_root / "helpers" / "baking_rights")

    type baking_rights_query = {
      levels : Raw_level.t list;
      cycles : Cycle.t list;
      bakers : baker_hash list;
      max_priority : int option;
      all : bool;
    }

    let baking_rights_query =
      let open RPC_query in
      query (fun levels cycles bakers max_priority all ->
          {levels; cycles; bakers; max_priority; all})
      |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
      |+ multi_field "cycle" Cycle.rpc_arg (fun t -> t.cycles)
      |+ multi_field "baker" Baker_hash.rpc_arg (fun t -> t.bakers)
      |+ opt_field "max_priority" RPC_arg.int (fun t -> t.max_priority)
      |+ flag "all" (fun t -> t.all)
      |> seal

    let baking_rights =
      RPC_service.get_service
        ~description:
          "Retrieves the list of bakers allowed to bake a block.\n\
           By default, it gives the best baking priorities for bakers that \
           have at least one opportunity below the 64th priority for the next \
           block.\n\
           Parameters `level` and `cycle` can be used to specify the (valid) \
           level(s) in the past or future at which the baking rights have to \
           be returned. Parameter `baker` can be used to restrict the results \
           to the given bakers. If parameter `all` is set, all the baking \
           opportunities for each baker at each level are returned, instead \
           of just the first one.\n\
           Returns the list of baking slots. Also returns the minimal \
           timestamps that correspond to these slots. The timestamps are \
           omitted for levels in the past, and are only estimates for levels \
           later that the next block, based on the hypothesis that all \
           predecessor blocks were baked at the first priority."
        ~query:baking_rights_query
        ~output:(list encoding)
        custom_root
  end

  let baking_priorities ctxt max_prio (level, pred_timestamp) =
    Baking.baking_priorities ctxt level
    >>=? fun contract_list ->
    let rec loop l acc priority =
      if Compare.Int.(priority > max_prio) then return (List.rev acc)
      else
        let (Misc.LCons (baker, next)) = l in
        ( match pred_timestamp with
        | None ->
            ok_none
        | Some pred_timestamp ->
            Baking.minimal_time ctxt priority pred_timestamp >|? Option.some )
        >>?= fun timestamp ->
        let acc = {level = level.level; baker; priority; timestamp} :: acc in
        next () >>=? fun l -> loop l acc (priority + 1)
    in
    loop contract_list [] 0

  let remove_duplicated_bakers rights =
    List.rev @@ fst
    @@ List.fold_left
         (fun (acc, previous) r ->
           if Baker_hash.Set.mem r.baker previous then (acc, previous)
           else (r :: acc, Baker_hash.Set.add r.baker previous))
         ([], Baker_hash.Set.empty)
         rights

  let register () =
    let open Services_registration in
    register0 S.baking_rights (fun ctxt q () ->
        requested_levels
          ~default:
            ( Level.succ ctxt (Level.current ctxt),
              Some (Timestamp.current ctxt) )
          ctxt
          q.cycles
          q.levels
        >>?= fun levels ->
        let max_priority =
          match q.max_priority with None -> 64 | Some max -> max
        in
        map_s (baking_priorities ctxt max_priority) levels
        >>=? fun rights ->
        let rights =
          if q.all then rights else List.map remove_duplicated_bakers rights
        in
        let rights = List.concat rights in
        match q.bakers with
        | [] ->
            return rights
        | _ :: _ as bakers ->
            let is_requested p =
              List.exists (Baker_hash.equal p.baker) bakers
            in
            return (List.filter is_requested rights))

  let get ctxt ?(levels = []) ?(cycles = []) ?(bakers = []) ?(all = false)
      ?max_priority block =
    RPC_context.make_call0
      S.baking_rights
      ctxt
      block
      {levels; cycles; bakers; max_priority; all}
      ()
end

module Endorsing_rights = struct
  type t = {
    level : Raw_level.t;
    baker : baker_hash;
    slots : int list;
    estimated_time : Time.t option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; baker; slots; estimated_time} ->
        (level, baker, slots, estimated_time))
      (fun (level, baker, slots, estimated_time) ->
        {level; baker; slots; estimated_time})
      (obj4
         (req "level" Raw_level.encoding)
         (req "baker" Baker_hash.encoding)
         (req "slots" (list uint16))
         (opt "estimated_time" Timestamp.encoding))

  module S = struct
    open Data_encoding

    let custom_root = RPC_path.(open_root / "helpers" / "endorsing_rights")

    type endorsing_rights_query = {
      levels : Raw_level.t list;
      cycles : Cycle.t list;
      bakers : baker_hash list;
    }

    let endorsing_rights_query =
      let open RPC_query in
      query (fun levels cycles bakers -> {levels; cycles; bakers})
      |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
      |+ multi_field "cycle" Cycle.rpc_arg (fun t -> t.cycles)
      |+ multi_field "baker" Baker_hash.rpc_arg (fun t -> t.bakers)
      |> seal

    let endorsing_rights =
      RPC_service.get_service
        ~description:
          "Retrieves the bakers allowed to endorse a block.\n\
           By default, it gives the endorsement slots for bakers that have at \
           least one in the next block.\n\
           Parameters `level` and `cycle` can be used to specify the (valid) \
           level(s) in the past or future at which the endorsement rights \
           have to be returned. Parameter `baker` can be used to restrict the \
           results to the given bakers.\n\
           Returns the list of endorsement slots. Also returns the minimal \
           timestamps that correspond to these slots. The timestamps are \
           omitted for levels in the past, and are only estimates for levels \
           later that the next block, based on the hypothesis that all \
           predecessor blocks were baked at the first priority."
        ~query:endorsing_rights_query
        ~output:(list encoding)
        custom_root
  end

  let endorsement_slots ctxt (level, estimated_time) =
    Baking.endorsement_rights ctxt level
    >>=? fun rights ->
    return
      (Baker_hash.Map.fold
         (fun baker (slots, _) acc ->
           {level = level.level; baker; slots; estimated_time} :: acc)
         rights
         [])

  let register () =
    let open Services_registration in
    register0 S.endorsing_rights (fun ctxt q () ->
        requested_levels
          ~default:(Level.current ctxt, Some (Timestamp.current ctxt))
          ctxt
          q.cycles
          q.levels
        >>?= fun levels ->
        map_s (endorsement_slots ctxt) levels
        >>=? fun rights ->
        let rights = List.concat rights in
        match q.bakers with
        | [] ->
            return rights
        | _ :: _ as bakers ->
            let is_requested p =
              List.exists (Baker_hash.equal p.baker) bakers
            in
            return (List.filter is_requested rights))

  let get ctxt ?(levels = []) ?(cycles = []) ?(bakers = []) block =
    RPC_context.make_call0
      S.endorsing_rights
      ctxt
      block
      {levels; cycles; bakers}
      ()
end

module Endorsing_power = struct
  let endorsing_power ctxt (operation, chain_id) =
    let (Operation_data data) = operation.protocol_data in
    match data.contents with
    | Single (Endorsement _) ->
        Baking.check_endorsement_rights
          ctxt
          chain_id
          {shell = operation.shell; protocol_data = data}
        >>=? fun (_, slots, _) -> return (List.length slots)
    | _ ->
        failwith "Operation is not an endorsement"

  module S = struct
    let endorsing_power =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Get the endorsing power of an endorsement, that is, the number of \
           slots that the endorser has"
        ~query:RPC_query.empty
        ~input:
          (obj2
             (req "endorsement_operation" Operation.encoding)
             (req "chain_id" Chain_id.encoding))
        ~output:int31
        RPC_path.(open_root / "endorsing_power")
  end

  let register () =
    let open Services_registration in
    register0 S.endorsing_power (fun ctxt () (op, chain_id) ->
        endorsing_power ctxt (op, chain_id))

  let get ctxt block op chain_id =
    RPC_context.make_call0 S.endorsing_power ctxt block () (op, chain_id)
end

module Required_endorsements = struct
  let required_endorsements ctxt block_delay =
    Baking.minimum_allowed_endorsements ctxt ~block_delay

  module S = struct
    type t = {block_delay : Period.t}

    let required_endorsements_query =
      let open RPC_query in
      query (fun block_delay -> {block_delay})
      |+ field "block_delay" Period.rpc_arg Period.zero (fun t ->
             t.block_delay)
      |> seal

    let required_endorsements =
      let open Data_encoding in
      RPC_service.get_service
        ~description:
          "Minimum number of endorsements for a block to be valid, given a \
           delay of the block's timestamp with respect to the minimum time to \
           bake at the block's priority"
        ~query:required_endorsements_query
        ~output:int31
        RPC_path.(open_root / "required_endorsements")
  end

  let register () =
    let open Services_registration in
    register0 S.required_endorsements (fun ctxt {block_delay} () ->
        return @@ required_endorsements ctxt block_delay)

  let get ctxt block block_delay =
    RPC_context.make_call0 S.required_endorsements ctxt block {block_delay} ()
end

module Minimal_valid_time = struct
  let minimal_valid_time ctxt ~priority ~endorsing_power =
    Baking.minimal_valid_time ctxt ~priority ~endorsing_power

  module S = struct
    type t = {priority : int; endorsing_power : int}

    let minimal_valid_time_query =
      let open RPC_query in
      query (fun priority endorsing_power -> {priority; endorsing_power})
      |+ field "priority" RPC_arg.int 0 (fun t -> t.priority)
      |+ field "endorsing_power" RPC_arg.int 0 (fun t -> t.endorsing_power)
      |> seal

    let minimal_valid_time =
      RPC_service.get_service
        ~description:
          "Minimal valid time for a block given a priority and an endorsing \
           power."
        ~query:minimal_valid_time_query
        ~output:Time.encoding
        RPC_path.(open_root / "minimal_valid_time")
  end

  let register () =
    let open Services_registration in
    register0 S.minimal_valid_time (fun ctxt {priority; endorsing_power} () ->
        Lwt.return @@ minimal_valid_time ctxt ~priority ~endorsing_power)

  let get ctxt block priority endorsing_power =
    RPC_context.make_call0
      S.minimal_valid_time
      ctxt
      block
      {priority; endorsing_power}
      ()
end

let register () =
  register () ;
  Baking_rights.register () ;
  Endorsing_rights.register () ;
  Endorsing_power.register () ;
  Required_endorsements.register () ;
  Minimal_valid_time.register ()

let baking_rights ctxt max_priority =
  let max = match max_priority with None -> 64 | Some m -> m in
  let level = Level.current ctxt in
  Baking_rights.baking_priorities ctxt max (level, None)
  >>=? fun l ->
  return
    ( level.level,
      List.map
        (fun {Baking_rights.baker; timestamp; _} -> (baker, timestamp))
        l )

let endorsing_power ctxt operation =
  Endorsing_power.endorsing_power ctxt operation

let required_endorsements ctxt delay =
  Required_endorsements.required_endorsements ctxt delay

let minimal_valid_time ctxt priority endorsing_power =
  Minimal_valid_time.minimal_valid_time ctxt priority endorsing_power
