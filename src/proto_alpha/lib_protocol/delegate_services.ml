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
  delegated_contracts : Contract.t list;
  delegated_balance : Tez.t;
  deactivated : bool;
  grace_period : Cycle.t;
  voting_power : int32;
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
           voting_power } ->
      ( balance,
        frozen_balance,
        frozen_balance_by_cycle,
        staking_balance,
        delegated_contracts,
        delegated_balance,
        deactivated,
        grace_period,
        voting_power ))
    (fun ( balance,
           frozen_balance,
           frozen_balance_by_cycle,
           staking_balance,
           delegated_contracts,
           delegated_balance,
           deactivated,
           grace_period,
           voting_power ) ->
      {
        balance;
        frozen_balance;
        frozen_balance_by_cycle;
        staking_balance;
        delegated_contracts;
        delegated_balance;
        deactivated;
        grace_period;
        voting_power;
      })
    (obj9
       (req "balance" Tez.encoding)
       (req "frozen_balance" Tez.encoding)
       (req "frozen_balance_by_cycle" Baker.frozen_balance_by_cycle_encoding)
       (req "staking_balance" Tez.encoding)
       (req "delegated_contracts" (list Contract.encoding))
       (req "delegated_balance" Tez.encoding)
       (req "deactivated" bool)
       (req "grace_period" Cycle.encoding)
       (req "voting_power" int32))

module S = struct
  let path = RPC_path.(open_root / "context" / "delegates")

  open Data_encoding

  type list_query = {active : bool; inactive : bool}

  let list_query : list_query RPC_query.t =
    let open RPC_query in
    query (fun active inactive -> {active; inactive})
    |+ flag "active" (fun t -> t.active)
    |+ flag "inactive" (fun t -> t.inactive)
    |> seal

  let add_deprecated_notice description =
    Format.sprintf
      "%s DEPRECATED: new RPC endpoint at \"bakers\" path segment instead of \
       \"delegates\", where baker hash is the new baker identifier."
      description

  let list_delegate =
    RPC_service.get_service
      ~description:(add_deprecated_notice "Lists all registered delegates.")
      ~query:list_query
      ~output:(list Signature.Public_key_hash.encoding)
      path

  let path = RPC_path.(path /: Signature.Public_key_hash.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:(add_deprecated_notice "Everything about a delegate.")
      ~query:RPC_query.empty
      ~output:info_encoding
      path

  let balance =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the full balance of a given delegate, including the \
            frozen balances.")
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "balance")

  let frozen_balance =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the total frozen balances of a given delegate, this \
            includes the frozen deposits, rewards and fees.")
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "frozen_balance")

  let frozen_balance_by_cycle =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the frozen balances of a given delegate, indexed by the \
            cycle by which it will be unfrozen.")
      ~query:RPC_query.empty
      ~output:Baker.frozen_balance_by_cycle_encoding
      RPC_path.(path / "frozen_balance_by_cycle")

  let staking_balance =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the total amount of tokens delegated to a given delegate. \
            This includes the balances of all the contracts that delegate to \
            it, but also the balance of the delegate itself and its frozen \
            fees and deposits. The rewards do not count in the delegated \
            balance until they are unfrozen.")
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "staking_balance")

  let delegated_contracts =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the list of contracts that delegate to a given delegate.")
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      RPC_path.(path / "delegated_contracts")

  let delegated_balance =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the balances of all the contracts that delegate to a \
            given delegate. This excludes the delegate's own balance and its \
            frozen balances.")
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "delegated_balance")

  let deactivated =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Tells whether the delegate is currently tagged as deactivated or \
            not.")
      ~query:RPC_query.empty
      ~output:bool
      RPC_path.(path / "deactivated")

  let grace_period =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "Returns the cycle by the end of which the delegate might be \
            deactivated if she fails to execute any delegate action. A \
            deactivated delegate might be reactivated (without loosing any \
            rolls) with a call to baker script. For deactivated delegates, \
            this value contains the cycle by which they were deactivated.")
      ~query:RPC_query.empty
      ~output:Cycle.encoding
      RPC_path.(path / "grace_period")

  let voting_power =
    RPC_service.get_service
      ~description:
        (add_deprecated_notice
           "The number of rolls in the vote listings for a given delegate.")
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "voting_power")
end

let register () =
  let open Services_registration in
  register0 S.list_delegate (fun ctxt q () ->
      Baker.list ctxt
      >>= fun bakers ->
      map_s
        (fun baker ->
          Baker.get_consensus_key ctxt baker
          >|=? fun consensus_key ->
          (baker, Signature.Public_key.hash consensus_key))
        bakers
      >>=? fun delegates ->
      match q with
      | {active = true; inactive = false} ->
          filter_map_s
            (fun (baker, pkh) ->
              Baker.deactivated ctxt baker
              >|=? function true -> None | false -> Some pkh)
            delegates
      | {active = false; inactive = true} ->
          filter_map_s
            (fun (baker, pkh) ->
              Baker.deactivated ctxt baker
              >|=? function false -> None | true -> Some pkh)
            delegates
      | _ ->
          return @@ List.map snd delegates) ;
  register1 S.info (fun ctxt pkh () () ->
      Baker.is_consensus_key ctxt pkh
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
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
          Vote.get_voting_power_free ctxt baker
          >|=? fun voting_power ->
          {
            balance;
            frozen_balance;
            frozen_balance_by_cycle;
            staking_balance;
            delegated_contracts;
            delegated_balance;
            deactivated;
            grace_period;
            voting_power;
          }) ;
  register1 S.balance (fun ctxt pkh () () ->
      Baker.is_consensus_key ctxt pkh
      >>=? function
      | None -> raise Not_found | Some baker -> Baker.full_balance ctxt baker) ;
  register1 S.frozen_balance (fun ctxt pkh () () ->
      Baker.is_consensus_key ctxt pkh
      >>=? function
      | None -> raise Not_found | Some baker -> Baker.frozen_balance ctxt baker) ;
  register1 S.frozen_balance_by_cycle (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
          Baker.frozen_balance_by_cycle ctxt baker >>= return) ;
  register1 S.staking_balance (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
          Baker.staking_balance ctxt baker) ;
  register1 S.delegated_contracts (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
          Baker.delegated_contracts ctxt baker >>= return) ;
  register1 S.delegated_balance (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
          Baker.delegated_balance ctxt baker) ;
  register1 S.deactivated (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None -> raise Not_found | Some baker -> Baker.deactivated ctxt baker) ;
  register1 S.grace_period (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None -> raise Not_found | Some baker -> Baker.grace_period ctxt baker) ;
  register1 S.voting_power (fun ctxt baker () () ->
      Baker.is_consensus_key ctxt baker
      >>=? function
      | None ->
          raise Not_found
      | Some baker ->
          Vote.get_voting_power_free ctxt baker)
