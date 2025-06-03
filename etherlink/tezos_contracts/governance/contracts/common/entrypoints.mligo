#import "storage.mligo" "Storage"
#import "errors.mligo" "Errors"
#import "voting.mligo" "Voting"
#import "rollup.mligo" "Rollup"
#import "events.mligo" "Events"
#import "validation.mligo" "Validation"
#import "utils/converters.mligo" "Converters"


let new_proposal
        (type pt)
        (payload : pt)
        (storage : pt Storage.t)
        : operation list * pt Storage.t =
    let _ = Validation.assert_no_tez_in_transaction () in
    let { voting_context; finished_voting; last_winner } = Voting.get_voting_state storage in
    let proposal_period = Voting.get_proposal_period voting_context in
    let potential_proposers = Voting.get_voters storage.config.delegation_contract in
    let (proposers, total_voting_power, real_voting_power) =
       Voting.filter_proposers potential_proposers proposal_period storage.config in
    let _ = Validation.assert_voting_power_positive total_voting_power in
    let _ = Assert.Error.assert (Set.size proposers > 0n) Errors.upvoting_limit_exceeded in
    let updated_period = Voting.add_new_proposal_and_upvote payload proposers real_voting_power proposal_period in
    let operations = match finished_voting with
        | Some event_payload -> [Events.create_voting_finished_event_operation event_payload]
        | None -> [] in
    let updated_storage = {
        storage with
        voting_context = Some { voting_context with period = updated_period; };
        last_winner = last_winner;
    } in
    operations, updated_storage


let upvote_proposal
        (type pt)
        (payload : pt)
        (storage : pt Storage.t)
        : operation list * pt Storage.t =
    let _ = Validation.assert_no_tez_in_transaction () in
    let { voting_context; finished_voting; last_winner } = Voting.get_voting_state storage in
    let proposal_period = Voting.get_proposal_period voting_context in
    let potential_upvoters = Voting.get_voters storage.config.delegation_contract in
    let (upvoters, total_voting_power, protocol_already_upvoted_addresses) =
          Voting.filter_upvoters payload potential_upvoters proposal_period storage.config in
    let _ = Validation.assert_voting_power_positive total_voting_power in
    let _ = Assert.Error.assert (Map.size upvoters > 0n) (if protocol_already_upvoted_addresses then Errors.proposal_already_upvoted else Errors.upvoting_limit_exceeded) in
    let new_voting_context =
            (Map.fold
            (fun (period,(upvoter,voting_power)) ->
                let proposal_period = Voting.get_proposal_period period in
                let updated_period = Voting.upvote_proposal payload upvoter voting_power proposal_period storage.config in
                { voting_context with period = updated_period })
           upvoters
           voting_context)
          in
    let operations = match finished_voting with
        | Some event_payload -> [Events.create_voting_finished_event_operation event_payload]
        | None -> [] in
    let updated_storage = {
        storage with
        voting_context = Some new_voting_context;
        last_winner = last_winner;
    } in
    operations, updated_storage


let vote
        (type pt)
        (vote : string)
        (storage : pt Storage.t)
        : operation list * pt Storage.t =
    let _ = Validation.assert_no_tez_in_transaction () in
    let voting_state = Voting.get_voting_state storage in
    let voting_context = voting_state.voting_context in
    let potential_voters = Voting.get_voters storage.config.delegation_contract in
    let (voters, total_voting_power) = Voting.filter_voters potential_voters voting_context in
    let _ = Validation.assert_voting_power_positive total_voting_power in
    let _ = Assert.Error.assert (Map.size voters > 0n) Errors.promotion_already_voted in
    let new_voting_context =
            (Map.fold
            (fun (period,(voter,voting_power)) ->
                let promotion_period = Voting.get_promotion_period period in
                let updated_period =
                  Voting.vote_promotion vote voter voting_power promotion_period
                in
           { voting_context with period = updated_period })
           voters
           voting_context)
          in
    let updated_storage = {
        storage with
        voting_context = Some new_voting_context;
    } in
    [], updated_storage


let trigger_rollup_upgrade
        (type pt)
        (rollup_address : address)
        (storage : pt Storage.t)
        (pack_payload : pt -> bytes)
        : operation list * pt Storage.t =
    let _ = Validation.assert_no_tez_in_transaction () in
    let { voting_context; finished_voting; last_winner = last_winner_opt } = Voting.get_voting_state storage in
    let last_winner = Option.value_with_error Errors.last_winner_not_found last_winner_opt  in
    let last_winner_trigger_history = last_winner.trigger_history in
    let _ = Assert.Error.assert (not Big_map.mem rollup_address last_winner.trigger_history) Errors.upgrade_for_address_already_triggered in
    let rollup_entry = Rollup.get_entry rollup_address in
    let upgrade_params = Rollup.get_upgrade_params (pack_payload last_winner.payload) in
    let upgrade_operation = Tezos.Next.Operation.transaction upgrade_params 0tez rollup_entry in
    let operations = match finished_voting with
        | Some event_payload -> [Events.create_voting_finished_event_operation event_payload]
        | None -> [] in
    let operations = upgrade_operation :: operations in
    let updated_storage = {
        storage with
        voting_context = Some voting_context;
        last_winner = Some {
            last_winner with
            trigger_history = Big_map.add rollup_address unit last_winner_trigger_history
        }
    } in
    operations, updated_storage
