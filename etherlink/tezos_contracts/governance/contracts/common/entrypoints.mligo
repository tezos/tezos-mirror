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
    let { voting_context; finished_voting; last_winner } = Voting.get_voting_state storage in
    let proposer = Converters.address_to_key_hash (Tezos.get_sender ()) in
    let voting_power = Tezos.voting_power proposer in
    let _ = Validation.assert_no_tez_in_transaction () in
    let _ = Validation.assert_voting_power_positive voting_power in
    let proposal_period = Voting.get_proposal_period voting_context in
    let updated_period = Voting.add_new_proposal_and_upvote payload proposer voting_power proposal_period storage.config in
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
    let { voting_context; finished_voting; last_winner } = Voting.get_voting_state storage in
    let upvoter = Converters.address_to_key_hash (Tezos.get_sender ()) in
    let voting_power = Tezos.voting_power upvoter in
    let _ = Validation.assert_no_tez_in_transaction () in
    let _ = Validation.assert_voting_power_positive voting_power in
    let proposal_period = Voting.get_proposal_period voting_context in
    let updated_period = Voting.upvote_proposal payload upvoter voting_power proposal_period storage.config in
    let operations = match finished_voting with
        | Some event_payload -> [Events.create_voting_finished_event_operation event_payload]
        | None -> [] in
    let updated_storage = { 
        storage with 
        voting_context = Some { voting_context with period = updated_period };
        last_winner = last_winner; 
    } in
    operations, updated_storage


let vote
        (type pt)
        (vote : string)
        (storage : pt Storage.t)
        : operation list * pt Storage.t =
    let voting_state = Voting.get_voting_state storage in
    let voting_context = voting_state.voting_context in
    let voter = Converters.address_to_key_hash (Tezos.get_sender ()) in
    let voting_power = Tezos.voting_power voter in
    let _ = Validation.assert_no_tez_in_transaction () in
    let _ = Validation.assert_voting_power_positive voting_power in
    let promotion_period = Voting.get_promotion_period voting_context in
    let updated_period = Voting.vote_promotion vote voter voting_power promotion_period in
    let updated_storage = { 
        storage with 
        voting_context = Some { voting_context with period = updated_period };
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
    let _ = assert_with_error (not Big_map.mem rollup_address last_winner.trigger_history) Errors.upgrade_for_address_already_triggered in
    let rollup_entry = Rollup.get_entry rollup_address in
    let upgrade_params = Rollup.get_upgrade_params (pack_payload last_winner.payload) in
    let upgrade_operation = Tezos.transaction upgrade_params 0tez rollup_entry in
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
