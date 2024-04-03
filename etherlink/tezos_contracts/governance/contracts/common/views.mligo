#import "storage.mligo" "Storage"
#import "events.mligo" "Events"
#import "voting.mligo" "Voting"

type 'pt voting_state_t = {
    period_index : nat;
    period_type : Events.period_type_t;
    remaining_blocks : nat;
    finished_voting : 'pt Events.voting_finished_event_payload_t option;
}


let get_period_type
        (type pt)
        (period : pt Storage.period_t)
        : Events.period_type_t =
    match period with
        | Proposal _ -> Proposal
        | Promotion _ -> Promotion


[@inline]
let get_voting_state
        (type pt)
        (storage : pt Storage.t)
        : pt voting_state_t = 
    let voting_state = Voting.get_voting_state storage in
    let voting_context = voting_state.voting_context in
    {
        period_index = voting_context.period_index;
        period_type = get_period_type voting_context.period;
        remaining_blocks = Voting.get_current_period_remaining_blocks storage.config;
        finished_voting = voting_state.finished_voting
    }