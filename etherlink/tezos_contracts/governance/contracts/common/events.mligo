#import "storage.mligo" "Storage"

type period_type_t = Proposal | Promotion

type 'pt voting_finished_event_payload_t = {
    finished_at_period_index : nat;
    finished_at_period_type : period_type_t;
    winner_proposal_payload : 'pt option;
}

[@inline]
let create_voting_finished_event
        (type pt)
        (period_index : nat)
        (period_type : period_type_t)
        (winner_proposal_payload : pt option)
        : pt voting_finished_event_payload_t = 
    {
        finished_at_period_index = period_index + 1n;
        finished_at_period_type = period_type;
        winner_proposal_payload = winner_proposal_payload;
    }


[@inline]
let create_voting_finished_event_operation
        (type pt)
        (event_payload : pt voting_finished_event_payload_t)
        : operation =
    Tezos.emit "%voting_finished" event_payload
