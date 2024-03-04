(*
    NOTE:
    started_at_level and period_length values should be chosen carefully 
    to be sure that the contract governance periods 
    never cross the boundaries of the tezos protocol governance periods. 
    This ensures the immutability of voting power throughout the entire voting period 
*)
type config_t = {
    (* 
        Used to align voting periods with protocol governance periods. 
        Should be the start level of the current protocol governance period 
    *)
    started_at_level : nat;

    (* 
        The duration of the of proposal and promotion periods represented in blocks. 
        period_length = tezos_governance_period_length / N, where N is integer divisor (factor)
    *)
    period_length : nat;

    (* 
        The duration of the l2 adoption period counted in seconds. 
        Used to generate an upgrade payload with activation timestamp 
        on trigger_upgrade entrypoint call 
    *)
    adoption_period_sec : nat;

    (* Number of proposals that an account may upvote and submit *)
    upvoting_limit : nat;               

    (* 
        The scale for proposal_quorum, promotion_quorum and promotion_supermajority params. 
        For example if config.scale = 100 and config.proposal_quorum = 80 
        then proposal_quorum_% == 80 / 100 == .80 == 80%
    *)
    scale : nat;       

    (* 
        Minimum ratio of all the cumulated stake of a proposal upvotes to the total stake 
        to advance the proposal to promotion period 
    *)
    proposal_quorum : nat;     

    (* 
        Minimum ratio of all the cumulated stake of cast ballots (yea, nay, and pass ballots) 
        to the total stake to consider the proposal as a voting winner 
    *)
    promotion_quorum : nat;    

    (* 
        Minimum ratio of cumulated stake of Yea ballots to the cumulated stake 
        of Yea and Nay ballots to consider the proposal as a voting winner
    *)
    promotion_supermajority : nat;      
}

type proposal_t = {
    proposer : key_hash;
    upvotes_voting_power : nat;
}

(* 'pt - payload type. The value that bakers vote for *)
type 'pt proposals_t = ('pt, proposal_t) big_map

type upvoters_upvotes_count_t = (key_hash, nat) big_map

type 'pt upvoters_proposals_t = (key_hash * 'pt, unit) big_map

type 'pt proposal_period_t = {
    upvoters_upvotes_count : upvoters_upvotes_count_t;
    upvoters_proposals : 'pt upvoters_proposals_t;
    proposals : 'pt proposals_t;
    max_upvotes_voting_power : nat option;
    winner_candidate : 'pt option;
    total_voting_power : nat;
}

type 'pt promotion_period_t = {
    voters : (key_hash, string) big_map;
    yea_voting_power : nat;
    nay_voting_power : nat;
    pass_voting_power : nat;
    total_voting_power : nat;
    winner_candidate : 'pt;
}

type 'pt period_t = 
    | Proposal of 'pt proposal_period_t 
    | Promotion of 'pt promotion_period_t

type 'pt voting_context_t = {
    period_index : nat;
    period : 'pt period_t;
}

type 'pt voting_winner_t = {
    payload : 'pt;
    trigger_history : (address, unit) big_map;
}

type 'pt t = {
    config : config_t;
    voting_context : ('pt voting_context_t) option;
    last_winner : ('pt voting_winner_t) option;
    metadata : (string, bytes) big_map;
}
