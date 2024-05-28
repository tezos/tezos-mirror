#import "common/storage.mligo" "Storage"
#import "common/errors.mligo" "Errors"
#import "common/voting.mligo" "Voting"
#import "common/rollup.mligo" "Rollup"
#import "common/entrypoints.mligo" "Entrypoints"
#import "common/views.mligo" "Views"

module KernelGovernance = struct

    type payload_t = bytes 
    type storage_t = payload_t Storage.t
    type return_t = operation list * storage_t


    [@entry] 
    let new_proposal 
            (kernel_root_hash : payload_t)
            (storage : storage_t) 
            : return_t = 
        let _ = Rollup.assert_kernel_root_hash_has_correct_length kernel_root_hash in
        Entrypoints.new_proposal kernel_root_hash storage
  

    [@entry]
    let upvote_proposal 
            (kernel_root_hash : payload_t)
            (storage : storage_t) 
            : return_t = 
        Entrypoints.upvote_proposal kernel_root_hash storage
  

    [@entry]
    let vote 
            (vote : string) 
            (storage : storage_t) 
            : return_t =
        Entrypoints.vote vote storage
  

    [@entry]
    let trigger_kernel_upgrade
            (rollup_address : address)
            (storage : storage_t) 
            : return_t =
        let pack_payload = fun 
                (payload : payload_t) 
                : bytes -> 
            let activation_timestamp = Rollup.get_activation_timestamp storage.config.adoption_period_sec in
            Rollup.get_kernel_upgrade_payload payload activation_timestamp in
        Entrypoints.trigger_rollup_upgrade rollup_address storage pack_payload


    [@view] 
    let get_voting_state
            (_ : unit) 
            (storage : storage_t) 
            : payload_t Views.voting_state_t = 
        Views.get_voting_state storage
end