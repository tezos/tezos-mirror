#import "common/utils/converters.mligo" "Converters"
#import "common/validation.mligo" "Validation"

module DelegateContract = struct
type voter_info = bool * (address set) option
type storage = (address, (key_hash, voter_info) map) big_map

[@entry]
let set_voting_key
    (voting_key, is_voting_key, opt_addresses : address * bool * (address set) option)
    (storage : storage) 
    : operation list * storage =
  let _ = Validation.assert_no_tez_in_transaction () in
  let sender = Converters.address_to_key_hash (Tezos.get_sender ()) in
  let voting_power = Tezos.voting_power sender in
  let _ = Validation.assert_voting_power_positive voting_power in
  let voter_info : voter_info option =
    match opt_addresses, is_voting_key with
      | None, false -> None
      | opt_addresses, is_voting_key -> Some (is_voting_key, opt_addresses)
  in
  let updated_voting_keys =
    let voting_keys : (key_hash, voter_info) map =
      match Big_map.find_opt voting_key storage with
        | None -> Map.empty
        | Some s -> s
    in
      let pre_updated_voting_keys = Map.update sender voter_info voting_keys in
        if Map.size pre_updated_voting_keys = 0n then None else Some pre_updated_voting_keys
    in
    let updated_storage =
        Big_map.update
                voting_key
                updated_voting_keys
                storage in
    ([], updated_storage)

[@view]
let is_voting_key_of
  (voting_key, baker, contract : address * key_hash * address option)
  (storage : storage)
  : bool =
  match Big_map.find_opt voting_key storage with
  | None -> false
  | Some voting_key_map ->
        ( match contract, Map.find_opt baker voting_key_map with
          | _ , None  -> false
          | None, Some _ -> true
          | Some _, Some (_, None) -> true
          | Some c, Some (true, Some whitelist) -> Set.mem c whitelist
          | Some c, Some (false, Some blacklist) -> not Set.mem c blacklist)

[@view]
let list_voters
    (voter, contract : address * address option)
    (storage : storage)
    : key_hash list =
  match contract, Big_map.find_opt voter storage with
  | _, None -> []
  | None, Some voter_map ->
      Map.fold (fun ((l,(d,_)) : key_hash list * (key_hash * voter_info)) : key_hash list -> d::l) voter_map []
  | Some c, Some voter_map ->
      Map.fold (fun ((l,(d,(b,wbl))) : key_hash list * (key_hash * voter_info)) : key_hash list ->
         match b, wbl with
         | _, None -> d::l
         | true, Some wl -> if Set.mem c wl then d::l else l
         | false, Some bl -> if Set.mem c bl then l else d::l
      ) voter_map []
end
