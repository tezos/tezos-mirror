#import "common/utils/converters.mligo" "Converters"
#import "common/validation.mligo" "Validation"

module DelegateContract = struct
type delegate_info = bool * (address set) option
type storage = (address, (address, delegate_info) map) big_map

[@entry]
let set_delegate
    (delegate, is_delegate, opt_addresses : address * bool * (address set) option)
    (storage : storage) 
    : operation list * storage =
  let _ = Validation.assert_no_tez_in_transaction () in
  let sender = Tezos.get_sender () in
  let voting_power =
      Tezos.voting_power (Converters.address_to_key_hash sender) in
  let _ = Validation.assert_voting_power_positive voting_power in
  let delegate_info : delegate_info option =
    match opt_addresses, is_delegate with
      | None, false -> None
      | None, true -> Some (is_delegate, None)
      | Some addresses, is_delegate -> Some (is_delegate, Some addresses)
  in
  let updated_delegates =
    let delegates : (address, delegate_info) map =
      match Big_map.find_opt delegate storage with
        | None -> Map.empty
        | Some s -> s
    in
      let pre_updated_delegates = Map.update sender delegate_info delegates in
        if Map.size pre_updated_delegates = 0n then None else Some pre_updated_delegates
    in
    let updated_storage =
        Big_map.update
                delegate
                updated_delegates
                storage in
    ([], updated_storage)

[@view]
let is_delegate
  (delegate, voter, contract : address * address * address option)
  (storage : storage)
  : bool =
  match Big_map.find_opt delegate storage with
  | None -> false
  | Some delegate_map ->
        ( match contract, Map.find_opt voter delegate_map with
          | _ , None  -> False
          | None, Some _ -> True
          | Some _, Some (_,None) -> True
          | Some c, Some (True,Some whitelist) -> Set.mem c whitelist
          | Some c, Some (False,Some blacklist) -> not Set.mem c blacklist)

[@view]
let list_delegates
    (delegate, contract : address* address option)
    (storage : storage)
    : address list =
  match contract, Big_map.find_opt delegate storage with
  | _,None -> []
  | None, Some delegate_map ->
      Map.fold (fun ((l,(d,_)):address list * (address * delegate_info)) :address list -> d::l) delegate_map []
  | Some c, Some delegate_map ->
      Map.fold (fun ((l,(d,(b,wbl))): address list * (address * delegate_info)) :address list ->
         match b,wbl with
         | _, None -> d::l
         | True, Some wl -> if Set.mem c wl then d::l else l
         | False, Some bl -> if Set.mem c bl then l else d::l
      ) delegate_map []      
end
