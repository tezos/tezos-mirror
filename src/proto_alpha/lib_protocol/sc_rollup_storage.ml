(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

type error +=
  | (* `Temporary *) Sc_rollup_already_staked
  | (* `Temporary *) Sc_rollup_disputed
  | (* `Temporary *) Sc_rollup_does_not_exist of Sc_rollup_repr.t
  | (* `Temporary *) Sc_rollup_no_conflict
  | (* `Temporary *) Sc_rollup_no_stakers
  | (* `Temporary *) Sc_rollup_not_staked
  | (* `Temporary *) Sc_rollup_not_staked_on_lcc
  | (* `Temporary *) Sc_rollup_parent_not_lcc
  | (* `Temporary *) Sc_rollup_remove_lcc
  | (* `Temporary *) Sc_rollup_staker_backtracked
  | (* `Temporary *) Sc_rollup_too_far_ahead
  | (* `Temporary *) Sc_rollup_too_recent
  | (* `Temporary *)
      Sc_rollup_unknown_commitment of
      Sc_rollup_repr.Commitment_hash.t
  | (* `Temporary *) Sc_rollup_bad_inbox_level

let () =
  let description = "Already staked." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_already_staked"
    ~title:"Already staked"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_already_staked -> Some () | _ -> None)
    (fun () -> Sc_rollup_already_staked) ;
  let description = "Attempted to cement a disputed commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_disputed"
    ~title:"Commitment disputed"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_disputed -> Some () | _ -> None)
    (fun () -> Sc_rollup_disputed) ;
  let description = "Attempted to use a rollup that has not been originated." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_does_not_exist"
    ~title:"Rollup does not exist"
    ~description
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Rollup %a does not exist" Sc_rollup_repr.pp x)
    Data_encoding.(obj1 (req "rollup" Sc_rollup_repr.encoding))
    (function Sc_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Sc_rollup_does_not_exist x) ;
  let description = "No conflict." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_conflict"
    ~title:"No conflict"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_conflict -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_conflict) ;
  let description = "No stakers." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_no_stakers"
    ~title:"No stakers"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_no_stakers -> Some () | _ -> None)
    (fun () -> Sc_rollup_no_stakers) ;
  let description = "Unknown staker." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_not_staked"
    ~title:"Unknown staker"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked) ;
  let description =
    "Attempted to withdraw while not staked on the last cemented commitment."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_not_staked_on_lcc"
    ~title:"Rollup not staked on LCC"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_not_staked_on_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_not_staked_on_lcc) ;
  let description = "Parent is not cemented." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_parent_not_lcc"
    ~title:"Parent not cemented"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_parent_not_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_parent_not_lcc) ;
  let description = "Can not remove a cemented commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_remove_lcc"
    ~title:"Can not remove cemented"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_remove_lcc -> Some () | _ -> None)
    (fun () -> Sc_rollup_remove_lcc) ;
  let description = "Staker backtracked." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_staker_backtracked"
    ~title:"Staker backtracked"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_staker_backtracked -> Some () | _ -> None)
    (fun () -> Sc_rollup_staker_backtracked) ;
  let description =
    "Commitment is too far ahead of the last cemented commitment."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_too_far_ahead"
    ~title:"Commitment too far ahead"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_too_far_ahead -> Some () | _ -> None)
    (fun () -> Sc_rollup_too_far_ahead) ;
  let description =
    "Attempted to cement a commitment before its refutation deadline."
  in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_too_recent"
    ~title:"Commitment too recent"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_too_recent -> Some () | _ -> None)
    (fun () -> Sc_rollup_too_recent) ;
  let description = "Unknown commitment." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_unknown_commitment"
    ~title:"Rollup does not exist"
    ~description
    ~pp:(fun ppf x ->
      Format.fprintf
        ppf
        "Commitment %a does not exist"
        Sc_rollup_repr.Commitment_hash.pp
        x)
    Data_encoding.(
      obj1 (req "commitment" Sc_rollup_repr.Commitment_hash.encoding))
    (function Sc_rollup_unknown_commitment x -> Some x | _ -> None)
    (fun x -> Sc_rollup_unknown_commitment x) ;
  let description = "Attempted to commit to a bad inbox level." in
  register_error_kind
    `Temporary
    ~id:"Sc_rollup_bad_inbox_level"
    ~title:"Committed too soon"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_bad_inbox_level -> Some () | _ -> None)
    (fun () -> Sc_rollup_bad_inbox_level) ;
  ()

(** To be removed once [Lwt_tzresult_syntax] is in the environment. *)
module Lwt_tzresult_syntax = struct
  let ( let* ) = ( >>=? )

  let return = return
end

module Store = Storage.Sc_rollup
module Commitment = Sc_rollup_repr.Commitment
module Commitment_hash = Sc_rollup_repr.Commitment_hash

let originate ctxt ~kind ~boot_sector =
  Raw_context.increment_origination_nonce ctxt >>?= fun (ctxt, nonce) ->
  Sc_rollup_repr.Address.from_nonce nonce >>?= fun address ->
  Storage.Sc_rollup.PVM_kind.add ctxt address kind >>= fun ctxt ->
  Storage.Sc_rollup.Initial_level.add
    ctxt
    address
    (Level_storage.current ctxt).level
  >>= fun ctxt ->
  Storage.Sc_rollup.Boot_sector.add ctxt address boot_sector >>= fun ctxt ->
  Storage.Sc_rollup.Inbox.init ctxt address Sc_rollup_inbox.empty
  >>=? fun (ctxt, size_diff) ->
  Store.Last_cemented_commitment.init ctxt address Commitment_hash.zero
  >>=? fun (ctxt, lcc_size_diff) ->
  Store.Staker_count.init ctxt address 0l >>=? fun (ctxt, stakers_size_diff) ->
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let boot_sector_size =
    Data_encoding.Binary.length
      Sc_rollup_repr.PVM.boot_sector_encoding
      boot_sector
  in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size
     + size_diff + lcc_size_diff + stakers_size_diff)
  in
  return (address, size, ctxt)

let kind ctxt address = Storage.Sc_rollup.PVM_kind.find ctxt address

let add_messages ctxt rollup messages =
  Storage.Sc_rollup.Inbox.get ctxt rollup >>=? fun (ctxt, inbox) ->
  let {Level_repr.level; _} = Raw_context.current_level ctxt in
  let inbox = Sc_rollup_inbox.add_messages messages level inbox in
  Storage.Sc_rollup.Inbox.update ctxt rollup inbox >>=? fun (ctxt, size) ->
  return (inbox, Z.of_int size, ctxt)

(** Try to consume n messages. *)
let consume_n_messages ctxt rollup n =
  let open Lwt_tzresult_syntax in
  let* (ctxt, inbox) = Storage.Sc_rollup.Inbox.get ctxt rollup in
  match Sc_rollup_inbox.consume_n_messages n inbox with
  | None -> return ctxt
  | Some inbox ->
      let* (ctxt, size) = Storage.Sc_rollup.Inbox.update ctxt rollup inbox in
      assert (Compare.Int.(size < 0)) ;
      return ctxt

let inbox ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Storage.Sc_rollup.Inbox.get ctxt rollup in
  return (res, ctxt)

let last_cemented_commitment ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Last_cemented_commitment.find ctxt rollup in
  match res with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some lcc -> return (lcc, ctxt)

let get_commitment ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Commitments.find (ctxt, rollup) commitment in
  match res with
  | None -> fail (Sc_rollup_unknown_commitment commitment)
  | Some commitment -> return (commitment, ctxt)

let get_predecessor ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* (commitment, ctxt) = get_commitment ctxt rollup node in
  return (commitment.predecessor, ctxt)

let find_staker ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some branch -> return (branch, ctxt)

let modify_staker_count ctxt rollup f =
  let open Lwt_tzresult_syntax in
  let* (ctxt, maybe_count) = Store.Staker_count.find ctxt rollup in
  let count = Option.value ~default:0l maybe_count in
  let* (ctxt, size_diff, _was_bound) =
    Store.Staker_count.add ctxt rollup (f count)
  in
  assert (Compare.Int.(size_diff = 0)) ;
  return ctxt

let get_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* (ctxt, maybe_staked_on_commitment) =
    Store.Commitment_stake_count.find (ctxt, rollup) node
  in
  return (Option.value ~default:0l maybe_staked_on_commitment, ctxt)

(** [set_commitment_added ctxt rollup node current] sets the commitment
    addition time of [node] to [current] iff the commitment time was
    not previously set, and leaves it unchanged otherwise.
 *)
let set_commitment_added ctxt rollup node new_value =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Commitment_added.find (ctxt, rollup) node in
  let new_value =
    match res with None -> new_value | Some old_value -> old_value
  in
  let* (ctxt, size_diff, _was_bound) =
    Store.Commitment_added.add (ctxt, rollup) node new_value
  in
  return (size_diff, ctxt)

let deallocate ctxt rollup node =
  let open Lwt_tzresult_syntax in
  if Commitment_hash.(node = zero) then return ctxt
  else
    let* (ctxt, _size_freed) =
      Store.Commitments.remove_existing (ctxt, rollup) node
    in
    let* (ctxt, _size_freed) =
      Store.Commitment_added.remove_existing (ctxt, rollup) node
    in
    let* (ctxt, _size_freed) =
      Store.Commitment_stake_count.remove_existing (ctxt, rollup) node
    in
    return ctxt

let modify_commitment_stake_count ctxt rollup node f =
  let open Lwt_tzresult_syntax in
  let* (count, ctxt) = get_commitment_stake_count ctxt rollup node in
  let new_count = f count in
  let* (ctxt, size_diff, _was_bound) =
    Store.Commitment_stake_count.add (ctxt, rollup) node new_count
  in
  return (new_count, size_diff, ctxt)

let increase_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* (_new_count, size_diff, ctxt) =
    modify_commitment_stake_count ctxt rollup node Int32.succ
  in
  return (size_diff, ctxt)

let decrease_commitment_stake_count ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* (new_count, _size_diff, ctxt) =
    modify_commitment_stake_count ctxt rollup node Int32.pred
  in
  if Compare.Int32.(new_count <= 0l) then deallocate ctxt rollup node
  else return ctxt

let deposit_stake ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  let* (ctxt, res) = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/2449
         We should lock stake here, and fail if there aren't enough funds.
      *)
      let* (ctxt, _size) = Store.Stakers.init (ctxt, rollup) staker lcc in
      let* ctxt = modify_staker_count ctxt rollup Int32.succ in
      return ctxt
  | Some _ -> fail Sc_rollup_already_staked

let withdraw_stake ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  let* (ctxt, res) = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some staked_on_commitment ->
      if Commitment_hash.(staked_on_commitment = lcc) then
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/2449
           We should refund stake here.
        *)
        let* (ctxt, _size_freed) =
          Store.Stakers.remove_existing (ctxt, rollup) staker
        in
        let* ctxt = modify_staker_count ctxt rollup Int32.pred in
        modify_staker_count ctxt rollup Int32.pred
      else fail Sc_rollup_not_staked_on_lcc

(* TODO https://gitlab.com/tezos/tezos/-/issues/2548
   These should be protocol constants. See issue for invariant that should be
   tested for. *)
let sc_rollup_max_lookahead = 30_000l

let sc_rollup_commitment_frequency = 20

(* 76 for Commitments entry + 4 for Commitment_stake_count entry + 4 for Commitment_added entry *)
let sc_rollup_commitment_storage_size_in_bytes = 84

let assert_commitment_not_too_far_ahead ctxt rollup lcc commitment =
  let open Lwt_tzresult_syntax in
  let* (ctxt, min_level) =
    if Commitment_hash.(lcc = zero) then
      let* level = Store.Initial_level.get ctxt rollup in
      return (ctxt, level)
    else
      let* (lcc, ctxt) = get_commitment ctxt rollup lcc in
      return (ctxt, Commitment.(lcc.inbox_level))
  in
  let max_level = Commitment.(commitment.inbox_level) in
  if
    Compare.Int32.(
      sc_rollup_max_lookahead < Raw_level_repr.diff max_level min_level)
  then fail Sc_rollup_too_far_ahead
  else return ctxt

let assert_commitment_frequency ctxt rollup commitment check =
  let open Lwt_tzresult_syntax in
  let pred = Commitment.(commitment.predecessor) in
  let* (ctxt, pred_level) =
    if Commitment_hash.(pred = zero) then
      let* level = Store.Initial_level.get ctxt rollup in
      return (ctxt, level)
    else
      let* (pred, ctxt) = get_commitment ctxt rollup commitment.predecessor in
      return (ctxt, Commitment.(pred.inbox_level))
  in
  if
    Raw_level_repr.(
      check
        commitment.inbox_level
        (add pred_level sc_rollup_commitment_frequency))
  then return ctxt
  else fail Sc_rollup_bad_inbox_level

(** Check invariants on [inbox_level], enforcing overallocation of storage and
    regularity of block prorudction.

     The constants used by [assert_refine_conditions_met] must be chosen such
     that the maximum cost of storage allocated by each staker at most the size
     of their deposit.
 *)
let assert_refine_conditions_met ctxt rollup lcc commitment =
  let open Lwt_tzresult_syntax in
  let* ctxt = assert_commitment_not_too_far_ahead ctxt rollup lcc commitment in
  (* We want to check the following inequalities on [commitment.inbox_level],
     [commitment.predecessor.inbox_level] and the constant [sc_rollup_commitment_frequency].

     - Greater-than-or-equal (>=), to ensure inbox_levels are monotonically
     increasing.  along each branch of commitments. Together with
     [assert_commitment_not_too_far_ahead] this is sufficient to limit the
     depth of the commitment tree, which is also the number commitments stored
     per staker. This constraint must be enforced at submission time.

     - Equality (=), so that that L2 blocks are produced at a regular rate.  This
     ensures that there is only ever one branch of correct commitments,
     simplifying refutation logic. This could also be enforced at refutation time
     rather than submission time, but doing it here works too.

     Because [a >= b && a = b] is equivalent to [a = b], we can the latter as
     an optimization.
  *)
  let check = Raw_level_repr.( = ) in
  assert_commitment_frequency ctxt rollup commitment check

let refine_stake ctxt rollup level staker commitment =
  let open Lwt_tzresult_syntax in
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  let* (staked_on, ctxt) = find_staker ctxt rollup staker in
  let* ctxt = assert_refine_conditions_met ctxt rollup lcc commitment in
  let new_hash = Commitment.hash commitment in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2559
     Add a test checking that L2 nodes can catch up after going offline. *)
  let rec go node ctxt =
    (* WARNING: Do NOT reorder this sequence of ifs.
       we must check for staked_on before LCC, since refining
       from the LCC to another commit is a valid operation. *)
    if Commitment_hash.(node = staked_on) then (
      (* Previously staked commit found:
         Insert new commitment if not existing *)
      let* (ctxt, commitment_size_diff, _was_bound) =
        Store.Commitments.add (ctxt, rollup) new_hash commitment
      in
      let* (commitment_added_size_diff, ctxt) =
        set_commitment_added ctxt rollup new_hash level
      in
      let* (ctxt, staker_count_diff) =
        Store.Stakers.update (ctxt, rollup) staker new_hash
      in
      let* (stake_count_size_diff, ctxt) =
        increase_commitment_stake_count ctxt rollup new_hash
      in
      let size_diff =
        commitment_size_diff + commitment_added_size_diff
        + stake_count_size_diff + staker_count_diff
      in
      (* First submission adds [sc_rollup_commitment_storage_size_in_bytes] to storage.
         Later submission adds 0 due to content-addressing. *)
      assert (
        Compare.Int.(
          size_diff = 0
          || size_diff = sc_rollup_commitment_storage_size_in_bytes)) ;
      return (new_hash, ctxt) (* See WARNING above. *))
    else if Commitment_hash.(node = lcc) then
      (* We reached the LCC, but [staker] is not staked directly on it.
         Thus, we backtracked. Note that everyone is staked indirectly on
         the LCC. *)
      fail Sc_rollup_staker_backtracked
    else
      let* (pred, ctxt) = get_predecessor ctxt rollup node in
      let* (_size, ctxt) = increase_commitment_stake_count ctxt rollup node in
      (go [@ocaml.tailcall]) pred ctxt
  in
  go Commitment.(commitment.predecessor) ctxt

let cement_commitment ctxt rollup level new_lcc =
  let open Lwt_tzresult_syntax in
  let refutation_deadline_blocks =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  (* get is safe, as Staker_count is initialized on origination *)
  let* (ctxt, total_staker_count) = Store.Staker_count.get ctxt rollup in
  if Compare.Int32.(total_staker_count <= 0l) then fail Sc_rollup_no_stakers
  else
    let* (old_lcc, ctxt) = last_cemented_commitment ctxt rollup in
    let* (new_lcc_commitment, ctxt) = get_commitment ctxt rollup new_lcc in
    let* (ctxt, new_lcc_added) =
      Store.Commitment_added.get (ctxt, rollup) new_lcc
    in
    if Commitment_hash.(new_lcc_commitment.predecessor <> old_lcc) then
      fail Sc_rollup_parent_not_lcc
    else
      let* (new_lcc_stake_count, ctxt) =
        get_commitment_stake_count ctxt rollup new_lcc
      in
      if Compare.Int32.(total_staker_count <> new_lcc_stake_count) then
        fail Sc_rollup_disputed
      else if
        Raw_level_repr.(level < add new_lcc_added refutation_deadline_blocks)
      then fail Sc_rollup_too_recent
      else
        (* update LCC *)
        let* (ctxt, lcc_size_diff) =
          Store.Last_cemented_commitment.update ctxt rollup new_lcc
        in
        assert (Compare.Int.(lcc_size_diff = 0)) ;
        (* At this point we know all stakers are implicitly staked
           on the new LCC, and no one is directly staked on the old LCC. We
           can safely deallocate the old LCC.
        *)
        let* ctxt = deallocate ctxt rollup old_lcc in
        let* ctxt =
          consume_n_messages
            ctxt
            rollup
            (Int32.to_int
            @@ Sc_rollup_repr.Number_of_messages.to_int32
                 new_lcc_commitment.number_of_messages)
        in
        let* (ctxt, _size) =
          Store.Commitments.remove_existing (ctxt, rollup) new_lcc
        in
        return ctxt

module Successor_map = Commitment_hash.Map

type conflict_point = Commitment_hash.t * Commitment_hash.t

let get_conflict_point ctxt rollup staker1 staker2 =
  let open Lwt_tzresult_syntax in
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  let* (staker1_branch, ctxt) = find_staker ctxt rollup staker1 in
  let* (staker2_branch, ctxt) = find_staker ctxt rollup staker2 in
  (* Build a map from commitments on the staker1 branch to their direct
     successor on this branch. *)
  let* (staker1_succ_map, ctxt) =
    let rec go node prev_map ctxt =
      if Commitment_hash.(node = lcc) then return (prev_map, ctxt)
      else
        let* (pred, ctxt) = get_predecessor ctxt rollup node in
        let new_map = Successor_map.add pred node prev_map in
        (go [@ocaml.tailcall]) pred new_map ctxt
    in
    go staker1_branch Successor_map.empty ctxt
  in
  (* Traverse from staker2 towards LCC. *)
  if Successor_map.mem staker2_branch staker1_succ_map then
    (* The staker1 branch contains the tip of the staker2 branch.
       Commitments are perfect agreement, or in partial agreement with staker1
       ahead. *)
    fail Sc_rollup_no_conflict
  else
    let rec go node ctxt =
      if Commitment_hash.(node = staker1_branch) then
        (* The staker2 branch contains the tip of the staker1 branch.
           The commitments are in partial agreement with staker2 ahead. *)
        fail Sc_rollup_no_conflict
      else
        let right = node in
        let* (pred, ctxt) = get_predecessor ctxt rollup node in
        match Successor_map.find pred staker1_succ_map with
        | None -> (go [@ocaml.tailcall]) pred ctxt
        | Some left -> return ((left, right), ctxt)
    in
    go staker2_branch ctxt

let remove_staker ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  let* (ctxt, res) = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None -> fail Sc_rollup_not_staked
  | Some staked_on ->
      if Commitment_hash.(staked_on = lcc) then fail Sc_rollup_remove_lcc
      else
        let* (ctxt, _size_diff) =
          Store.Stakers.remove_existing (ctxt, rollup) staker
        in
        let* ctxt = modify_staker_count ctxt rollup Int32.pred in
        let rec go node ctxt =
          if Commitment_hash.(node = lcc) then return ctxt
          else
            let* (pred, ctxt) = get_predecessor ctxt rollup node in
            let* ctxt = decrease_commitment_stake_count ctxt rollup node in
            (go [@ocaml.tailcall]) pred ctxt
        in
        go staked_on ctxt

let list ctxt = Storage.Sc_rollup.PVM_kind.keys ctxt >|= Result.return
