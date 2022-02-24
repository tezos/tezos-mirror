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
  let level = Raw_context.current_level ctxt in
  Sc_rollup_repr.Address.from_nonce nonce >>?= fun address ->
  Storage.Sc_rollup.PVM_kind.add ctxt address kind >>= fun ctxt ->
  Storage.Sc_rollup.Initial_level.add
    ctxt
    address
    (Level_storage.current ctxt).level
  >>= fun ctxt ->
  Storage.Sc_rollup.Boot_sector.add ctxt address boot_sector >>= fun ctxt ->
  let inbox = Sc_rollup_inbox_repr.empty address level.level in
  Storage.Sc_rollup.Inbox.init ctxt address inbox >>=? fun (ctxt, size_diff) ->
  Store.Last_cemented_commitment.init ctxt address Commitment_hash.zero
  >>=? fun (ctxt, lcc_size_diff) ->
  Store.Staker_count.init ctxt address 0l >>=? fun (ctxt, stakers_size_diff) ->
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let boot_sector_size =
    Data_encoding.Binary.length Data_encoding.string boot_sector
  in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size
     + size_diff + lcc_size_diff + stakers_size_diff)
  in
  return (address, size, ctxt)

let kind ctxt address = Storage.Sc_rollup.PVM_kind.find ctxt address

let last_cemented_commitment ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Last_cemented_commitment.find ctxt rollup in
  match res with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some lcc -> return (lcc, ctxt)

(** Try to consume n messages. *)
let consume_n_messages ctxt rollup n =
  let open Lwt_tzresult_syntax in
  let* (ctxt, inbox) = Storage.Sc_rollup.Inbox.get ctxt rollup in
  Sc_rollup_inbox_repr.consume_n_messages n inbox >>?= function
  | None -> return ctxt
  | Some inbox ->
      let* (ctxt, size) = Storage.Sc_rollup.Inbox.update ctxt rollup inbox in
      assert (Compare.Int.(size <= 0)) ;
      return ctxt

let inbox ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Storage.Sc_rollup.Inbox.find ctxt rollup in
  match res with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some inbox -> return (inbox, ctxt)

let add_messages ctxt rollup messages =
  let open Raw_context in
  Storage.Sc_rollup.Inbox.get ctxt rollup >>=? fun (ctxt, inbox) ->
  Sc_rollup_in_memory_inbox.current_messages ctxt rollup
  |> fun current_messages ->
  let {Level_repr.level; _} = Raw_context.current_level ctxt in
  (*
      Notice that the protocol is forgetful: it throws away the inbox
      history. On the contrary, the history is stored by the rollup
      node to produce inclusion proofs when needed.
  *)
  Sc_rollup_inbox_repr.(
    add_messages history_at_genesis inbox level messages current_messages)
  >>=? fun (current_messages, _, inbox) ->
  Sc_rollup_in_memory_inbox.set_current_messages ctxt rollup current_messages
  |> fun ctxt ->
  Storage.Sc_rollup.Inbox.update ctxt rollup inbox >>=? fun (ctxt, size) ->
  return (inbox, Z.of_int size, ctxt)

(* This function is called in other functions in the module only after they have
   checked for the existence of the rollup, and therefore it is not necessary
   for it to check for the existence of the rollup again. It is not directly
   exposed by the module. Instead, a different public function [get_commitment]
   is provided, which checks for the existence of [rollup] before calling
   [get_commitment_internal]. *)
let get_commitment_internal ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Commitments.find (ctxt, rollup) commitment in
  match res with
  | None -> fail (Sc_rollup_unknown_commitment commitment)
  | Some commitment -> return (commitment, ctxt)

let get_commitment ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  (* Assert that a last cemented commitment exists. *)
  let* (_lcc, ctxt) = last_cemented_commitment ctxt rollup in
  get_commitment_internal ctxt rollup commitment

let get_predecessor ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* (commitment, ctxt) = get_commitment_internal ctxt rollup node in
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

let sc_rollup_commitment_frequency =
  (* Think twice before changing this. And then avoid doing it. *)
  20

(* 76 for Commitments entry + 4 for Commitment_stake_count entry + 4 for Commitment_added entry *)
let sc_rollup_commitment_storage_size_in_bytes = 84

let assert_commitment_not_too_far_ahead ctxt rollup lcc commitment =
  let open Lwt_tzresult_syntax in
  let* (ctxt, min_level) =
    if Commitment_hash.(lcc = zero) then
      let* level = Store.Initial_level.get ctxt rollup in
      return (ctxt, level)
    else
      let* (lcc, ctxt) = get_commitment_internal ctxt rollup lcc in
      return (ctxt, Commitment.(lcc.inbox_level))
  in
  let max_level = Commitment.(commitment.inbox_level) in
  if
    Compare.Int32.(
      sc_rollup_max_lookahead < Raw_level_repr.diff max_level min_level)
  then fail Sc_rollup_too_far_ahead
  else return ctxt

(** Enfore that a commitment's inbox level increases by an exact fixed amount over its predecessor.
    This property is used in several places - not obeying it causes severe breakage.
*)
let assert_commitment_frequency ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  let pred = Commitment.(commitment.predecessor) in
  let* (ctxt, pred_level) =
    if Commitment_hash.(pred = zero) then
      let* level = Store.Initial_level.get ctxt rollup in
      return (ctxt, level)
    else
      let* (pred, ctxt) =
        get_commitment_internal ctxt rollup commitment.predecessor
      in
      return (ctxt, Commitment.(pred.inbox_level))
  in
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
  if
    Raw_level_repr.(
      commitment.inbox_level = add pred_level sc_rollup_commitment_frequency)
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
  assert_commitment_frequency ctxt rollup commitment

let refine_stake ctxt rollup staker commitment =
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
      let level = (Raw_context.current_level ctxt).level in
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

let publish_commitment ctxt rollup staker commitment =
  let open Lwt_tzresult_syntax in
  let* (ctxt, res) = Store.Stakers.find (ctxt, rollup) staker in
  match res with
  | None ->
      let* ctxt = deposit_stake ctxt rollup staker in
      refine_stake ctxt rollup staker commitment
  | Some _ -> refine_stake ctxt rollup staker commitment

let cement_commitment ctxt rollup new_lcc =
  let open Lwt_tzresult_syntax in
  let refutation_deadline_blocks =
    Constants_storage.sc_rollup_challenge_window_in_blocks ctxt
  in
  (* Calling [last_final_commitment] first to trigger failure in case of
     non-existing rollup. *)
  let* (old_lcc, ctxt) = last_cemented_commitment ctxt rollup in
  (* Get is safe, as [Stakers_size] is initialized on origination. *)
  let* (ctxt, total_staker_count) = Store.Staker_count.get ctxt rollup in
  if Compare.Int32.(total_staker_count <= 0l) then fail Sc_rollup_no_stakers
  else
    let* (new_lcc_commitment, ctxt) =
      get_commitment_internal ctxt rollup new_lcc
    in
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
        let level = (Raw_context.current_level ctxt).level in
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
        consume_n_messages
          ctxt
          rollup
          (Int32.to_int
          @@ Sc_rollup_repr.Number_of_messages.to_int32
               new_lcc_commitment.number_of_messages)

type conflict_point = Commitment_hash.t * Commitment_hash.t

(** [goto_inbox_level ctxt rollup inbox_level commit] Follows the predecessors of [commit] until it
    arrives at the exact [inbox_level]. The result is the commit hash at the given inbox level. *)
let goto_inbox_level ctxt rollup inbox_level commit =
  let open Lwt_tzresult_syntax in
  let rec go ctxt commit =
    let* (info, ctxt) = get_commitment_internal ctxt rollup commit in
    if Raw_level_repr.(info.Commitment.inbox_level <= inbox_level) then (
      (* Assert that we're exactly at that level. If this isn't the case, we're most likely in a
         situation where inbox levels are inconsistent. *)
      assert (Raw_level_repr.(info.inbox_level = inbox_level)) ;
      return (commit, ctxt))
    else (go [@ocaml.tailcall]) ctxt info.predecessor
  in
  go ctxt commit

let get_conflict_point ctxt rollup staker1 staker2 =
  let open Lwt_tzresult_syntax in
  (* Ensure the LCC is set. *)
  let* (lcc, ctxt) = last_cemented_commitment ctxt rollup in
  (* Find out on which commitments the competitors are staked. *)
  let* (commit1, ctxt) = find_staker ctxt rollup staker1 in
  let* (commit2, ctxt) = find_staker ctxt rollup staker2 in
  let* () =
    fail_when
      Commitment_hash.(
        (* If PVM is in pre-boot state, there might be stakes on the zero commitment. *)
        commit1 = zero || commit2 = zero
        (* If either commit is the LCC, that also means there can't be a conflict. *)
        || commit1 = lcc
        || commit2 = lcc)
      Sc_rollup_no_conflict
  in
  let* (commit1_info, ctxt) = get_commitment_internal ctxt rollup commit1 in
  let* (commit2_info, ctxt) = get_commitment_internal ctxt rollup commit2 in
  (* Make sure that both commits are at the same inbox level. In case they are not move the commit
     that is farther ahead to the exact inbox level of the other.

     We do this instead of an alternating traversal of either commit to ensure the we can detect
     wonky inbox level increases. For example, if the inbox levels decrease in different intervals
     between commits for either history, we risk going past the conflict point and accidentally
     determined that the commits are not in conflict by joining at the same commit. *)
  let target_inbox_level =
    Raw_level_repr.min commit1_info.inbox_level commit2_info.inbox_level
  in
  let* (commit1, ctxt) =
    goto_inbox_level ctxt rollup target_inbox_level commit1
  in
  let* (commit2, ctxt) =
    goto_inbox_level ctxt rollup target_inbox_level commit2
  in
  (* The inbox level of a commitment increases by a fixed amount over the preceding commitment.
     We use this fact in the following to efficiently traverse both commitment histories towards
     the conflict points. *)
  let rec traverse_in_parallel ctxt commit1 commit2 =
    if Commitment_hash.(commit1 = commit2) then
      (* This case will most dominantly happen when either commit is part of the other's history.
         It occurs when the commit that is farther ahead gets dereferenced to its predecessor often
         enough to land at the other commit. *)
      fail Sc_rollup_no_conflict
    else
      let* (commit1_info, ctxt) = get_commitment_internal ctxt rollup commit1 in
      let* (commit2_info, ctxt) = get_commitment_internal ctxt rollup commit2 in
      assert (
        Raw_level_repr.(commit1_info.inbox_level = commit2_info.inbox_level)) ;
      if Commitment_hash.(commit1_info.predecessor = commit2_info.predecessor)
      then
        (* Same predecessor means we've found the conflict points. *)
        return ((commit1, commit2), ctxt)
      else
        (* Different predecessors means they run in parallel. *)
        (traverse_in_parallel [@ocaml.tailcall])
          ctxt
          commit1_info.predecessor
          commit2_info.predecessor
  in
  traverse_in_parallel ctxt commit1 commit2

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

let initial_level ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* level = Storage.Sc_rollup.Initial_level.find ctxt rollup in
  match level with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some level -> return level
