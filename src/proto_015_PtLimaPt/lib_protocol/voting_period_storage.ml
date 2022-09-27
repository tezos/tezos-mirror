(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(*
  The shell uses the convention that a context at level n is the resulting
  context of the application of block n.
  Therefore when using an RPC on the last level of a voting period, the context
  that is inspected is the resulting one.

  However [Amendment.may_start_new_voting_period] is run at the end of voting
  period and it has to prepare the context for validating operations of the next
  period. This causes the counter-intuitive result that the info returned by RPCs
  at last level of a voting period mention data of the next voting period.

  For example, when validating the last block of a proposal period at level n
  we have:
  - Input context:

       voting_period = { kind = Proposal;
                         index = i;
                         start_position = n - blocks_per_voting_period}

       - position  = n - start_position = blocks_per_voting_period
       - remaining = blocks_per_voting_period - (position + 1) = 0

  - Output context:

       voting_period = { kind = Exploration;
                         index = i + 1;
                         start_position = n + 1}

      Now if we calculate position and remaining in the voting period we get
      strange results:
       - position  = n - (n + 1) = -1
       - remaining = blocks_per_voting_period

  To work around this issue, two RPCs were added
  `Voting_period_storage.get_rpc_current_info`, which returns the correct
  info also for the last context of a period, and
  `Voting_period_storage.get_rpc_succ_info`, which can be used at the last
  context of a period to craft operations that will be valid for the first
  block of the new period.

  This odd behaviour could be fixed if [Amendment.may_start_new_voting_period]
  was called when we start validating the first block of a voting period instead
  that at the end of the validation of the last block of a voting period.
  This should be carefully done because the voting period listing depends on
  the rolls and it might break some invariant.

  When this is implemented one should:
  - edit the function [reset_current] and [inc_current] to use the
    current level and not the next one.
  - remove the storage for pred_kind
  - make Voting_period_repr.t abstract

  You can also look at the MR description here:
  https://gitlab.com/metastatedev/tezos/-/merge_requests/333
 *)

(* Voting periods start at the first block of a cycle. More formally,
   the invariant of start_position with respect to cycle_position is:
     cycle_position mod blocks_per_cycle ==
     position_in_period mod blocks_per_cycle *)

let blocks_per_voting_period ctxt =
  let open Constants_storage in
  Int32.(mul (cycles_per_voting_period ctxt) (blocks_per_cycle ctxt))

let set_current = Storage.Vote.Current_period.update

let get_current = Storage.Vote.Current_period.get

let init = Storage.Vote.Current_period.init

let init_first_period ctxt ~start_position =
  init ctxt @@ Voting_period_repr.root ~start_position >>=? fun ctxt ->
  Storage.Vote.Pred_period_kind.init ctxt Voting_period_repr.Proposal

let common ctxt =
  get_current ctxt >>=? fun current_period ->
  Storage.Vote.Pred_period_kind.update ctxt current_period.kind >|=? fun ctxt ->
  let start_position =
    (* because we are preparing the voting period for the next block we need to
       use the next level. *)
    Int32.succ (Level_storage.current ctxt).level_position
  in
  (ctxt, current_period, start_position)

let reset ctxt =
  common ctxt >>=? fun (ctxt, current_period, start_position) ->
  Voting_period_repr.raw_reset current_period ~start_position
  |> set_current ctxt

let succ ctxt =
  common ctxt >>=? fun (ctxt, current_period, start_position) ->
  Voting_period_repr.raw_succ current_period ~start_position |> set_current ctxt

let get_current_kind ctxt = get_current ctxt >|=? fun {kind; _} -> kind

let get_current_info ctxt =
  get_current ctxt >|=? fun voting_period ->
  let blocks_per_voting_period = blocks_per_voting_period ctxt in
  let level = Level_storage.current ctxt in
  let position = Voting_period_repr.position_since level voting_period in
  let remaining =
    Voting_period_repr.remaining_blocks
      level
      voting_period
      ~blocks_per_voting_period
  in
  Voting_period_repr.{voting_period; position; remaining}

let get_current_remaining ctxt =
  get_current ctxt >|=? fun voting_period ->
  let blocks_per_voting_period = blocks_per_voting_period ctxt in
  Voting_period_repr.remaining_blocks
    (Level_storage.current ctxt)
    voting_period
    ~blocks_per_voting_period

let is_last_block ctxt =
  get_current_remaining ctxt >|=? fun remaining ->
  Compare.Int32.(remaining = 0l)

let blocks_before_activation ctxt =
  get_current ctxt >>=? function
  | Voting_period_repr.{kind = Adoption; _} ->
      get_current_remaining ctxt >>=? return_some
  | _ -> return_none

let get_rpc_current_info ctxt =
  get_current_info ctxt
  >>=? fun ({voting_period; position; _} as voting_period_info) ->
  if Compare.Int32.(position = Int32.minus_one) then
    let level = Level_storage.current ctxt in
    let blocks_per_voting_period = blocks_per_voting_period ctxt in
    Storage.Vote.Pred_period_kind.get ctxt >|=? fun pred_kind ->
    let voting_period : Voting_period_repr.t =
      {
        index = Int32.pred voting_period.index;
        kind = pred_kind;
        start_position =
          Int32.(sub voting_period.start_position blocks_per_voting_period);
      }
    in
    let position = Voting_period_repr.position_since level voting_period in
    let remaining =
      Voting_period_repr.remaining_blocks
        level
        voting_period
        ~blocks_per_voting_period
    in
    ({voting_period; remaining; position} : Voting_period_repr.info)
  else return voting_period_info

let get_rpc_succ_info ctxt =
  Level_storage.from_raw_with_offset
    ctxt
    ~offset:1l
    (Level_storage.current ctxt).level
  >>?= fun level ->
  get_current ctxt >|=? fun voting_period ->
  let blocks_per_voting_period = blocks_per_voting_period ctxt in
  let position = Voting_period_repr.position_since level voting_period in
  let remaining =
    Voting_period_repr.remaining_blocks
      level
      voting_period
      ~blocks_per_voting_period
  in
  Voting_period_repr.{voting_period; position; remaining}

module Testnet_dictator = struct
  (* This error must never happen. It is deliberately unregistered so
     that the execution fails loudly if [overwrite_current_kind] is
     ever called on mainnet. *)
  type error += Forbidden_on_mainnet

  let overwrite_current_kind ctxt chain_id kind =
    error_when
      Chain_id.(chain_id = Constants_repr.mainnet_id)
      Forbidden_on_mainnet
    >>?= fun () ->
    get_current ctxt >>=? fun current_period ->
    let new_period = {current_period with kind} in
    set_current ctxt new_period
end
