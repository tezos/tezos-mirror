(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let unslashable_cycle ctxt ~cycle =
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let max_slashing_period = Constants_storage.max_slashing_period ctxt in
  Cycle_repr.sub cycle (preserved_cycles + max_slashing_period - 1)

let squash_unstaked_frozen_deposits ctxt ~from_cycle ~into_cycle ~delegate =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* from_unstaked_frozen_deposits_opt =
    Storage.Contract.Unstaked_frozen_deposits.find (ctxt, contract) from_cycle
  in
  match from_unstaked_frozen_deposits_opt with
  | None -> (* nothing to squash *) return ctxt
  | Some from_unstaked_frozen_deposits ->
      let* ctxt =
        Storage.Contract.Unstaked_frozen_deposits.remove_existing
          (ctxt, contract)
          from_cycle
      in
      let* into_unstaked_frozen_deposits_opt =
        Storage.Contract.Unstaked_frozen_deposits.find
          (ctxt, contract)
          into_cycle
      in
      let*? squashed_unstaked_frozen_deposits =
        match into_unstaked_frozen_deposits_opt with
        | None -> Ok from_unstaked_frozen_deposits
        | Some into_unstaked_frozen_deposits ->
            Deposits_repr.(
              from_unstaked_frozen_deposits ++? into_unstaked_frozen_deposits)
      in
      let*! ctxt =
        Storage.Contract.Unstaked_frozen_deposits.add
          (ctxt, contract)
          into_cycle
          squashed_unstaked_frozen_deposits
      in
      return ctxt

let squash_unslashable_unstaked_frozen_deposits_at_cycle_end ctxt ~last_cycle =
  match unslashable_cycle ctxt ~cycle:last_cycle with
  | None -> return ctxt
  | Some last_unslashable_cycle ->
      let new_unslashable_cycle = Cycle_repr.succ last_unslashable_cycle in
      Storage.Delegates.fold
        ctxt
        ~order:`Undefined
        ~init:(ok ctxt)
        ~f:(fun delegate ctxt ->
          let open Lwt_result_syntax in
          let*? ctxt in
          squash_unstaked_frozen_deposits
            ctxt
            ~from_cycle:last_unslashable_cycle
            ~into_cycle:new_unslashable_cycle
            ~delegate)
