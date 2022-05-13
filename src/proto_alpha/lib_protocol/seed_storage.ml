(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
  | Unknown of {
      oldest : Cycle_repr.t;
      cycle : Cycle_repr.t;
      latest : Cycle_repr.t;
    }

(* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"seed.unknown_seed"
    ~title:"Unknown seed"
    ~description:"The requested seed is not available"
    ~pp:(fun ppf (oldest, cycle, latest) ->
      if Cycle_repr.(cycle < oldest) then
        Format.fprintf
          ppf
          "The seed for cycle %a has been cleared from the context  (oldest \
           known seed is for cycle %a)"
          Cycle_repr.pp
          cycle
          Cycle_repr.pp
          oldest
      else
        Format.fprintf
          ppf
          "The seed for cycle %a has not been computed yet  (latest known seed \
           is for cycle %a)"
          Cycle_repr.pp
          cycle
          Cycle_repr.pp
          latest)
    Data_encoding.(
      obj3
        (req "oldest" Cycle_repr.encoding)
        (req "requested" Cycle_repr.encoding)
        (req "latest" Cycle_repr.encoding))
    (function
      | Unknown {oldest; cycle; latest} -> Some (oldest, cycle, latest)
      | _ -> None)
    (fun (oldest, cycle, latest) -> Unknown {oldest; cycle; latest})

let compute_unrevealed c ~revealed =
  let levels = Level_storage.levels_with_commitments_in_cycle c revealed in
  let combine (c, unrevealed) level =
    Storage.Seed.Nonce.get c level >>=? function
    | Revealed _ ->
        Storage.Seed.Nonce.remove_existing c level >|=? fun c -> (c, unrevealed)
    | Unrevealed u ->
        Storage.Seed.Nonce.remove_existing c level >|=? fun c ->
        (c, u :: unrevealed)
  in
  List.fold_left_es combine (c, []) levels

let compute_randao ctxt =
  let current_cycle = (Level_storage.current ctxt).cycle in
  let preserved = Constants_storage.preserved_cycles ctxt in
  let cycle_computed = Cycle_repr.add current_cycle (preserved + 1) in
  Storage.Seed.For_cycle.mem ctxt cycle_computed >>= fun seed_computed ->
  (* Check if seed has already been computed, and not in cycle 0. *)
  match Cycle_repr.(seed_computed, pred current_cycle, pred cycle_computed) with
  | b, Some prev_cycle, Some prev_cycle_computed when not b ->
      (* Retrieve the levels with nonce commitments in the previous cycle. *)
      let levels =
        Level_storage.levels_with_commitments_in_cycle ctxt prev_cycle
      in
      (* Retrieve previous preserved seed. *)
      Storage.Seed.For_cycle.get ctxt prev_cycle_computed >>=? fun prev_seed ->
      (* Generate preserved seed by updating previous preserved seed with current revealed nonces. *)
      let combine (c, random_seed) level =
        Storage.Seed.Nonce.get c level >>=? function
        | Revealed nonce -> return (c, Seed_repr.nonce random_seed nonce)
        | Unrevealed _ -> return (c, random_seed)
      in
      let seed = Seed_repr.deterministic_seed prev_seed in
      List.fold_left_es combine (ctxt, seed) levels >>=? fun (c, seed) ->
      Storage.Seed.For_cycle.init c cycle_computed seed
  | _, _, _ -> return ctxt

let for_cycle ctxt cycle =
  let preserved = Constants_storage.preserved_cycles ctxt in
  let current_cycle = (Level_storage.current ctxt).cycle in
  let latest =
    if Cycle_repr.(current_cycle = root) then
      Cycle_repr.add current_cycle (preserved + 1)
    else Cycle_repr.add current_cycle preserved
  in
  let oldest =
    match Cycle_repr.sub current_cycle preserved with
    | None -> Cycle_repr.root
    | Some oldest -> oldest
  in
  error_unless
    Cycle_repr.(oldest <= cycle && cycle <= latest)
    (Unknown {oldest; cycle; latest})
  >>?= fun () -> Storage.Seed.For_cycle.get ctxt cycle

let init ?initial_seed ctxt =
  let preserved = Constants_storage.preserved_cycles ctxt in
  List.fold_left_es
    (fun (c, ctxt) seed ->
      let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
      Storage.Seed.For_cycle.init ctxt cycle seed >|=? fun ctxt -> (c + 1, ctxt))
    (0, ctxt)
    (Seed_repr.initial_seeds ?initial_seed (preserved + 2))
  >|=? snd

let cycle_end ctxt last_cycle =
  (* NB: the clearing of past seeds is done elsewhere by the caller *)
  match Cycle_repr.pred last_cycle with
  | None -> return (ctxt, [])
  | Some previous_cycle ->
      (* cycle with revelations *)
      compute_unrevealed ctxt ~revealed:previous_cycle
