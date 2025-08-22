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

type seed_computation_status =
  | Nonce_revelation_stage
  | Vdf_revelation_stage of {
      seed_discriminant : Seed_repr.seed;
      seed_challenge : Seed_repr.seed;
    }
  | Computation_finished

type error +=
  | (* `Permanent *)
      Unknown of {
      oldest : Cycle_repr.t;
      cycle : Cycle_repr.t;
      latest : Cycle_repr.t;
    }
  | Already_accepted
  | Unverified_vdf
  | Too_early_revelation

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
    (fun (oldest, cycle, latest) -> Unknown {oldest; cycle; latest}) ;
  register_error_kind
    `Temporary
    ~id:"vdf.too_early_revelation"
    ~title:"Too early VDF revelation"
    ~description:"VDF revelation before the end of the nonce revelation period"
    Data_encoding.unit
    (function Too_early_revelation -> Some () | _ -> None)
    (fun () -> Too_early_revelation) ;
  register_error_kind
    `Branch
    ~id:"vdf.unverified_result"
    ~title:"Unverified VDF"
    ~description:"VDF verification failed"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "A correct VDF result and Wesolowski's proof are expected")
    Data_encoding.unit
    (function Unverified_vdf -> Some () | _ -> None)
    (fun () -> Unverified_vdf) ;
  register_error_kind
    `Branch
    ~id:"vdf.previously_revealed"
    ~title:"Previously revealed VDF"
    ~description:"Duplicate VDF revelation in cycle"
    Data_encoding.unit
    (function Already_accepted -> Some () | _ -> None)
    (fun () -> Already_accepted)

let purge_nonces_and_get_unrevealed ctxt ~cycle =
  let open Lwt_result_syntax in
  let levels = Level_storage.levels_with_commitments_in_cycle ctxt cycle in
  let combine (c, unrevealed) level =
    let* seed = Storage.Seed.Nonce.get c level in
    match seed with
    | Revealed _ ->
        let+ c = Storage.Seed.Nonce.remove_existing c level in
        (c, unrevealed)
    | Unrevealed u ->
        let+ c = Storage.Seed.Nonce.remove_existing c level in
        (c, u :: unrevealed)
  in
  List.fold_left_es combine (ctxt, []) levels

let compute_randao ctxt =
  let open Lwt_result_syntax in
  let current_cycle = (Level_storage.current ctxt).cycle in
  let delay = Constants_storage.consensus_rights_delay ctxt in
  let cycle_computed = Cycle_repr.add current_cycle (delay + 1) in
  let*! seed_computed = Storage.Seed.For_cycle.mem ctxt cycle_computed in
  (* Check if seed has already been computed, and not in cycle 0. *)
  match Cycle_repr.(pred current_cycle, pred cycle_computed) with
  | Some prev_cycle, Some prev_cycle_computed when not seed_computed ->
      (* Retrieve the levels with nonce commitments in the previous cycle. *)
      let levels =
        Level_storage.levels_with_commitments_in_cycle ctxt prev_cycle
      in
      (* Retrieve previous preserved seed. *)
      let* prev_seed = Storage.Seed.For_cycle.get ctxt prev_cycle_computed in
      (* Generate preserved seed by updating previous preserved seed with current revealed nonces. *)
      let combine (c, random_seed) level =
        let* seed = Storage.Seed.Nonce.get c level in
        match seed with
        | Revealed nonce -> return (c, Seed_repr.update_seed random_seed nonce)
        | Unrevealed _ -> return (c, random_seed)
      in
      let seed = Seed_repr.deterministic_seed prev_seed in
      let* c, seed = List.fold_left_es combine (ctxt, seed) levels in
      Storage.Seed.For_cycle.init c cycle_computed seed
  | _, _ -> return ctxt

let get_seed_computation_status ctxt =
  let open Lwt_result_syntax in
  let current_level = Level_storage.current ctxt in
  let current_cycle = current_level.cycle in
  let nonce_revelation_threshold =
    Constants_storage.nonce_revelation_threshold ctxt
  in
  if Compare.Int32.(current_level.cycle_position < nonce_revelation_threshold)
  then return Nonce_revelation_stage
  else
    let* status = Storage.Seed.get_status ctxt in
    match status with
    | RANDAO_seed ->
        let delay = Constants_storage.consensus_rights_delay ctxt in
        let cycle_computed = Cycle_repr.add current_cycle (delay + 1) in
        let previous_cycle = Cycle_repr.add current_cycle delay in
        let* seed_discriminant =
          Storage.Seed.For_cycle.get ctxt previous_cycle
        in
        let* seed_challenge = Storage.Seed.For_cycle.get ctxt cycle_computed in
        return (Vdf_revelation_stage {seed_discriminant; seed_challenge})
    | VDF_seed -> return Computation_finished

let check_vdf ctxt vdf_solution =
  let open Lwt_result_syntax in
  let* r = get_seed_computation_status ctxt in
  match r with
  | Computation_finished -> tzfail Already_accepted
  | Nonce_revelation_stage -> tzfail Too_early_revelation
  | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
      (* To avoid recomputing the discriminant and challenge for every (potentially
       * invalid) submission in a cycle, we compute them once and store them *)
      let* stored = Storage.Seed.VDF_setup.find ctxt in
      let* ctxt, setup =
        match stored with
        | None ->
            let setup =
              Seed_repr.generate_vdf_setup ~seed_discriminant ~seed_challenge
            in
            let*! ctxt = Storage.Seed.VDF_setup.add ctxt setup in
            return (ctxt, setup)
        | Some setup -> return (ctxt, setup)
      in
      let*? () =
        error_unless
          (Option.value
             ~default:false
             (Seed_repr.verify
                setup
                (Constants_storage.vdf_difficulty ctxt)
                vdf_solution))
          Unverified_vdf
      in
      return_unit

let update_seed ctxt vdf_solution =
  let open Lwt_result_syntax in
  (* compute and update seed and change seed status from RANDAO to
     VDF *)
  let current_cycle = (Level_storage.current ctxt).cycle in
  let delay = Constants_storage.consensus_rights_delay ctxt in
  let cycle_computed = Cycle_repr.add current_cycle (delay + 1) in
  let* seed_challenge = Storage.Seed.For_cycle.get ctxt cycle_computed in
  let new_seed = Seed_repr.vdf_to_seed seed_challenge vdf_solution in
  Storage.Seed.For_cycle.update ctxt cycle_computed new_seed Seed_repr.VDF_seed

let raw_for_cycle = Storage.Seed.For_cycle.get

let for_cycle ctxt cycle =
  let open Lwt_result_syntax in
  let delay = Constants_storage.consensus_rights_delay ctxt in
  let current_cycle = Cycle_storage.current ctxt in
  let latest =
    if Cycle_repr.(current_cycle = root) then
      Cycle_repr.add current_cycle (delay + 1)
    else Cycle_repr.add current_cycle delay
  in
  let oldest = Cycle_storage.oldest_cycle_with_sampling_data ctxt in
  let*? () =
    error_unless
      Cycle_repr.(oldest <= cycle && cycle <= latest)
      (Unknown {oldest; cycle; latest})
  in
  Storage.Seed.For_cycle.get ctxt cycle

let init ?initial_seed ctxt =
  let open Lwt_result_syntax in
  let delay = Constants_storage.consensus_rights_delay ctxt in
  let* ctxt = Storage.Seed_status.init ctxt Seed_repr.RANDAO_seed in
  let+ (_ : int), ctxt =
    List.fold_left_es
      (fun (c, ctxt) seed ->
        let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
        let+ ctxt = Storage.Seed.For_cycle.init ctxt cycle seed in
        (c + 1, ctxt))
      (0, ctxt)
      (Seed_repr.initial_seeds ?initial_seed (delay + 2))
  in
  ctxt

let cycle_end ctxt last_cycle =
  let open Lwt_result_syntax in
  let*! ctxt = Storage.Seed.VDF_setup.remove ctxt in
  (* NB: the clearing of past seeds is done elsewhere by the caller *)
  match Cycle_repr.pred last_cycle with
  | None -> return (ctxt, [])
  | Some previous_cycle ->
      (* cycle with revelations *)
      purge_nonces_and_get_unrevealed ctxt ~cycle:previous_cycle

let remove_for_cycle = Storage.Seed.For_cycle.remove_existing
