(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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
  | Invalid_consensus_key_update_noop of Cycle_repr.t
  | Invalid_consensus_key_update_active
  | Invalid_consensus_key_update_tz4 of Bls.Public_key.t

let () =
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.invalid_noop"
    ~title:"Invalid key for consensus key update"
    ~description:"Tried to update the consensus key with the active key"
    ~pp:(fun ppf cycle ->
      Format.fprintf
        ppf
        "Invalid key while updating a consensus key (already active since %a)."
        Cycle_repr.pp
        cycle)
    Data_encoding.(obj1 (req "cycle" Cycle_repr.encoding))
    (function Invalid_consensus_key_update_noop c -> Some c | _ -> None)
    (fun c -> Invalid_consensus_key_update_noop c) ;
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.active"
    ~title:"Active consensus key"
    ~description:
      "The delegate consensus key is already used by another delegate"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The delegate consensus key is already used by another delegate")
    Data_encoding.empty
    (function Invalid_consensus_key_update_active -> Some () | _ -> None)
    (fun () -> Invalid_consensus_key_update_active) ;
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.tz4"
    ~title:"Consensus key cannot be a tz4"
    ~description:"Consensus key cannot be a tz4 (BLS public key)."
    ~pp:(fun ppf pk ->
      Format.fprintf
        ppf
        "The consensus key %a is forbidden as it is a BLS public key."
        Bls.Public_key_hash.pp
        (Bls.Public_key.hash pk))
    Data_encoding.(obj1 (req "delegate_pk" Bls.Public_key.encoding))
    (function Invalid_consensus_key_update_tz4 pk -> Some pk | _ -> None)
    (fun pk -> Invalid_consensus_key_update_tz4 pk)

type pk = Raw_context.consensus_pk = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

type t = {
  delegate : Signature.Public_key_hash.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

let pkh {delegate; consensus_pkh; consensus_pk = _} = {delegate; consensus_pkh}

let zero =
  {
    consensus_pkh = Signature.Public_key_hash.zero;
    delegate = Signature.Public_key_hash.zero;
  }

let pp ppf {delegate; consensus_pkh} =
  Format.fprintf ppf "@[<v 2>%a" Signature.Public_key_hash.pp delegate ;
  if not (Signature.Public_key_hash.equal delegate consensus_pkh) then
    Format.fprintf
      ppf
      "@,Active key: %a"
      Signature.Public_key_hash.pp
      consensus_pkh ;
  Format.fprintf ppf "@]"

(* Invariant:
      No two delegates use the same active consensus key at a given time.

   To ensure that, {!Storage.Consensus_keys} contains keys that will be active
   at cycle `current + preserved_cycles + 1`.
*)

let check_unused ctxt pkh =
  let open Lwt_result_syntax in
  let*! is_active = Storage.Consensus_keys.mem ctxt pkh in
  fail_when is_active Invalid_consensus_key_update_active

let check_not_tz4 : Signature.Public_key.t -> unit tzresult =
  let open Result_syntax in
  function
  | Bls pk -> tzfail (Invalid_consensus_key_update_tz4 pk)
  | Ed25519 _ | Secp256k1 _ | P256 _ -> return_unit

let set_unused = Storage.Consensus_keys.remove

let set_used = Storage.Consensus_keys.add

let init ctxt delegate pk =
  let open Lwt_result_syntax in
  let*? () = check_not_tz4 pk in
  let pkh = Signature.Public_key.hash pk in
  let* () = check_unused ctxt pkh in
  let*! ctxt = set_used ctxt pkh in
  Storage.Contract.Consensus_key.init ctxt (Contract_repr.Implicit delegate) pk

let active_pubkey ctxt delegate =
  let open Lwt_result_syntax in
  let* pk =
    Storage.Contract.Consensus_key.get ctxt (Contract_repr.Implicit delegate)
  in
  let pkh = Signature.Public_key.hash pk in
  return {consensus_pk = pk; consensus_pkh = pkh; delegate}

let active_key ctxt delegate =
  let open Lwt_result_syntax in
  let* pk = active_pubkey ctxt delegate in
  return (pkh pk)

let raw_pending_updates ctxt ?up_to_cycle delegate =
  let open Lwt_result_syntax in
  let relevant_cycles =
    let level = Raw_context.current_level ctxt in
    let first_cycle = Cycle_repr.succ level.cycle in
    let last_cycle =
      match up_to_cycle with
      | None ->
          let cycles_delay =
            Constants_storage.consensus_key_activation_delay ctxt
          in
          Cycle_repr.add first_cycle cycles_delay
      | Some cycle -> cycle
    in
    Cycle_repr.(first_cycle ---> last_cycle)
  in
  let delegate = Contract_repr.Implicit delegate in
  List.filter_map_es
    (fun cycle ->
      let* pending_for_cycle =
        Storage.Pending_consensus_keys.find (ctxt, cycle) delegate
      in
      pending_for_cycle |> Option.map (fun pk -> (cycle, pk)) |> return)
    relevant_cycles

let pending_updates ctxt delegate =
  let open Lwt_result_syntax in
  let* updates = raw_pending_updates ctxt delegate in
  return
    (List.map (fun (c, pk) -> (c, Signature.Public_key.hash pk, pk)) updates)

let raw_active_pubkey_for_cycle ctxt delegate cycle =
  let open Lwt_result_syntax in
  let* pendings = raw_pending_updates ctxt ~up_to_cycle:cycle delegate in
  let* active = active_pubkey ctxt delegate in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  match List.hd (List.rev pendings) with
  | None -> return (current_cycle, active.consensus_pk)
  | Some (cycle, pk) -> return (cycle, pk)

let active_pubkey_for_cycle ctxt delegate cycle =
  let open Lwt_result_syntax in
  let+ _, consensus_pk = raw_active_pubkey_for_cycle ctxt delegate cycle in
  {
    consensus_pk;
    consensus_pkh = Signature.Public_key.hash consensus_pk;
    delegate;
  }

let register_update ctxt delegate pk =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let cycles_delay = Constants_storage.consensus_key_activation_delay ctxt in
    Cycle_repr.add current_level.cycle (cycles_delay + 1)
  in
  let* () =
    let* first_active_cycle, active_pubkey =
      raw_active_pubkey_for_cycle ctxt delegate update_cycle
    in
    fail_when
      Signature.Public_key.(pk = active_pubkey)
      (Invalid_consensus_key_update_noop first_active_cycle)
  in
  let*? () = check_not_tz4 pk in
  let pkh = Signature.Public_key.hash pk in
  let* () = check_unused ctxt pkh in
  let*! ctxt = set_used ctxt pkh in
  let* {consensus_pkh = old_pkh; _} =
    active_pubkey_for_cycle ctxt delegate update_cycle
  in
  let*! ctxt = set_unused ctxt old_pkh in
  let*! ctxt =
    Storage.Pending_consensus_keys.add
      (ctxt, update_cycle)
      (Contract_repr.Implicit delegate)
      pk
  in
  return ctxt

let activate ctxt ~new_cycle =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Pending_consensus_keys.fold
      (ctxt, new_cycle)
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun delegate pk ctxt ->
        Storage.Contract.Consensus_key.add ctxt delegate pk)
  in
  Storage.Pending_consensus_keys.clear (ctxt, new_cycle)
