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
  | Invalid_consensus_key_update_noop of
      (Cycle_repr.t * Operation_repr.consensus_key_kind)
  | Invalid_consensus_key_update_active of
      (Signature.Public_key_hash.t * Operation_repr.consensus_key_kind)
  | Invalid_consensus_key_update_tz4 of
      (Bls.Public_key.t * Operation_repr.consensus_key_kind)
  | Invalid_consensus_key_update_another_delegate of
      (Signature.Public_key_hash.t * Operation_repr.consensus_key_kind)

let () =
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.invalid_noop"
    ~title:"Invalid key for consensus key update"
    ~description:"Tried to update the consensus key with the active key"
    ~pp:(fun ppf (cycle, kind) ->
      Format.fprintf
        ppf
        "Invalid key while updating a %a key (already active since %a)."
        Operation_repr.pp_consensus_key_kind
        kind
        Cycle_repr.pp
        cycle)
    Data_encoding.(
      obj2
        (req "cycle" Cycle_repr.encoding)
        (req "kind" Operation_repr.consensus_key_kind_encoding))
    (function Invalid_consensus_key_update_noop c -> Some c | _ -> None)
    (fun c -> Invalid_consensus_key_update_noop c) ;
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.active"
    ~title:"Active consensus key"
    ~description:"The delegate consensus key is already used"
    ~pp:(fun ppf (pkh, kind) ->
      Format.fprintf
        ppf
        "The delegate %a key %a is already used as another consensus or \
         companion key"
        Operation_repr.pp_consensus_key_kind
        kind
        Signature.Public_key_hash.pp
        pkh)
    Data_encoding.(
      obj2
        (req "consensus_pkh" Signature.Public_key_hash.encoding)
        (req "kind" Operation_repr.consensus_key_kind_encoding))
    (function Invalid_consensus_key_update_active c -> Some c | _ -> None)
    (fun c -> Invalid_consensus_key_update_active c) ;
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.tz4"
    ~title:"Consensus key cannot be a tz4"
    ~description:"Consensus key cannot be a tz4 (BLS public key)."
    ~pp:(fun ppf (pk, kind) ->
      Format.fprintf
        ppf
        "The %a key %a is forbidden as it is a BLS public key."
        Operation_repr.pp_consensus_key_kind
        kind
        Bls.Public_key_hash.pp
        (Bls.Public_key.hash pk))
    Data_encoding.(
      obj2
        (req "delegate_pk" Bls.Public_key.encoding)
        (req "kind" Operation_repr.consensus_key_kind_encoding))
    (function Invalid_consensus_key_update_tz4 c -> Some c | _ -> None)
    (fun c -> Invalid_consensus_key_update_tz4 c) ;
  register_error_kind
    `Permanent
    ~id:"delegate.consensus_key.another_delegate"
    ~title:"Consensus key cannot be another delegate"
    ~description:"Consensus key cannot be another delegate"
    ~pp:(fun ppf (pkh, kind) ->
      Format.fprintf
        ppf
        "The key %a cannot be registered as a %a key, as it is the manager key \
         of another registered delegate."
        Signature.Public_key_hash.pp
        pkh
        Operation_repr.pp_consensus_key_kind
        kind)
    Data_encoding.(
      obj2
        (req "consensus_pkh" Signature.Public_key_hash.encoding)
        (req "kind" Operation_repr.consensus_key_kind_encoding))
    (function
      | Invalid_consensus_key_update_another_delegate c -> Some c | _ -> None)
    (fun c -> Invalid_consensus_key_update_another_delegate c)

type 'a typed_kind =
  | Consensus : Signature.public_key typed_kind
  | Companion : Bls.Public_key.t option typed_kind

type pk = Raw_context.consensus_pk = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
  companion_pk : Bls.Public_key.t option;
  companion_pkh : Bls.Public_key_hash.t option;
}

type power = Raw_context.consensus_power = {
  consensus_key : pk;
  attesting_power : Attesting_power_repr.t;
  dal_power : int;
}

type t = {
  delegate : Signature.Public_key_hash.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {delegate; consensus_pkh} -> (delegate, consensus_pkh))
    (fun (delegate, consensus_pkh) -> {delegate; consensus_pkh})
  @@ obj2
       (req "delegate" Signature.Public_key_hash.encoding)
       (req "consensus_pkh" Signature.Public_key_hash.encoding)

let pkh
    {
      delegate;
      consensus_pkh;
      companion_pkh = _;
      consensus_pk = _;
      companion_pk = _;
    } =
  {delegate; consensus_pkh}

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
      No two delegates use the same active companion key at a given time.
      An active key is either a consensus key, or a companion key: it
   cannot be both at once.

   To ensure that, {!Storage.Consensus_keys} contains keys (both consensus and
   companion) that will be active at cycle `current + consensus_rights_delay + 1`.
*)

let check_unused ctxt kind pkh =
  let open Lwt_result_syntax in
  let*! is_active = Storage.Consensus_keys.mem ctxt pkh in
  fail_when is_active (Invalid_consensus_key_update_active (pkh, kind))

let check_not_tz4 kind : Signature.Public_key.t -> unit tzresult =
  let open Result_syntax in
  function
  | Bls pk -> tzfail (Invalid_consensus_key_update_tz4 (pk, kind))
  | Ed25519 _ | Secp256k1 _ | P256 _ | Mldsa44 _ -> return_unit

let check_is_not_another_delegate ctxt kind pkh delegate =
  let open Lwt_result_syntax in
  if Signature.Public_key_hash.equal pkh delegate then return_unit
  else
    let*! is_another_delegate = Storage.Delegates.mem ctxt pkh in
    fail_when
      is_another_delegate
      (Invalid_consensus_key_update_another_delegate (pkh, kind))

let set_unused = Storage.Consensus_keys.remove

let set_used = Storage.Consensus_keys.add

let init ctxt delegate pk =
  let open Lwt_result_syntax in
  let pkh = Signature.Public_key.hash pk in
  let* () = check_unused ctxt Consensus pkh in
  let*! ctxt = set_used ctxt pkh in
  Storage.Contract.Consensus_key.init ctxt (Contract_repr.Implicit delegate) pk

let init_bootstrap ctxt delegate pk =
  let open Lwt_result_syntax in
  (* bootstrap account should be self delegate *)
  let*! ctxt = set_unused ctxt delegate in
  let pkh = Signature.Public_key.hash pk in
  let* () = check_unused ctxt Consensus pkh in
  let*! ctxt = set_used ctxt pkh in
  let*! ctxt =
    Storage.Contract.Consensus_key.add ctxt (Contract_repr.Implicit delegate) pk
  in
  return ctxt

let active_pubkey_kind (type a) ctxt ~(kind : a typed_kind) delegate :
    a tzresult Lwt.t =
  match kind with
  | Consensus ->
      Storage.Contract.Consensus_key.get ctxt (Contract_repr.Implicit delegate)
  | Companion ->
      Storage.Contract.Companion_key.find ctxt (Contract_repr.Implicit delegate)

let active_pubkey ctxt delegate =
  let open Lwt_result_syntax in
  let* consensus_pk = active_pubkey_kind ctxt ~kind:Consensus delegate in
  let consensus_pkh = Signature.Public_key.hash consensus_pk in
  let* companion_pk = active_pubkey_kind ctxt ~kind:Companion delegate in
  let companion_pkh = Option.map Bls.Public_key.hash companion_pk in
  return {consensus_pk; consensus_pkh; companion_pk; companion_pkh; delegate}

let active_key ctxt delegate =
  let open Lwt_result_syntax in
  let* pk = active_pubkey ctxt delegate in
  return (pkh pk)

let raw_pending_updates (type a) ctxt ?up_to_cycle ~(kind : a typed_kind)
    delegate : (Cycle_repr.t * a) list tzresult Lwt.t =
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
  let find (type a) (ctxt, cycle) delegate (kind : a typed_kind) :
      a option tzresult Lwt.t =
    match kind with
    | Consensus -> Storage.Pending_consensus_keys.find (ctxt, cycle) delegate
    | Companion ->
        let* pk = Storage.Pending_companion_keys.find (ctxt, cycle) delegate in
        return (Option.map Option.some pk)
  in
  List.filter_map_es
    (fun cycle ->
      let* pending_for_cycle = find (ctxt, cycle) delegate kind in
      pending_for_cycle |> Option.map (fun pk -> (cycle, pk)) |> return)
    relevant_cycles

let pending_updates ctxt delegate =
  let open Lwt_result_syntax in
  let* updates = raw_pending_updates ctxt ~kind:Consensus delegate in
  return
    (List.map (fun (c, pk) -> (c, Signature.Public_key.hash pk, pk)) updates)

let pending_companion_updates ctxt delegate =
  let open Lwt_result_syntax in
  let* updates = raw_pending_updates ctxt ~kind:Companion delegate in
  return
    (List.filter_map
       (fun (c, pk) ->
         Option.map (fun pk -> (c, Bls.Public_key.hash pk, pk)) pk)
       updates)

let raw_active_pubkey_for_cycle ctxt ~kind delegate cycle =
  let open Lwt_result_syntax in
  let* pendings = raw_pending_updates ctxt ~up_to_cycle:cycle ~kind delegate in
  let* active_pk = active_pubkey_kind ctxt ~kind delegate in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  match List.hd (List.rev pendings) with
  | None -> return (current_cycle, active_pk)
  | Some (cycle, pk) -> return (cycle, pk)

let active_pubkey_for_cycle ctxt delegate cycle =
  let open Lwt_result_syntax in
  let* _, consensus_pk =
    raw_active_pubkey_for_cycle ctxt ~kind:Consensus delegate cycle
  in
  let* _, companion_pk =
    raw_active_pubkey_for_cycle ctxt ~kind:Companion delegate cycle
  in
  return
    {
      consensus_pk;
      consensus_pkh = Signature.Public_key.hash consensus_pk;
      companion_pk;
      companion_pkh = Option.map Bls.Public_key.hash companion_pk;
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
      raw_active_pubkey_for_cycle ctxt ~kind:Consensus delegate update_cycle
    in
    fail_when
      Signature.Public_key.(pk = active_pubkey)
      (Invalid_consensus_key_update_noop (first_active_cycle, Consensus))
  in
  let pkh = Signature.Public_key.hash pk in
  let* () = check_unused ctxt Consensus pkh in
  let* () = check_is_not_another_delegate ctxt Consensus pkh delegate in
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

let register_update_companion ctxt delegate pk =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let cycles_delay = Constants_storage.consensus_key_activation_delay ctxt in
    Cycle_repr.add current_level.cycle (cycles_delay + 1)
  in
  let* () =
    let* first_active_cycle, active_companion_pubkey =
      raw_active_pubkey_for_cycle ctxt ~kind:Companion delegate update_cycle
    in
    match active_companion_pubkey with
    | None -> return_unit
    | Some active_pubkey ->
        fail_when
          Bls.Public_key.(pk = active_pubkey)
          (Invalid_consensus_key_update_noop (first_active_cycle, Companion))
  in
  let pkh : Signature.public_key_hash =
    Signature.Bls (Bls.Public_key.hash pk)
  in
  let* () = check_unused ctxt Companion pkh in
  let* () = check_is_not_another_delegate ctxt Companion pkh delegate in
  let*! ctxt = set_used ctxt pkh in
  let* {companion_pkh = old_pkh; _} =
    active_pubkey_for_cycle ctxt delegate update_cycle
  in
  let*! ctxt =
    match old_pkh with
    | None -> Lwt.return ctxt
    | Some old_pkh -> set_unused ctxt (Signature.Bls old_pkh)
  in
  let*! ctxt =
    Storage.Pending_companion_keys.add
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
  let* ctxt = Storage.Pending_consensus_keys.clear (ctxt, new_cycle) in
  let* ctxt =
    Storage.Pending_companion_keys.fold
      (ctxt, new_cycle)
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun delegate pk ctxt ->
        Storage.Contract.Companion_key.add ctxt delegate pk)
  in
  Storage.Pending_companion_keys.clear (ctxt, new_cycle)
