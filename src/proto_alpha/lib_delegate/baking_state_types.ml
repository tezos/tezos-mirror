(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Key_id = struct
  type t = Signature.Public_key_hash.t

  let to_pkh pkh = pkh

  let compare = Signature.Public_key_hash.compare

  let equal = Signature.Public_key_hash.equal

  let encoding = Signature.Public_key_hash.encoding

  let pp = Signature.Public_key_hash.pp

  module Table = Signature.Public_key_hash.Table
end

module Key = struct
  (** A consensus key (aka, a validator) is identified by its alias name, its
    public key, its public key hash, and its secret key. *)
  type t = {
    alias : string;
    id : Key_id.t;
    public_key : Signature.Public_key.t;
    secret_key_uri : Client_keys.sk_uri;
  }

  let make ~alias ~public_key ~public_key_hash ~secret_key_uri =
    {alias; public_key; id = public_key_hash; secret_key_uri}

  let encoding_for_logging__cannot_decode =
    let open Data_encoding in
    conv
      (fun {alias; public_key; id; secret_key_uri = _} ->
        (alias, public_key, id))
      (fun _ ->
        Stdlib.failwith
          (Format.sprintf
             "This encoding should only be used to encode values for event \
              logging; decoding is impossible (%s)"
             __LOC__))
      (obj3
         (req "alias" string)
         (req "public_key" Signature.Public_key.encoding)
         (req "public_key_hash" Signature.Public_key_hash.encoding))

  let pp fmt {alias; id; _} =
    Format.fprintf fmt "'%s' (%a)" alias Signature.Public_key_hash.pp id

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare {id; _} {id = id'; _} = Key_id.compare id id'
    end)

    let find_pkh pkh s =
      let exception Found of elt in
      try
        iter
          (fun ({id; _} as delegate) ->
            if Signature.Public_key_hash.equal pkh (Key_id.to_pkh id) then
              raise (Found delegate)
            else ())
          s ;
        None
      with Found d -> Some d
  end
end

module Maybe_known_key = struct
  type t = Known_key of Key.t | Id_only of Key_id.t

  let key_id = function Known_key key -> key.id | Id_only id -> id

  let encoding_for_logging__cannot_decode =
    let open Data_encoding in
    conv
      (function
        | Known_key {alias; public_key; id; _} ->
            (Some alias, Some public_key, id)
        | Id_only pkh -> (None, None, pkh))
      (fun _ ->
        Stdlib.failwith
          (Format.sprintf
             "This event should only be used to encode events; decoding is \
              impossible: %s"
             __LOC__))
      (obj3
         (opt "alias" string)
         (opt "public_key" Signature.Public_key.encoding)
         (req "public_key_hash" Signature.Public_key_hash.encoding))

  let pp fmt = function
    | Known_key key -> Key.pp fmt key
    | Id_only id -> Key_id.pp fmt id

  let make ~known_keys pkh =
    match Key.Set.find_pkh pkh known_keys with
    | None -> Id_only pkh
    | Some key -> Known_key key
end

module Delegate_id = Key_id

module Delegate = struct
  type manager_key = Maybe_known_key.t

  type t = {
    manager_key : manager_key;
    consensus_key : Key.t;
    companion_key : Key.t option;
  }

  let delegate_id t = Maybe_known_key.key_id t.manager_key

  let encoding_for_logging__cannot_decode =
    let open Data_encoding in
    conv
      (fun {manager_key; consensus_key; companion_key} ->
        (manager_key, consensus_key, companion_key))
      (fun (manager_key, consensus_key, companion_key) ->
        {manager_key; consensus_key; companion_key})
      (obj3
         (req "manager_key" Maybe_known_key.encoding_for_logging__cannot_decode)
         (req "consensus_key" Key.encoding_for_logging__cannot_decode)
         (opt "companion_key" Key.encoding_for_logging__cannot_decode))

  let pp fmt {manager_key; consensus_key; companion_key} =
    Format.fprintf
      fmt
      "%a@ with@ consensus key@ %a"
      Maybe_known_key.pp
      manager_key
      Key.pp
      consensus_key ;
    match companion_key with
    | None -> ()
    | Some companion_key ->
        Format.fprintf fmt "@ and@ companion key@ %a" Key.pp companion_key

  let companion_key_is_not_in_wallet =
    let open Internal_event.Simple in
    declare_2
      ~section:[Protocol.name; "baker"; "delegates"]
      ~name:"companion_key_is_not_in_wallet"
      ~level:Error
      ~msg:
        "Companion key {companion_key} for {delegate} is absent from the \
         client wallet. The baker will only be able to issue attestations \
         without DAL for this delegate, and the delegate will lose DAL \
         rewards."
      ("delegate", encoding_for_logging__cannot_decode)
      ("companion_key", Environment.Bls.Public_key_hash.encoding)

  let of_validator ~known_keys
      {
        Plugin.RPC.Validators.consensus_key = consensus_pkh;
        companion_key = companion_bls_pkh_opt;
        delegate = manager_pkh;
        slots = _;
        level = _;
      } =
    let open Lwt_syntax in
    match Key.Set.find_pkh consensus_pkh known_keys with
    | None -> return_none
    | Some consensus_key ->
        let manager_key = Maybe_known_key.make ~known_keys manager_pkh in
        let* companion_key =
          match companion_bls_pkh_opt with
          | None -> return_none
          | Some companion_bls_pkh ->
              let companion_key =
                Key.Set.find_pkh (Bls companion_bls_pkh) known_keys
              in
              let* () =
                if
                  Option.is_none companion_key
                  && Signature.Public_key_hash.is_bls consensus_pkh
                then
                  Events.(
                    emit
                      companion_key_is_not_in_wallet
                      ( {manager_key; consensus_key; companion_key},
                        companion_bls_pkh ))
                else return_unit
              in
              return companion_key
        in
        return_some {manager_key; consensus_key; companion_key}
end
