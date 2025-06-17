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
    Format.fprintf fmt "%s (%a)" alias Signature.Public_key_hash.pp id

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

module Delegate_id = struct
  type t = Signature.public_key_hash

  let of_pkh pkh = pkh

  let to_pkh pkh = pkh

  let equal = Signature.Public_key_hash.equal

  let encoding = Signature.Public_key_hash.encoding

  let pp = Signature.Public_key_hash.pp
end

module Delegate = struct
  type t = {
    consensus_key : Key.t;
    companion_key : Key.t option;
    delegate_id : Delegate_id.t;
  }

  let encoding_for_logging__cannot_decode =
    let open Data_encoding in
    conv
      (fun {consensus_key; companion_key; delegate_id} ->
        (consensus_key, delegate_id, companion_key))
      (fun (consensus_key, delegate_id, companion_key) ->
        {consensus_key; delegate_id; companion_key})
      (obj3
         (req "consensus_key" Key.encoding_for_logging__cannot_decode)
         (req "delegate" Delegate_id.encoding)
         (opt "companion_key" Key.encoding_for_logging__cannot_decode))

  let pp fmt {consensus_key; delegate_id; companion_key} =
    let str_companion_key =
      match companion_key with
      | Some companion_key ->
          Format.asprintf " with companion key %a" Key.pp companion_key
      | None -> ""
    in
    if Signature.Public_key_hash.equal consensus_key.id delegate_id then
      Format.fprintf fmt "%a%s" Key.pp consensus_key str_companion_key
    else
      Format.fprintf
        fmt
        "%a%s@,on behalf of %a"
        Key.pp
        consensus_key
        str_companion_key
        Delegate_id.pp
        delegate_id

  let companion_key_is_not_in_wallet =
    let open Internal_event.Simple in
    declare_2
      ~section:[Protocol.name; "baker"; "delegates"]
      ~name:"companion_key_is_not_in_wallet"
      ~level:Error
      ~msg:
        "Companion key {companion_key} is not provided in the wallet but \
         registered in the protocol for {delegate}"
      ("delegate", Delegate_id.encoding)
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
        let delegate_id = Delegate_id.of_pkh manager_pkh in
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
                      (delegate_id, companion_bls_pkh))
                else return_unit
              in
              return companion_key
        in
        return_some {consensus_key; delegate_id; companion_key}
end
