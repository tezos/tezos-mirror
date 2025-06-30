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
    alias : string option;
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
         (req "alias" (option string))
         (req "public_key" Signature.Public_key.encoding)
         (req "public_key_hash" Signature.Public_key_hash.encoding))

  let pp fmt {alias; id; _} =
    match alias with
    | None -> Format.fprintf fmt "%a" Signature.Public_key_hash.pp id
    | Some alias ->
        Format.fprintf fmt "%s (%a)" alias Signature.Public_key_hash.pp id
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
end
