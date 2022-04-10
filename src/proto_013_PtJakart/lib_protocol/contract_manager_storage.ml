(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
  | (* `Branch *) Unrevealed_manager_key of Contract_repr.t
  | (* `Permanent *)
      Inconsistent_hash of {
      public_key : Signature.Public_key.t;
      expected_hash : Signature.Public_key_hash.t;
      provided_hash : Signature.Public_key_hash.t;
    }
  | (* `Branch *) Previously_revealed_key of Contract_repr.t
  | (* `Branch *) Missing_manager_contract of Contract_repr.t

let () =
  register_error_kind
    `Branch
    ~id:"contract.unrevealed_key"
    ~title:"Manager operation precedes key revelation"
    ~description:
      "One tried to apply a manager operation without revealing the manager \
       public key"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Unrevealed manager key for contract %a."
        Contract_repr.pp
        s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Unrevealed_manager_key s -> Some s | _ -> None)
    (fun s -> Unrevealed_manager_key s) ;
  register_error_kind
    `Permanent
    ~id:"contract.manager.inconsistent_hash"
    ~title:"Inconsistent public key hash"
    ~description:
      "A revealed manager public key is inconsistent with the announced hash"
    ~pp:(fun ppf (k, eh, ph) ->
      Format.fprintf
        ppf
        "The hash of the manager public key %s is not %a as announced but %a"
        (Signature.Public_key.to_b58check k)
        Signature.Public_key_hash.pp
        ph
        Signature.Public_key_hash.pp
        eh)
    Data_encoding.(
      obj3
        (req "public_key" Signature.Public_key.encoding)
        (req "expected_hash" Signature.Public_key_hash.encoding)
        (req "provided_hash" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_hash {public_key; expected_hash; provided_hash} ->
          Some (public_key, expected_hash, provided_hash)
      | _ -> None)
    (fun (public_key, expected_hash, provided_hash) ->
      Inconsistent_hash {public_key; expected_hash; provided_hash}) ;
  register_error_kind
    `Branch
    ~id:"contract.previously_revealed_key"
    ~title:"Manager operation already revealed"
    ~description:"One tried to reveal twice a manager public key"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Previously revealed manager key for contract %a."
        Contract_repr.pp
        s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Previously_revealed_key s -> Some s | _ -> None)
    (fun s -> Previously_revealed_key s) ;
  register_error_kind
    `Branch
    ~id:"contract.missing_manager_contract"
    ~title:"Missing manager contract"
    ~description:"The manager contract is missing from the storage"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "The contract %a is missing from the storage."
        Contract_repr.pp
        s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Missing_manager_contract s -> Some s | _ -> None)
    (fun s -> Missing_manager_contract s)

let init = Storage.Contract.Manager.init

let is_manager_key_revealed c manager =
  let contract = Contract_repr.implicit_contract manager in
  Storage.Contract.Manager.find c contract >>=? function
  | None -> return_false
  | Some (Manager_repr.Hash _) -> return_false
  | Some (Manager_repr.Public_key _) -> return_true

let reveal_manager_key c manager public_key =
  let contract = Contract_repr.implicit_contract manager in
  Storage.Contract.Manager.get c contract >>=? function
  | Public_key _ -> fail (Previously_revealed_key contract)
  | Hash v ->
      let actual_hash = Signature.Public_key.hash public_key in
      if Signature.Public_key_hash.equal actual_hash v then
        let v = Manager_repr.Public_key public_key in
        Storage.Contract.Manager.update c contract v
      else
        fail
          (Inconsistent_hash
             {public_key; expected_hash = v; provided_hash = actual_hash})

let get_manager_key ?error ctxt pkh =
  let contract = Contract_repr.implicit_contract pkh in
  Storage.Contract.Manager.find ctxt contract >>=? function
  | None -> (
      match error with
      | None -> fail (Missing_manager_contract contract)
      | Some error -> fail error)
  | Some (Manager_repr.Hash _) -> (
      match error with
      | None -> fail (Unrevealed_manager_key contract)
      | Some error -> fail error)
  | Some (Manager_repr.Public_key pk) -> return pk

let remove_existing = Storage.Contract.Manager.remove_existing
