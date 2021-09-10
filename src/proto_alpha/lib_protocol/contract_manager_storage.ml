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
      Inconsistent_hash of
      Signature.Public_key.t
      * Signature.Public_key_hash.t
      * Signature.Public_key_hash.t
  | (* `Branch *) Previously_revealed_key of Contract_repr.t

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
    (function Inconsistent_hash (k, eh, ph) -> Some (k, eh, ph) | _ -> None)
    (fun (k, eh, ph) -> Inconsistent_hash (k, eh, ph)) ;
  register_error_kind
    `Branch
    ~id:"contract.previously_revealed_key"
    ~title:"Manager operation already revealed"
    ~description:"One tried to revealed twice a manager public key"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Previously revealed manager key for contract %a."
        Contract_repr.pp
        s)
    Data_encoding.(obj1 (req "contract" Contract_repr.encoding))
    (function Previously_revealed_key s -> Some s | _ -> None)
    (fun s -> Previously_revealed_key s)

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
      else fail (Inconsistent_hash (public_key, v, actual_hash))

let get_manager_key c manager =
  let contract = Contract_repr.implicit_contract manager in
  Storage.Contract.Manager.find c contract >>=? function
  | None -> failwith "get_manager_key"
  | Some (Manager_repr.Hash _) -> fail (Unrevealed_manager_key contract)
  | Some (Manager_repr.Public_key v) -> return v

let revealed_key ctxt delegate error =
  Storage.Contract.Manager.find ctxt (Contract_repr.implicit_contract delegate)
  >>=? function
  | None | Some (Manager_repr.Hash _) -> fail error
  (* (Unregistered_delegate delegate) *)
  | Some (Manager_repr.Public_key pk) -> return pk

let remove_existing = Storage.Contract.Manager.remove_existing
