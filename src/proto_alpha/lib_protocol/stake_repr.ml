(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type staker =
  | Single of Contract_repr.t * Signature.public_key_hash
  | Shared of Signature.public_key_hash

let staker_encoding =
  let open Data_encoding in
  let single_tag = 0 in
  let single_encoding =
    obj2
      (req "contract" Contract_repr.encoding)
      (req "delegate" Signature.Public_key_hash.encoding)
  in
  let shared_tag = 1 in
  let shared_encoding =
    obj1 (req "delegate" Signature.Public_key_hash.encoding)
  in
  def
    ~title:"staker"
    ~description:
      "Abstract notion of staker used in operation receipts, either a single \
       staker or all the stakers delegating to some delegate."
    "staker"
  @@ matching
       (function
         | Single (contract, delegate) ->
             matched single_tag single_encoding (contract, delegate)
         | Shared delegate -> matched shared_tag shared_encoding delegate)
       [
         case
           ~title:"Single"
           (Tag single_tag)
           single_encoding
           (function
             | Single (contract, delegate) -> Some (contract, delegate)
             | _ -> None)
           (fun (contract, delegate) -> Single (contract, delegate));
         case
           ~title:"Shared"
           (Tag shared_tag)
           shared_encoding
           (function Shared delegate -> Some delegate | _ -> None)
           (fun delegate -> Shared delegate);
       ]

let compare_staker sa sb =
  match (sa, sb) with
  | Single (ca, da), Single (cb, db) ->
      Compare.or_else (Contract_repr.compare ca cb) (fun () ->
          Signature.Public_key_hash.compare da db)
  | Shared da, Shared db -> Signature.Public_key_hash.compare da db
  | Single _, Shared _ -> -1
  | Shared _, Single _ -> 1

let staker_delegate = function
  | Single (_contract, delegate) -> delegate
  | Shared delegate -> delegate

type t = {frozen : Tez_repr.t; delegated : Tez_repr.t}

let make ~frozen ~delegated = {frozen; delegated}

let get_frozen {frozen; _} = frozen

let encoding =
  let open Data_encoding in
  conv
    (fun {frozen; delegated} -> (frozen, delegated))
    (fun (frozen, delegated) -> {frozen; delegated})
    (obj2 (req "frozen" Tez_repr.encoding) (req "delegated" Tez_repr.encoding))

let zero = make ~frozen:Tez_repr.zero ~delegated:Tez_repr.zero

let ( +? ) {frozen = f1; delegated = d1} {frozen = f2; delegated = d2} =
  let open Result_syntax in
  let* frozen = Tez_repr.(f1 +? f2) in
  let+ delegated = Tez_repr.(d1 +? d2) in
  {frozen; delegated}

module Full = struct
  type t = {
    own_frozen : Tez_repr.t;
    costaked_frozen : Tez_repr.t;
    delegated : Tez_repr.t;
  }

  let make ~own_frozen ~costaked_frozen ~delegated =
    {own_frozen; costaked_frozen; delegated}

  let zero =
    make
      ~own_frozen:Tez_repr.zero
      ~costaked_frozen:Tez_repr.zero
      ~delegated:Tez_repr.zero

  let encoding =
    let open Data_encoding in
    conv
      (fun {own_frozen; costaked_frozen; delegated} ->
        (own_frozen, costaked_frozen, delegated))
      (fun (own_frozen, costaked_frozen, delegated) ->
        {own_frozen; costaked_frozen; delegated})
      (obj3
         (req "own_frozen" Tez_repr.encoding)
         (req "costaked_frozen" Tez_repr.encoding)
         (req "delegated" Tez_repr.encoding))
end
