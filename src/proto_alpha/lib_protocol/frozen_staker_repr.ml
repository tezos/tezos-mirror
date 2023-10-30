(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Single of {staker : Contract_repr.t; delegate : Signature.public_key_hash}
  | Shared of {delegate : Signature.public_key_hash}

let baker pkh = Single {staker = Contract_repr.Implicit pkh; delegate = pkh}

let single ~staker ~delegate = Single {staker; delegate}

let shared ~delegate = Shared {delegate}

let of_staker (staker : Staker_repr.t) =
  match staker with
  | Single (staker, delegate) -> single ~staker ~delegate
  | Shared delegate -> shared ~delegate

let encoding =
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
    ~title:"frozen_staker"
    ~description:
      "Abstract notion of staker used in operation receipts for frozen \
       deposits, either a single staker or all the stakers delegating to some \
       delegate."
    "frozen_staker"
  @@ matching
       (function
         | Single {staker; delegate} ->
             matched single_tag single_encoding (staker, delegate)
         | Shared {delegate} -> matched shared_tag shared_encoding delegate)
       [
         case
           ~title:"Single"
           (Tag single_tag)
           single_encoding
           (function
             | Single {staker; delegate} -> Some (staker, delegate) | _ -> None)
           (fun (staker, delegate) -> Single {staker; delegate});
         case
           ~title:"Shared"
           (Tag shared_tag)
           shared_encoding
           (function Shared {delegate} -> Some delegate | _ -> None)
           (fun delegate -> Shared {delegate});
       ]

let compare sa sb =
  match (sa, sb) with
  | Single {staker = sa; delegate = da}, Single {staker = sb; delegate = db} ->
      Compare.or_else (Contract_repr.compare sa sb) (fun () ->
          Signature.Public_key_hash.compare da db)
  | Shared {delegate = da}, Shared {delegate = db} ->
      Signature.Public_key_hash.compare da db
  | Single _, Shared _ -> -1
  | Shared _, Single _ -> 1
