(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Baker of Signature.public_key_hash
  | Single_staker of {
      staker : Contract_repr.t;
      delegate : Signature.public_key_hash;
    }
  | Shared_between_stakers of {delegate : Signature.public_key_hash}

let baker pkh = Baker pkh

let single_staker ~staker ~delegate =
  match (staker : Contract_repr.t) with
  | Implicit pkh when Signature.Public_key_hash.(pkh = delegate) -> Baker pkh
  | _ -> Single_staker {staker; delegate}

let shared_between_stakers ~delegate = Shared_between_stakers {delegate}

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
  let baker_tag = 2 in
  let baker_encoding = obj1 (req "baker" Signature.Public_key_hash.encoding) in
  def
    ~title:"frozen_staker"
    ~description:
      "Abstract notion of staker used in operation receipts for frozen \
       deposits, either a single staker or all the stakers delegating to some \
       delegate."
    "frozen_staker"
  @@ matching
       (function
         | Baker baker -> matched baker_tag baker_encoding baker
         | Single_staker {staker; delegate} ->
             matched single_tag single_encoding (staker, delegate)
         | Shared_between_stakers {delegate} ->
             matched shared_tag shared_encoding delegate)
       [
         case
           ~title:"Single"
           (Tag single_tag)
           single_encoding
           (function
             | Single_staker {staker; delegate} -> Some (staker, delegate)
             | _ -> None)
           (fun (staker, delegate) -> single_staker ~staker ~delegate);
         case
           ~title:"Shared"
           (Tag shared_tag)
           shared_encoding
           (function
             | Shared_between_stakers {delegate} -> Some delegate | _ -> None)
           (fun delegate -> Shared_between_stakers {delegate});
         case
           ~title:"Baker"
           (Tag baker_tag)
           baker_encoding
           (function Baker baker -> Some baker | _ -> None)
           (fun baker -> Baker baker);
       ]

let compare sa sb =
  match (sa, sb) with
  | Baker ba, Baker bb -> Signature.Public_key_hash.compare ba bb
  | Baker _, _ -> -1
  | _, Baker _ -> 1
  | ( Single_staker {staker = sa; delegate = da},
      Single_staker {staker = sb; delegate = db} ) ->
      Compare.or_else (Contract_repr.compare sa sb) (fun () ->
          Signature.Public_key_hash.compare da db)
  | ( Shared_between_stakers {delegate = da},
      Shared_between_stakers {delegate = db} ) ->
      Signature.Public_key_hash.compare da db
  | Single_staker _, Shared_between_stakers _ -> -1
  | Shared_between_stakers _, Single_staker _ -> 1
