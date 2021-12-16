(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type balance =
  | Contract of Contract_repr.t
  | Legacy_rewards of Signature.Public_key_hash.t * Cycle_repr.t
  | Block_fees
  | Legacy_deposits of Signature.Public_key_hash.t * Cycle_repr.t
  | Deposits of Signature.Public_key_hash.t
  | Nonce_revelation_rewards
  | Double_signing_evidence_rewards
  | Endorsing_rewards
  | Baking_rewards
  | Baking_bonuses
  | Legacy_fees of Signature.Public_key_hash.t * Cycle_repr.t
  | Storage_fees
  | Double_signing_punishments
  | Lost_endorsing_rewards of Signature.Public_key_hash.t * bool * bool
  | Liquidity_baking_subsidies
  | Burned
  | Commitments of Blinded_public_key_hash.t
  | Bootstrap
  | Invoice
  | Initial_commitments
  | Minted

let balance_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Contract"
           (obj2
              (req "kind" (constant "contract"))
              (req "contract" Contract_repr.encoding))
           (function Contract c -> Some ((), c) | _ -> None)
           (fun ((), c) -> Contract c);
         case
           (Tag 1)
           ~title:"Legacy_rewards"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "legacy_rewards"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function Legacy_rewards (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Legacy_rewards (d, l));
         case
           (Tag 2)
           ~title:"Block_fees"
           (obj2
              (req "kind" (constant "accumulator"))
              (req "category" (constant "block fees")))
           (function Block_fees -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Block_fees);
         case
           (Tag 3)
           ~title:"Legacy_deposits"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "legacy_deposits"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function
             | Legacy_deposits (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Legacy_deposits (d, l));
         case
           (Tag 4)
           ~title:"Deposits"
           (obj3
              (req "kind" (constant "freezer"))
              (req "category" (constant "deposits"))
              (req "delegate" Signature.Public_key_hash.encoding))
           (function Deposits d -> Some ((), (), d) | _ -> None)
           (fun ((), (), d) -> Deposits d);
         case
           (Tag 5)
           ~title:"Nonce_revelation_rewards"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "nonce revelation rewards")))
           (function Nonce_revelation_rewards -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Nonce_revelation_rewards);
         case
           (Tag 6)
           ~title:"Double_signing_evidence_rewards"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "double signing evidence rewards")))
           (function
             | Double_signing_evidence_rewards -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Double_signing_evidence_rewards);
         case
           (Tag 7)
           ~title:"Endorsing_rewards"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "endorsing rewards")))
           (function Endorsing_rewards -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Endorsing_rewards);
         case
           (Tag 8)
           ~title:"Baking_rewards"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "baking rewards")))
           (function Baking_rewards -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Baking_rewards);
         case
           (Tag 9)
           ~title:"Baking_bonuses"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "baking bonuses")))
           (function Baking_bonuses -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Baking_bonuses);
         case
           (Tag 10)
           ~title:"Legacy_fees"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "legacy_fees"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function Legacy_fees (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Legacy_fees (d, l));
         case
           (Tag 11)
           ~title:"Storage_fees"
           (obj2
              (req "kind" (constant "burned"))
              (req "category" (constant "storage fees")))
           (function Storage_fees -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Storage_fees);
         case
           (Tag 12)
           ~title:"Double_signing_punishments"
           (obj2
              (req "kind" (constant "burned"))
              (req "category" (constant "punishments")))
           (function Double_signing_punishments -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Double_signing_punishments);
         case
           (Tag 13)
           ~title:"Lost_endorsing_rewards"
           (obj5
              (req "kind" (constant "burned"))
              (req "category" (constant "lost endorsing rewards"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "participation" Data_encoding.bool)
              (req "revelation" Data_encoding.bool))
           (function
             | Lost_endorsing_rewards (d, p, r) -> Some ((), (), d, p, r)
             | _ -> None)
           (fun ((), (), d, p, r) -> Lost_endorsing_rewards (d, p, r));
         case
           (Tag 14)
           ~title:"Liquidity_baking_subsidies"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "subsidy")))
           (function Liquidity_baking_subsidies -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Liquidity_baking_subsidies);
         case
           (Tag 15)
           ~title:"Burned"
           (obj2
              (req "kind" (constant "burned"))
              (req "category" (constant "burned")))
           (function Burned -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Burned);
         case
           (Tag 16)
           ~title:"Commitments"
           (obj3
              (req "kind" (constant "commitment"))
              (req "category" (constant "commitment"))
              (req "committer" Blinded_public_key_hash.encoding))
           (function Commitments bpkh -> Some ((), (), bpkh) | _ -> None)
           (fun ((), (), bpkh) -> Commitments bpkh);
         case
           (Tag 17)
           ~title:"Bootstrap"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "bootstrap")))
           (function Bootstrap -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Bootstrap);
         case
           (Tag 18)
           ~title:"Invoice"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "invoice")))
           (function Invoice -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Invoice);
         case
           (Tag 19)
           ~title:"Initial_commitments"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "commitment")))
           (function Initial_commitments -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Initial_commitments);
         case
           (Tag 20)
           ~title:"Minted"
           (obj2
              (req "kind" (constant "minted"))
              (req "category" (constant "minted")))
           (function Minted -> Some ((), ()) | _ -> None)
           (fun ((), ()) -> Minted);
       ]

let is_not_zero c = not (Compare.Int.equal c 0)

let compare_balance ba bb =
  match (ba, bb) with
  | (Contract ca, Contract cb) -> Contract_repr.compare ca cb
  | (Legacy_rewards (pkha, ca), Legacy_rewards (pkhb, cb)) ->
      let c = Signature.Public_key_hash.compare pkha pkhb in
      if is_not_zero c then c else Cycle_repr.compare ca cb
  | (Legacy_deposits (pkha, ca), Legacy_deposits (pkhb, cb)) ->
      let c = Signature.Public_key_hash.compare pkha pkhb in
      if is_not_zero c then c else Cycle_repr.compare ca cb
  | (Deposits pkha, Deposits pkhb) ->
      Signature.Public_key_hash.compare pkha pkhb
  | ( Lost_endorsing_rewards (pkha, pa, ra),
      Lost_endorsing_rewards (pkhb, pb, rb) ) ->
      let c = Signature.Public_key_hash.compare pkha pkhb in
      if is_not_zero c then c
      else
        let c = Compare.Bool.compare pa pb in
        if is_not_zero c then c else Compare.Bool.compare ra rb
  | (Commitments bpkha, Commitments bpkhb) ->
      Blinded_public_key_hash.compare bpkha bpkhb
  | (Legacy_fees (pkha, ca), Legacy_fees (pkhb, cb)) ->
      let c = Signature.Public_key_hash.compare pkha pkhb in
      if is_not_zero c then c else Cycle_repr.compare ca cb
  | (_, _) ->
      let index b =
        match b with
        | Contract _ -> 0
        | Legacy_rewards _ -> 1
        | Block_fees -> 2
        | Legacy_deposits _ -> 3
        | Deposits _ -> 4
        | Nonce_revelation_rewards -> 5
        | Double_signing_evidence_rewards -> 6
        | Endorsing_rewards -> 7
        | Baking_rewards -> 8
        | Baking_bonuses -> 9
        | Legacy_fees _ -> 10
        | Storage_fees -> 11
        | Double_signing_punishments -> 12
        | Lost_endorsing_rewards _ -> 13
        | Liquidity_baking_subsidies -> 14
        | Burned -> 15
        | Commitments _ -> 16
        | Bootstrap -> 17
        | Invoice -> 18
        | Initial_commitments -> 19
        | Minted -> 20
        (* don't forget to add parameterized cases in the first part of the function *)
      in
      Compare.Int.compare (index ba) (index bb)

type balance_update = Debited of Tez_repr.t | Credited of Tez_repr.t

let balance_update_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance_update"
  @@ obj1
       (req
          "change"
          (conv
             (function
               | Credited v -> Tez_repr.to_mutez v
               | Debited v -> Int64.neg (Tez_repr.to_mutez v))
             ( Json.wrap_error @@ fun v ->
               if Compare.Int64.(v < 0L) then
                 match Tez_repr.of_mutez (Int64.neg v) with
                 | Some v -> Debited v
                 | None -> assert false (* [of_mutez z] is [None] iff [z < 0] *)
               else
                 match Tez_repr.of_mutez v with
                 | Some v -> Credited v
                 | None -> assert false (* same *) )
             int64))

type update_origin =
  | Block_application
  | Protocol_migration
  | Subsidy
  | Simulation

let compare_update_origin oa ob =
  let index o =
    match o with
    | Block_application -> 0
    | Protocol_migration -> 1
    | Subsidy -> 2
    | Simulation -> 3
  in
  Compare.Int.compare (index oa) (index ob)

let update_origin_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.update_origin"
  @@ obj1 @@ req "origin"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Block_application"
           (constant "block")
           (function Block_application -> Some () | _ -> None)
           (fun () -> Block_application);
         case
           (Tag 1)
           ~title:"Protocol_migration"
           (constant "migration")
           (function Protocol_migration -> Some () | _ -> None)
           (fun () -> Protocol_migration);
         case
           (Tag 2)
           ~title:"Subsidy"
           (constant "subsidy")
           (function Subsidy -> Some () | _ -> None)
           (fun () -> Subsidy);
         case
           (Tag 3)
           ~title:"Simulation"
           (constant "simulation")
           (function Simulation -> Some () | _ -> None)
           (fun () -> Simulation);
       ]

type balance_updates = (balance * balance_update * update_origin) list

let balance_updates_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance_updates"
  @@ list
       (conv
          (function
            | (balance, balance_update, update_origin) ->
                ((balance, balance_update), update_origin))
          (fun ((balance, balance_update), update_origin) ->
            (balance, balance_update, update_origin))
          (merge_objs
             (merge_objs balance_encoding balance_update_encoding)
             update_origin_encoding))

module BalanceMap = Map.Make (struct
  type t = balance * update_origin

  let compare (ba, ua) (bb, ub) =
    let c = compare_balance ba bb in
    if is_not_zero c then c else compare_update_origin ua ub
end)

let group_balance_updates balance_updates =
  List.fold_left_e
    (fun acc (b, update, o) ->
      (match BalanceMap.find (b, o) acc with
      | None -> ok update
      | Some present -> (
          match (present, update) with
          | (Credited a, Debited b) | (Debited b, Credited a) ->
              if Tez_repr.(a >= b) then
                Tez_repr.(a -? b) >>? fun update -> ok (Credited update)
              else Tez_repr.(b -? a) >>? fun update -> ok (Debited update)
          | (Credited a, Credited b) ->
              Tez_repr.(a +? b) >>? fun update -> ok (Credited update)
          | (Debited a, Debited b) ->
              Tez_repr.(a +? b) >>? fun update -> ok (Debited update)))
      >>? function
      | Credited update when Tez_repr.(update = zero) ->
          ok (BalanceMap.remove (b, o) acc)
      | update -> ok (BalanceMap.add (b, o) update acc))
    BalanceMap.empty
    balance_updates
  >>? fun map ->
  ok (BalanceMap.fold (fun (b, o) u acc -> (b, u, o) :: acc) map [])
