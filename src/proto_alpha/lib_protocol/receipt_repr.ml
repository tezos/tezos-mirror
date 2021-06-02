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
  | Rewards of Signature.Public_key_hash.t * Cycle_repr.t
  | Fees of Signature.Public_key_hash.t * Cycle_repr.t
  | Deposits of Signature.Public_key_hash.t * Cycle_repr.t

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
           ~title:"Rewards"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "rewards"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function Rewards (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Rewards (d, l));
         case
           (Tag 2)
           ~title:"Fees"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "fees"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function Fees (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Fees (d, l));
         case
           (Tag 3)
           ~title:"Deposits"
           (obj4
              (req "kind" (constant "freezer"))
              (req "category" (constant "deposits"))
              (req "delegate" Signature.Public_key_hash.encoding)
              (req "cycle" Cycle_repr.encoding))
           (function Deposits (d, l) -> Some ((), (), d, l) | _ -> None)
           (fun ((), (), d, l) -> Deposits (d, l));
       ]

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
                 | None -> failwith "Qty.of_mutez"
               else
                 match Tez_repr.of_mutez v with
                 | Some v -> Credited v
                 | None -> failwith "Qty.of_mutez" )
             int64))

type update_origin = Block_application | Protocol_migration | Subsidy

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

let cleanup_balance_updates balance_updates =
  List.filter
    (fun (_, (Credited update | Debited update), _) ->
      not (Tez_repr.equal update Tez_repr.zero))
    balance_updates
