(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Balance_update = struct
  type category =
    | Block_fees
    | Baking_rewards
    | Baking_bonuses
    | Attestation_rewards
    | Dal_attestation_rewards

  let category_encoding =
    let open Data_encoding in
    def "tezindex.category"
    @@ union
         [
           case
             (Tag 0)
             ~title:"Block_fees"
             unit
             (function Block_fees -> Some () | _ -> None)
             (fun () -> Block_fees);
           case
             (Tag 1)
             ~title:"Baking_rewards"
             unit
             (function Baking_rewards -> Some () | _ -> None)
             (fun () -> Baking_rewards);
           case
             (Tag 2)
             ~title:"Baking_bonuses"
             unit
             (function Baking_bonuses -> Some () | _ -> None)
             (fun () -> Baking_bonuses);
           case
             (Tag 3)
             ~title:"Attestation_rewards"
             unit
             (function Attestation_rewards -> Some () | _ -> None)
             (fun () -> Attestation_rewards);
           case
             (Tag 4)
             ~title:"Dal_attestation_rewards"
             unit
             (function Dal_attestation_rewards -> Some () | _ -> None)
             (fun () -> Dal_attestation_rewards);
         ]

  type result =
    | Lost
    | Contract
    | Delegate
    | Baker_own_stake
    | Baker_edge
    | Staker

  let result_encoding =
    let open Data_encoding in
    def "tezindex.result"
    @@ union
         [
           case
             (Tag 0)
             ~title:"Lost"
             unit
             (function Lost -> Some () | _ -> None)
             (fun () -> Lost);
           case
             (Tag 1)
             ~title:"Contract"
             unit
             (function Contract -> Some () | _ -> None)
             (fun () -> Contract);
           case
             (Tag 2)
             ~title:"Delegate"
             unit
             (function Delegate -> Some () | _ -> None)
             (fun () -> Delegate);
           case
             (Tag 3)
             ~title:"Baker_own_stake"
             unit
             (function Baker_own_stake -> Some () | _ -> None)
             (fun () -> Baker_own_stake);
           case
             (Tag 4)
             ~title:"Baker_edge"
             unit
             (function Baker_edge -> Some () | _ -> None)
             (fun () -> Baker_edge);
           case
             (Tag 5)
             ~title:"Staker"
             unit
             (function Staker -> Some () | _ -> None)
             (fun () -> Staker);
         ]

  type balance_update = {
    address : Signature.public_key_hash;
    category : category;
    result : result;
    value : int64;
  }

  let balance_update_encoding =
    let open Data_encoding in
    conv
      (fun {address; category; result; value} ->
        (address, category, result, value))
      (fun (address, category, result, value) ->
        {address; category; result; value})
      (obj4
         (req "address" Signature.Public_key_hash.encoding)
         (req "category" category_encoding)
         (req "result" result_encoding)
         (req "value" int64))

  let balance_updates_encoding = Data_encoding.list balance_update_encoding

  let category_to_string = function
    | Block_fees -> "Block_fees"
    | Baking_rewards -> "Baking_rewards"
    | Baking_bonuses -> "Baking_bonuses"
    | Attestation_rewards -> "Attestation_rewards"
    | Dal_attestation_rewards -> "Dal_attestation_rewards"

  let category_of_string = function
    | "Block_fees" -> Some Block_fees
    | "Baking_rewards" -> Some Baking_rewards
    | "Baking_bonuses" -> Some Baking_bonuses
    | "Attestation_rewards" -> Some Attestation_rewards
    | "Dal_attestation_rewards" -> Some Dal_attestation_rewards
    | _ -> None

  let pp_category fmt category =
    Format.fprintf fmt "%s" (category_to_string category)

  let result_to_string = function
    | Lost -> "Lost"
    | Contract -> "Contract"
    | Delegate -> "Delegate"
    | Baker_own_stake -> "Baker_own_stake"
    | Baker_edge -> "Baker_edge"
    | Staker -> "Staker"

  let result_of_string = function
    | "Lost" -> Some Lost
    | "Contract" -> Some Contract
    | "Delegate" -> Some Delegate
    | "Baker_own_stake" -> Some Baker_own_stake
    | "Baker_edge" -> Some Baker_edge
    | "Staker" -> Some Staker
    | _ -> None

  let pp_result fmt result = Format.fprintf fmt "%s" (result_to_string result)

  let pp_balance_update fmt {address; category; result; value} =
    Format.fprintf
      fmt
      " Address: %a,@, Category: %a,@, Result: %a,@, Value: %Ld"
      Tezos_crypto.Signature.Public_key_hash.pp_short
      address
      pp_category
      category
      pp_result
      result
      value
end
