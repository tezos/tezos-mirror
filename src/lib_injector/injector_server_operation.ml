(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {entrypoint : string; value : string}

type t =
  | Transaction of {
      amount : int64;
      destination : string;
      parameters : parameters option;
    }

(* adapted from Operation_repr.Encoding.Manager_operations *)
let encoding : t Data_encoding.t =
  let open Data_encoding in
  let case tag kind encoding proj inj =
    case
      ~title:kind
      (Tag tag)
      (merge_objs (obj1 (req "kind" (constant kind))) encoding)
      (fun o -> Option.map (fun p -> ((), p)) (proj o))
      (fun ((), p) -> inj p)
  in
  def "injector_operation"
  @@ union
       [
         case
           0
           "transaction"
           (obj3
              (req "amount" string)
              (req "destination" string)
              (opt
                 "parameters"
                 (obj2 (req "entrypoint" string) (req "value" string))))
           (function
             | Transaction {amount; destination; parameters} ->
                 Some
                   ( Int64.to_string amount,
                     destination,
                     Option.map
                       (fun {entrypoint; value} -> (entrypoint, value))
                       parameters ))
           (fun (amount, destination, parameters) ->
             Transaction
               {
                 amount = Int64.of_string amount;
                 destination;
                 parameters =
                   Option.map
                     (fun (entrypoint, value) -> {entrypoint; value})
                     parameters;
               });
       ]

let pp ppf = function
  | Transaction {amount; destination = _; parameters = _} ->
      Format.fprintf ppf "Transaction of %Ld tez" amount

let unique = function Transaction _ -> true
