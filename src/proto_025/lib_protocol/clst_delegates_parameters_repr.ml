(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  edge_of_clst_staking_over_baking_millionth : int32;
  ratio_of_clst_staking_over_direct_staking_billionth : int32;
}

type update = Update of t | Unregister

let maximum_ratio_of_clst_staking_over_direct_staking_billionth =
  (* max is 20% (200_000_000) *)
  200_000_000l

let default_edge_of_clst_staking_over_baking_millionth =
  (* max is 10% (100_000) *)
  100_000l

let maximum_edge_of_clst_staking_over_baking_millionth = 1_000_000l

let default =
  {
    edge_of_clst_staking_over_baking_millionth =
      default_edge_of_clst_staking_over_baking_millionth;
    ratio_of_clst_staking_over_direct_staking_billionth =
      maximum_ratio_of_clst_staking_over_direct_staking_billionth;
  }

type error += Invalid_clst_delegates_parameters

let () =
  register_error_kind
    `Permanent
    ~id:"operations.invalid_clst_delegates_parameters"
    ~title:"Invalid parameters for CLST delegates parameters"
    ~description:"The CLST delegates parameters are invalid."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid CLST delegates parameters")
    Data_encoding.empty
    (function Invalid_clst_delegates_parameters -> Some () | _ -> None)
    (fun () -> Invalid_clst_delegates_parameters)

let make ~edge_of_clst_staking_over_baking_millionth
    ~ratio_of_clst_staking_over_direct_staking_billionth =
  let check_range value minimum maximum =
    Compare.Int32.(value >= minimum && value <= maximum)
  in
  if
    not
      (check_range
         edge_of_clst_staking_over_baking_millionth
         0l
         maximum_edge_of_clst_staking_over_baking_millionth
      && check_range
           ratio_of_clst_staking_over_direct_staking_billionth
           0l
           maximum_ratio_of_clst_staking_over_direct_staking_billionth)
  then Result_syntax.tzfail Invalid_clst_delegates_parameters
  else
    Ok
      {
        edge_of_clst_staking_over_baking_millionth;
        ratio_of_clst_staking_over_direct_staking_billionth;
      }

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun {
           edge_of_clst_staking_over_baking_millionth;
           ratio_of_clst_staking_over_direct_staking_billionth;
         }
       ->
      ( edge_of_clst_staking_over_baking_millionth,
        ratio_of_clst_staking_over_direct_staking_billionth ))
    (fun ( edge_of_clst_staking_over_baking_millionth,
           ratio_of_clst_staking_over_direct_staking_billionth )
       ->
      Result.map_error
        (fun _ -> "Invalid staking parameters")
        (make
           ~edge_of_clst_staking_over_baking_millionth
           ~ratio_of_clst_staking_over_direct_staking_billionth))
    (obj2
       (req "edge_of_clst_staking_over_baking_millionth" int32)
       (req "ratio_of_clst_staking_over_direct_staking_billionth" int32))

let update_encoding =
  let open Data_encoding in
  (* The `update` type being isomorphic to option, we reuse the `option`
     encoding for the binary case. However, we use a custom encoding in JSON, as
     it will provide more information for the tooling. *)
  let json_encoding =
    union
      [
        case
          ~title:"update"
          (Tag 0)
          (obj2 (req "kind" (constant "update")) (req "parameters" encoding))
          (function Update p -> Some ((), p) | Unregister -> None)
          (fun ((), p) -> Update p);
        case
          ~title:"unregister"
          (Tag 1)
          (obj1 (req "kind" (constant "unregister")))
          (function Unregister -> Some () | Update _ -> None)
          (fun () -> Unregister);
      ]
  in
  let binary_encoding =
    conv
      (function Update p -> Some p | Unregister -> None)
      (function Some p -> Update p | None -> Unregister)
      (option encoding)
  in
  splitted ~json:json_encoding ~binary:binary_encoding
