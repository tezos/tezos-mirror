(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Request = struct
  type ('a, 'b) t =
    | New_rollup_node_block : Int32.t -> (unit, error trace) t
    | Apply_evm_events : Int32.t -> (unit, error trace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"New_rollup_node_block"
          (obj2
             (req "request" (constant "new_rollup_node_block"))
             (req "rollup_head" int32))
          (function
            | View (New_rollup_node_block rollup_head) -> Some ((), rollup_head)
            | _ -> None)
          (fun ((), rollup_head) -> View (New_rollup_node_block rollup_head));
        case
          (Tag 1)
          ~title:"Apply_evm_events"
          (obj2
             (req "request" (constant "apply_evm_events"))
             (req "rollup_lvl" int32))
          (function
            | View (Apply_evm_events rollup_lvl) -> Some ((), rollup_lvl)
            | _ -> None)
          (fun ((), rollup_head) -> View (Apply_evm_events rollup_head));
      ]

  let pp ppf (View r) =
    match r with
    | New_rollup_node_block rollup_head ->
        Format.fprintf ppf "New_rollup_node_block (level %ld)" rollup_head
    | Apply_evm_events rollup_lvl ->
        Format.fprintf ppf "Apply_evm_events (level %ld)" rollup_lvl
end
