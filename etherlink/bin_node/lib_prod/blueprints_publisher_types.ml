(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Request = struct
  type ('a, 'b) t =
    | Publish : {
        level : Z.t;
        payload : [`External of string] list;
      }
        -> (unit, error trace) t
    | New_l2_head : {rollup_head : Z.t} -> (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    let external_encoding =
      conv (function `External msg -> msg) (fun msg -> `External msg) string
    in
    union
      [
        case
          (Tag 0)
          ~title:"Publish"
          (obj3
             (req "request" (constant "publish"))
             (req "level" n)
             (req "payload" (list external_encoding)))
          (function
            | View (Publish {level; payload}) -> Some ((), level, payload)
            | _ -> None)
          (fun ((), level, payload) -> View (Publish {level; payload}));
        case
          (Tag 1)
          ~title:"New_l2_head"
          (obj2 (req "request" (constant "new_l2_head")) (req "rollup_head" n))
          (function
            | View (New_l2_head {rollup_head}) -> Some ((), rollup_head)
            | _ -> None)
          (fun ((), rollup_head) -> View (New_l2_head {rollup_head}));
      ]

  let pp ppf (View r) =
    match r with
    | Publish {level; payload = _} ->
        Format.fprintf ppf "Publish { level = %a }" Z.pp_print level
    | New_l2_head {rollup_head} ->
        Format.fprintf
          ppf
          "New_l2_head { rollup_head = %a }"
          Z.pp_print
          rollup_head
end
