(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Request = struct
  type payload =
    | Blueprint of Sequencer_blueprint.t list
    | Inbox of Blueprint_types.payload

  type ('a, 'b) t =
    | Publish : {level : Z.t; payload : payload} -> (unit, error trace) t
    | New_rollup_node_block : int32 -> (unit, error trace) t

  type view = View : _ t -> view

  let view req = View req

  let publish_payload_encoding =
    let open Data_encoding in
    let external_encoding =
      conv (function `External msg -> msg) (fun msg -> `External msg) string
    in
    union
      [
        case
          (Tag 0)
          ~title:"Blueprint"
          (list (dynamic_size Sequencer_blueprint.chunk_encoding))
          (function Blueprint chunks -> Some chunks | _ -> None)
          (fun chunks -> Blueprint chunks);
        case
          (Tag 1)
          ~title:"Inbox"
          (list external_encoding)
          (function Inbox payload -> Some payload | _ -> None)
          (fun payload -> Inbox payload);
      ]

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Publish"
          (obj3
             (req "request" (constant "publish"))
             (req "level" n)
             (req "payload" publish_payload_encoding))
          (function
            | View (Publish {level; payload}) -> Some ((), level, payload)
            | _ -> None)
          (fun ((), level, payload) -> View (Publish {level; payload}));
        case
          (Tag 1)
          ~title:"New_rollup_node_block"
          (obj2
             (req "request" (constant "new_l2_head"))
             (req "rollup_head" int32))
          (function
            | View (New_rollup_node_block rollup_level) ->
                Some ((), rollup_level)
            | _ -> None)
          (fun ((), rollup_level) -> View (New_rollup_node_block rollup_level));
      ]

  let pp ppf (View r) =
    match r with
    | Publish {level; payload = _} ->
        Format.fprintf ppf "Publish { level = %a }" Z.pp_print level
    | New_rollup_node_block rollup_level ->
        Format.fprintf ppf "New_rollup_node_block %ld" rollup_level
end
