(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2023-2024 Marigold <contact@marigold.dev>         *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
(*                                                                           *)
(*****************************************************************************)

(** The kind of operations that can be injected by the rollup node. *)
type t =
  | Publish
  | Add_messages
  | Cement
  | Timeout
  | Refute
  | Recover
  | Execute_outbox_message
  | Publish_dal_commitment

let all =
  [
    Publish;
    Add_messages;
    Cement;
    Timeout;
    Refute;
    Recover;
    Execute_outbox_message;
    Publish_dal_commitment;
  ]

module Map = Map.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)

let to_string = function
  | Publish -> "publish"
  | Add_messages -> "add_messages"
  | Cement -> "cement"
  | Timeout -> "timeout"
  | Refute -> "refute"
  | Recover -> "recover"
  | Execute_outbox_message -> "execute_outbox_message"
  | Publish_dal_commitment -> "publish_dal_commitment"

let of_string = function
  | "publish" -> Some Publish
  | "add_messages" -> Some Add_messages
  | "cement" -> Some Cement
  | "timeout" -> Some Timeout
  | "refute" -> Some Refute
  | "recover" -> Some Recover
  | "execute_outbox_message" -> Some Execute_outbox_message
  | "publish_dal_commitment" -> Some Publish_dal_commitment
  | _ -> None

let of_string_exn s =
  match of_string s with
  | Some p -> p
  | None -> invalid_arg ("operation_kind_of_string " ^ s)

let encoding =
  Data_encoding.string_enum
  @@ List.map
       (fun operation_kind -> (to_string operation_kind, operation_kind))
       all

let map_encoding value_encoding =
  let open Data_encoding in
  conv
    Map.bindings
    (fun l -> List.to_seq l |> Map.of_seq)
    (Dictionary_encoding.dictionary_encoding
       ~keys:all
       ~string_of_key:to_string
       ~key_of_string:of_string_exn
       ~value_encoding)

let priority_order = function
  | Timeout -> 0
  | Refute -> 1
  | Publish -> 2
  | Publish_dal_commitment -> 2
  | Cement -> 2
  | Recover -> 3
  | Add_messages -> 4
  | Execute_outbox_message -> 5

let compare_priority k1 k2 = compare (priority_order k1) (priority_order k2)
