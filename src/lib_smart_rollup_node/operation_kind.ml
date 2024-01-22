(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

let all =
  [
    Publish;
    Add_messages;
    Cement;
    Timeout;
    Refute;
    Recover;
    Execute_outbox_message;
  ]

module Map = Map.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)

type fee_parameters = Injector_common.fee_parameter Map.t

let to_string = function
  | Publish -> "publish"
  | Add_messages -> "add_messages"
  | Cement -> "cement"
  | Timeout -> "timeout"
  | Refute -> "refute"
  | Recover -> "recover"
  | Execute_outbox_message -> "execute_outbox_message"

let of_string = function
  | "publish" -> Some Publish
  | "add_messages" -> Some Add_messages
  | "cement" -> Some Cement
  | "timeout" -> Some Timeout
  | "refute" -> Some Refute
  | "recover" -> Some Recover
  | "execute_outbox_message" -> Some Execute_outbox_message
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
    (Utils.dictionary_encoding
       ~keys:all
       ~string_of_key:to_string
       ~key_of_string:of_string_exn
       ~value_encoding)

let fee_parameters_encoding ~default_fee_parameter =
  map_encoding (fun (operation_kind : t) ->
      Injector_common.fee_parameter_encoding
        ~default_fee_parameter:(default_fee_parameter operation_kind))
