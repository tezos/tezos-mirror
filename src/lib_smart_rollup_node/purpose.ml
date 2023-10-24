(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*****************************************************************************)

type t = Operating | Batching | Cementing | Recovering | Executing_outbox

let all = [Operating; Batching; Cementing; Recovering; Executing_outbox]

module Map = Map.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)

type operators = Signature.Public_key_hash.t Map.t

let to_string = function
  | Operating -> "operating"
  | Batching -> "batching"
  | Cementing -> "cementing"
  | Recovering -> "recovering"
  | Executing_outbox -> "executing_outbox"

let of_string = function
  (* For backward compability:
     "publish", "refute", "timeout" -> Operating
     "add_messages" -> Batching
     "cement" -> Cementing
  *)
  | "operating" | "publish" | "refute" | "timeout" -> Some Operating
  | "batching" | "add_messages" -> Some Batching
  | "cementing" | "cement" -> Some Cementing
  | "recovering" -> Some Recovering
  | "executing_outbox" -> Some Executing_outbox
  | _ -> None

let of_string_exn s =
  match of_string s with
  | Some p -> p
  | None -> invalid_arg ("purpose_of_string " ^ s)

let operators_encoding =
  let open Data_encoding in
  conv
    Map.bindings
    (fun l -> List.to_seq l |> Map.of_seq)
    (Utils.dictionary_encoding
       ~keys:all
       ~string_of_key:to_string
       ~key_of_string:of_string_exn
       ~value_encoding:(fun _ -> Signature.Public_key_hash.encoding))

(* For each purpose, it returns a list of associated operation kinds *)
let operation_kind : t -> Operation_kind.t list = function
  | Batching -> [Add_messages]
  | Cementing -> [Cement]
  | Operating -> [Publish; Refute; Timeout]
  | Recovering -> [Recover]
  | Executing_outbox -> [Execute_outbox_message]

(* Maps a list of operation kinds to their corresponding purposes,
   based on their presence in the input list. *)
let of_operation_kind (operation_kinds : Operation_kind.t list) : t list =
  List.filter
    (fun purpose ->
      let expected_operation_kinds = operation_kind purpose in
      List.exists
        (fun kind -> List.mem ~equal:Stdlib.( = ) kind expected_operation_kinds)
        operation_kinds)
    all

let make_map ?default bindings =
  let map = Map.of_seq @@ List.to_seq bindings in
  match default with
  | None -> map
  | Some default ->
      List.fold_left
        (fun map purpose -> Map.update purpose (fun _ -> Some default) map)
        map
        all
