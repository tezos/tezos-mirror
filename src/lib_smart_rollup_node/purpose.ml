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

let pp fmt purpose = Format.pp_print_string fmt (to_string purpose)

let encoding =
  Data_encoding.string_enum @@ List.map (fun p -> (to_string p, p)) all

type error +=
  | Missing_operator of t
  | Too_many_operators of {
      expected_purposes : t list;
      given_operators : operators;
    }

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

let pp_operators fmt operators =
  Format.pp_print_list
    (fun fmt (purpose, operator) ->
      Format.fprintf
        fmt
        "%a: %a"
        pp
        purpose
        Signature.Public_key_hash.pp
        operator)
    fmt
    (Map.bindings operators)

let () =
  register_error_kind
    ~id:"sc_rollup.node.missing_mode_operator"
    ~title:"Missing operator for the chosen mode"
    ~description:"Missing operator for the chosen mode."
    ~pp:(fun ppf missing_purpose ->
      Format.fprintf
        ppf
        "@[<hov>Missing operator for the purpose of %a.@]"
        pp
        missing_purpose)
    `Permanent
    Data_encoding.(obj1 (req "missing_purpose" encoding))
    (function
      | Missing_operator missing_purpose -> Some missing_purpose | _ -> None)
    (fun missing_purpose -> Missing_operator missing_purpose) ;
  register_error_kind
    ~id:"sc_rollup.node.too_many_operators"
    ~title:"Too many operators for the chosen mode"
    ~description:"Too many operators for the chosen mode."
    ~pp:(fun ppf (expected_purposes, given_operators) ->
      Format.fprintf
        ppf
        "@[<hov>Too many operators, expecting operators for only %a, have %a.@]"
        (Format.pp_print_list pp)
        expected_purposes
        pp_operators
        given_operators)
    `Permanent
    Data_encoding.(
      obj2
        (req "expected_purposes" (list encoding))
        (req "given_operators" operators_encoding))
    (function
      | Too_many_operators {expected_purposes; given_operators} ->
          Some (expected_purposes, given_operators)
      | _ -> None)
    (fun (expected_purposes, given_operators) ->
      Too_many_operators {expected_purposes; given_operators})

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

let make_operator ?default_operator ~needed_purposes purposed_key =
  let open Result_syntax in
  List.fold_left_e
    (fun map purpose ->
      let purposed_key = List.assq purpose purposed_key in
      (* first the purpose then if none default *)
      let purposed_key = Option.either purposed_key default_operator in
      let+ operator =
        Option.value_e
          purposed_key
          ~error:(TzTrace.make (Missing_operator purpose))
      in
      Map.add purpose operator map)
    Map.empty
    needed_purposes

let replace_operator ?default_operator ~needed_purposes
    (purposed_key : (t * Signature.public_key_hash) list) operators =
  let open Result_syntax in
  let replacement_map =
    List.fold_left
      (fun map (purpose, key) ->
        Map.update
          purpose
          (function
            | Some _key -> invalid_arg "mutiple keys for the same purpose"
            | None -> Some key)
          map)
      Map.empty
      purposed_key
  in
  let operators =
    Map.merge
      (fun _purpose replacement_operator existing_operator ->
        (* replacement_operator > default_operator > existing_operator *)
        match (replacement_operator, existing_operator) with
        | Some replacement_operator, _ -> Some replacement_operator
        | _, existing_operator ->
            Option.either default_operator existing_operator)
      replacement_map
      operators
  in
  let map_size = Map.cardinal operators in
  let needed_purpose_len = List.length needed_purposes in
  let* () =
    error_when
      (map_size <> needed_purpose_len)
      (Too_many_operators
         {expected_purposes = needed_purposes; given_operators = operators})
  in
  return operators
