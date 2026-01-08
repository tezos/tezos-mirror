(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type 'a t =
  | Operating : Signature.public_key_hash t
  | Batching : Signature.public_key_hash list t
  | Cementing : Signature.public_key_hash t
  | Recovering : Signature.public_key_hash t
  | Executing_outbox : Signature.public_key_hash t

type ex_purpose = Purpose : 'a t -> ex_purpose

let all =
  [
    Purpose Operating;
    Purpose Batching;
    Purpose Cementing;
    Purpose Recovering;
    Purpose Executing_outbox;
  ]

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6641

   replace this map by a Gmap one so the gadt is exposed in the map
   type.
*)
module Map = Map.Make (struct
  type nonrec t = ex_purpose

  let compare = Stdlib.compare
end)

type 'a operator =
  | Single : Signature.public_key_hash -> Signature.public_key_hash operator
  | Multiple :
      Signature.public_key_hash list
      -> Signature.public_key_hash list operator

type ex_operator = Operator : 'a operator -> ex_operator

type operators = ex_operator Map.t

let no_operators = Map.empty

let to_string (type kind) : kind t -> string = function
  | Operating -> "operating"
  | Batching -> "batching"
  | Cementing -> "cementing"
  | Recovering -> "recovering"
  | Executing_outbox -> "executing_outbox"

let to_string_ex_purpose (Purpose purpose) = to_string purpose

let pp fmt purpose = Format.pp_print_string fmt (to_string purpose)

let pp_ex_purpose fmt (Purpose purpose) = pp fmt purpose

let encoding =
  Data_encoding.string_enum
  @@ List.map (fun p -> (to_string_ex_purpose p, p)) all

module Set = struct
  include Set.Make (struct
    type nonrec t = ex_purpose

    let compare = Stdlib.compare
  end)

  let pp fmt s =
    Format.pp_print_seq
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      pp_ex_purpose
      fmt
      (to_seq s)

  let encoding =
    let open Data_encoding in
    conv
      (fun s -> to_seq s |> List.of_seq)
      (fun l -> List.to_seq l |> of_seq)
      (list encoding)
end

type error += Missing_operators of Set.t

let of_string_ex_purpose = function
  (* For backward compability:
     "publish", "refute", "timeout" -> Operating
     "add_messages" -> Batching
     "cement" -> Cementing
  *)
  | "operating" | "publish" | "refute" | "timeout" -> Some (Purpose Operating)
  | "batching" | "add_messages" -> Some (Purpose Batching)
  | "cementing" | "cement" -> Some (Purpose Cementing)
  | "recovering" -> Some (Purpose Recovering)
  | "executing_outbox" -> Some (Purpose Executing_outbox)
  | _ -> None

let of_string_exn_ex_purpose s =
  match of_string_ex_purpose s with
  | Some p -> p
  | None -> invalid_arg ("purpose_of_string " ^ s)

let ex_operator_encoding =
  let open Data_encoding in
  let single_case =
    case
      ~title:"Signature.public_key_hash operator"
      (Tag 0)
      Signature.Public_key_hash.encoding
      (function
        | Operator (Single pkh) -> Some pkh | Operator (Multiple _) -> None)
      (fun pkh -> Operator (Single pkh))
  in
  let multiple_case =
    case
      ~title:"multiple operator"
      (Tag 1)
      (list Signature.Public_key_hash.encoding)
      (function
        | Operator (Multiple pkhs) -> Some pkhs | Operator (Single _) -> None)
      (fun pkhs -> Operator (Multiple pkhs))
  in
  union [single_case; multiple_case]

let operators_encoding =
  let open Data_encoding in
  conv
    Map.bindings
    (fun l -> List.to_seq l |> Map.of_seq)
    (Dictionary_encoding.dictionary_encoding
       ~keys:all
       ~string_of_key:to_string_ex_purpose
       ~key_of_string:of_string_exn_ex_purpose
       ~value_encoding:(fun _ -> ex_operator_encoding))

let () =
  register_error_kind
    ~id:"sc_rollup.node.missing_operators"
    ~title:"Missing operators for the chosen mode"
    ~description:"Missing operators for the chosen mode."
    ~pp:(fun ppf missing_purposes ->
      Format.fprintf
        ppf
        "@[<hov>Missing operators for purposes %a.@]"
        Set.pp
        missing_purposes)
    `Permanent
    Data_encoding.(obj1 (req "missing_purposes" Set.encoding))
    (function Missing_operators p -> Some p | _ -> None)
    (fun p -> Missing_operators p)

module Event = struct
  include Internal_event.Simple

  let section = ["smart_rollup_node"; "configuration"]

  let unused_operators =
    declare_1
      ~section
      ~name:"smart_rollup_node_config_unused_operators"
      ~msg:"[Warning]: Unused operators for {extra_purposes}."
      ~level:Warning
      ("extra_purposes", Set.encoding)
      ~pp1:Set.pp
end

(* For each purpose, it returns a list of associated operation kinds *)
let operation_kind : ex_purpose -> Operation_kind.t list = function
  | Purpose Batching -> [Add_messages; Publish_dal_commitment]
  | Purpose Cementing -> [Cement]
  | Purpose Operating -> [Publish; Refute; Timeout]
  | Purpose Recovering -> [Recover]
  | Purpose Executing_outbox -> [Execute_outbox_message]

(* Maps a list of operation kinds to their corresponding purposes,
   based on their presence in the input list. *)
let of_operation_kind (operation_kinds : Operation_kind.t list) :
    ex_purpose list =
  List.filter
    (fun purpose ->
      let expected_operation_kinds = operation_kind purpose in
      List.exists
        (fun kind -> List.mem ~equal:Stdlib.( = ) kind expected_operation_kinds)
        operation_kinds)
    all

let new_operator_for_purpose : type kind.
    kind t -> Signature.public_key_hash list -> kind operator =
 fun purpose pkh ->
  match (purpose, pkh) with
  | _, [] -> invalid_arg "no public key hash given for the new operator"
  | Operating, [pkh] -> Single pkh
  | Batching, pkhs -> Multiple pkhs
  | Cementing, [pkh] -> Single pkh
  | Recovering, [pkh] -> Single pkh
  | Executing_outbox, [pkh] -> Single pkh
  | _, _ -> invalid_arg "multiple keys for a purpose allowing only one key"

let update : type kind.
    kind t ->
    Signature.public_key_hash list ->
    ex_operator option ->
    ex_operator option =
 fun purpose pkh_to_add -> function
  | Some (Operator (Multiple pkhs)) ->
      Some (Operator (Multiple (pkh_to_add @ pkhs)))
  | Some (Operator (Single _)) ->
      invalid_arg "multiple keys for a purpose allowing only one key"
  | None -> Some (Operator (new_operator_for_purpose purpose pkh_to_add))

let replace_operators ?default_operator ~needed_purposes
    (purposed_key : (ex_purpose * Signature.public_key_hash) list) operators =
  let open Lwt_result_syntax in
  let replacement_map =
    List.fold_left
      (fun map ((Purpose purpose as ex_purpose), key) ->
        Map.update ex_purpose (update purpose [key]) map)
      Map.empty
      purposed_key
  in
  let needed_purposes = Set.of_list needed_purposes in
  let given_purposes =
    Map.bindings replacement_map |> List.map fst |> Set.of_list
  in
  let operators, missing, extra =
    Set.fold
      (fun (Purpose p as purpose) (acc, missing, extra) ->
        (* replacement_operator > default_operator > existing_operator *)
        let op =
          match Map.find purpose replacement_map with
          | Some op -> Some op
          | None -> (
              match default_operator with
              | Some pkh -> Some (Operator (new_operator_for_purpose p [pkh]))
              | None -> (
                  match Map.find purpose operators with
                  | Some op -> Some op
                  | None -> None))
        in
        match op with
        | Some op -> (Map.add purpose op acc, missing, Set.remove purpose extra)
        | None -> (acc, Set.add purpose missing, extra))
      needed_purposes
      (Map.empty, Set.empty, given_purposes)
  in
  let*? () = error_unless (Set.is_empty missing) (Missing_operators missing) in
  let*! () =
    if not (Set.is_empty extra) then Event.(emit unused_operators) extra
    else Lwt.return_unit
  in
  return operators

(* ~default:pkh1 ~needed_purpose:[operating,batching] [batching,pkh2;
   batching, pkh3] *)
let make_operators ?default_operator ~needed_purposes purposed_key =
  replace_operators ?default_operator ~needed_purposes purposed_key Map.empty

let single_operator : ex_operator -> Signature.public_key_hash operator =
  function
  (* this case can't happens because we are in control of the map with
     this interface and we don't allow such case. *)
  | Operator (Multiple _) -> assert false
  | Operator (Single pkh) -> Single pkh

let multiple_operator : ex_operator -> Signature.Public_key_hash.t list operator
    = function
  | Operator (Multiple pkhs) -> Multiple pkhs
  | Operator (Single _pkh) -> assert false

let find_operator : type kind. kind t -> operators -> kind operator option =
 fun purpose operator_per_purpose ->
  let operator = Map.find (Purpose purpose) operator_per_purpose in
  match purpose with
  | Batching -> Option.map multiple_operator operator
  | Operating -> Option.map single_operator operator
  | Cementing -> Option.map single_operator operator
  | Recovering -> Option.map single_operator operator
  | Executing_outbox -> Option.map single_operator operator

let mem_operator pkh operator_per_purpose =
  Map.exists
    (fun _ op ->
      match op with
      | Operator (Single operator) -> Signature.Public_key_hash.(operator = pkh)
      | Operator (Multiple operator) ->
          List.mem ~equal:Signature.Public_key_hash.equal pkh operator)
    operator_per_purpose

let operators_bindings = Map.bindings
