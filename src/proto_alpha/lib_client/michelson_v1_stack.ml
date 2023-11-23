(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_micheline
open Protocol
open Alpha_context

type localized_node = {
  parser_loc : Micheline_parser.location option;
  canonical_loc : Micheline.canonical_location;
  node : string Micheline.canonical;
}

let print_localized_node_location fmt localized_node =
  match localized_node.parser_loc with
  | Some parser_loc ->
      Format.fprintf
        fmt
        "%s"
        (Format.kasprintf
           String.capitalize_ascii
           "%a"
           Micheline_parser.print_location
           parser_loc)
  | None -> Format.fprintf fmt "At position %d" localized_node.canonical_loc

let print_localized_node fmt localized_node =
  Micheline_printer.print_expr_unwrapped
    fmt
    (Micheline_printer.printable Fun.id localized_node.node)

let localize_node ~(parsed : string Michelson_v1_parser.parser_result)
    (n : (Micheline.canonical_location, string) Micheline.node) : localized_node
    =
  let canonical_loc = Micheline.location n in
  let parser_loc =
    let open Option_syntax in
    let* oloc =
      List.assoc ~equal:Int.equal canonical_loc parsed.unexpansion_table
    in
    let+ ploc, _ = List.assoc ~equal:Int.equal oloc parsed.expansion_table in
    ploc
  in
  {parser_loc; canonical_loc; node = Micheline.strip_locations n}

let localized_node_encoding : localized_node Data_encoding.t =
  Data_encoding.(
    conv
      (fun {parser_loc; canonical_loc; node} ->
        (parser_loc, canonical_loc, node))
      (fun (parser_loc, canonical_loc, node) ->
        {parser_loc; canonical_loc; node})
      (obj3
         (req "parser_location" (option Micheline_parser.location_encoding))
         (req
            "canonical_location"
            Micheline_encoding.canonical_location_encoding)
         (req
            "node"
            (Micheline_encoding.canonical_encoding
               ~variant:"alpha_client"
               Data_encoding.string))))

type error +=
  | Wrong_stack_item of localized_node
  | Wrong_stack of localized_node
  | Wrong_other_contracts_item of localized_node
  | Wrong_other_contracts of localized_node
  | Wrong_extra_big_maps_item of localized_node
  | Wrong_extra_big_maps of localized_node
  | Invalid_address_for_smart_contract of string

let () =
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.stack.wrong_stack_item"
    ~title:"Wrong stack item"
    ~description:"Failed to parse an item in a typed stack."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for an item in a typed stack. Expected: \
         Stack_elt <ty> <value>; got %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_stack_item node -> Some node | _ -> None)
    (fun node -> Wrong_stack_item node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.stack.wrong_stack"
    ~title:"Wrong stack"
    ~description:"Failed to parse a typed stack."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for a typed stack. Expected a sequence of \
         Stack_elt <ty> <value>; got %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_stack node -> Some node | _ -> None)
    (fun node -> Wrong_stack node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_other_contracts_item"
    ~title:"Wrong description of an other contract"
    ~description:"Failed to parse an item in a description of other contracts."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for an item in a description of other \
         contracts. Expected: Contract <address> <ty>; got %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_other_contracts_item node -> Some node | _ -> None)
    (fun node -> Wrong_other_contracts_item node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_other_contracts"
    ~title:"Wrong description of a list of other contracts"
    ~description:"Failed to parse a description of other contracts."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for a description of other contracts. Expected \
         a sequence of Contract <address> <ty>; got %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_other_contracts node -> Some node | _ -> None)
    (fun node -> Wrong_other_contracts node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_extra_big_maps_item"
    ~title:"Wrong description of an extra big map"
    ~description:"Failed to parse an item in a description of extra big maps."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for an item in a description of extra big \
         maps. Expected: Big_map <index> <key_type> <value_type> <content>; \
         got %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_extra_big_maps_item node -> Some node | _ -> None)
    (fun node -> Wrong_extra_big_maps_item node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_extra_big_maps"
    ~title:"Wrong description of a list of extra big maps"
    ~description:"Failed to parse a description of extra big maps."
    ~pp:(fun ppf node ->
      Format.fprintf
        ppf
        "%a,@ Unexpected format for a description of extra big maps. Expected \
         a sequence of Big_map <index> <key_type> <value_type> <content>; got \
         %a."
        print_localized_node_location
        node
        print_localized_node
        node)
    localized_node_encoding
    (function Wrong_extra_big_maps node -> Some node | _ -> None)
    (fun node -> Wrong_extra_big_maps node) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"InvalidAddressForSmartContract"
    ~title:"Invalid address for smart contract"
    ~description:
      "Invalid input, expected a smart contract address in base58 check \
       notation (KT1...)"
    Data_encoding.(obj1 (req "invalid_address" string))
    ~pp:(fun ppf literal ->
      Format.fprintf
        ppf
        "Bad argument value for a smart contract address. Expected an address \
         in base58 checked notation starting with 'KT1', but given '%s'"
        literal)
    (function Invalid_address_for_smart_contract str -> Some str | _ -> None)
    (fun str -> Invalid_address_for_smart_contract str)

let parse_expression (node : (_, string) Micheline.node) =
  Environment.wrap_tzresult
  @@ Michelson_v1_primitives.prims_of_strings (Micheline.strip_locations node)

let parse_stack_item ~parsed =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Stack_elt", [ty; v], _annot) ->
      let* ty = parse_expression ty in
      let* v = parse_expression v in
      return (ty, v)
  | e -> tzfail (Wrong_stack_item (localize_node ~parsed e))

let parse_other_contract_item ~parsed =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Contract", [address; ty], _annot) as e ->
      let* address = parse_expression address in
      let* address =
        match Micheline.root address with
        | Micheline.String (_loc, s) -> (
            match Environment.Base58.decode s with
            | Some (Contract_hash.Data h) -> return h
            | Some _ | None -> tzfail (Invalid_address_for_smart_contract s))
        | _ -> tzfail (Wrong_other_contracts_item (localize_node ~parsed e))
      in
      let* ty = parse_expression ty in
      return RPC.Scripts.S.{address; ty}
  | e -> tzfail (Wrong_other_contracts_item (localize_node ~parsed e))

let parse_extra_big_map_item ~parsed =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Big_map", [id; kty; vty; items], _annot) as e ->
      let* id = parse_expression id in
      let* id =
        match Micheline.root id with
        | Micheline.Int (_loc, id) -> return (Big_map.Id.parse_z id)
        | _ -> tzfail (Wrong_other_contracts_item (localize_node ~parsed e))
      in
      let* kty = parse_expression kty in
      let* vty = parse_expression vty in
      let* items = parse_expression items in
      return RPC.Scripts.S.{id; kty; vty; items}
  | e -> tzfail (Wrong_extra_big_maps_item (localize_node ~parsed e))

let parse_sequence ?node ~(parsed : string Michelson_v1_parser.parser_result)
    ~error parse_item =
  let node = Option.value ~default:(Micheline.root parsed.expanded) node in
  let error () = error (localize_node ~parsed node) in
  match node with
  | Micheline.Seq (_loc, l) ->
      record_trace_eval error @@ List.map_e (parse_item ~parsed) l
  | _ -> Result_syntax.tzfail (error ())

let parse_stack ?node parsed =
  parse_sequence
    ?node
    ~parsed
    ~error:(fun node -> Wrong_stack node)
    parse_stack_item

let parse_other_contracts ?node parsed =
  parse_sequence
    ?node
    ~parsed
    ~error:(fun node -> Wrong_other_contracts node)
    parse_other_contract_item

let parse_extra_big_maps ?node parsed =
  parse_sequence
    ?node
    ~parsed
    ~error:(fun node -> Wrong_extra_big_maps node)
    parse_extra_big_map_item
