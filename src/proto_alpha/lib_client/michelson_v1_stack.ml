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

type error +=
  | Wrong_stack_item of Micheline_parser.location * Micheline_printer.node
  | Wrong_stack of Micheline_parser.location * Micheline_printer.node
  | Wrong_other_contracts_item of
      Micheline_parser.location * Micheline_printer.node
  | Wrong_other_contracts of Micheline_parser.location * Micheline_printer.node
  | Wrong_extra_big_maps_item of
      Micheline_parser.location * Micheline_printer.node
  | Wrong_extra_big_maps of Micheline_parser.location * Micheline_printer.node
  | Invalid_address_for_smart_contract of string

let micheline_printer_location_encoding :
    Micheline_printer.location Data_encoding.encoding =
  let open Data_encoding in
  conv
    (fun loc -> loc.Micheline_printer.comment)
    (fun comment -> {comment})
    (option string)

let micheline_printer_node_encoding :
    Micheline_printer.node Data_encoding.encoding =
  Micheline_encoding.table_encoding
    ~variant:""
    micheline_printer_location_encoding
    Data_encoding.string

let () =
  let open Data_encoding in
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.stack.wrong_stack_item"
    ~title:"Wrong stack item"
    ~description:"Failed to parse an item in a typed stack."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for an item in a typed stack. Expected: Stack_elt \
         <ty> <value>; got %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function Wrong_stack_item (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_stack_item (loc, node)) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.stack.wrong_stack"
    ~title:"Wrong stack"
    ~description:"Failed to parse a typed stack."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for a typed stack. Expected a sequence of Stack_elt \
         <ty> <value>; got %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function Wrong_stack (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_stack (loc, node)) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_other_contracts_item"
    ~title:"Wrong description of an other contract"
    ~description:"Failed to parse an item in a description of other contracts."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for an item in a description of other contracts. \
         Expected: Contract <address> <ty>; got %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function
      | Wrong_other_contracts_item (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_other_contracts_item (loc, node)) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_other_contracts"
    ~title:"Wrong description of a list of other contracts"
    ~description:"Failed to parse a description of other contracts."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for a description of other contracts. Expected a \
         sequence of Contract <address> <ty>; got %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function
      | Wrong_other_contracts (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_other_contracts (loc, node)) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_extra_big_maps_item"
    ~title:"Wrong description of an extra big map"
    ~description:"Failed to parse an item in a description of extra big maps."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for an item in a description of extra big maps. \
         Expected: Big_map <index> <key_type> <value_type> <content>; got %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function
      | Wrong_extra_big_maps_item (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_extra_big_maps_item (loc, node)) ;
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"michelson.wrong_extra_big_maps"
    ~title:"Wrong description of a list of extra big maps"
    ~description:"Failed to parse a description of extra big maps."
    ~pp:(fun ppf (_loc, node) ->
      Format.fprintf
        ppf
        "Unexpected format for a description of extra big maps. Expected a \
         sequence of Big_map <index> <key_type> <value_type> <content>; got \
         %a."
        Micheline_printer.print_expr_unwrapped
        node)
    (obj2
       (req "location" Micheline_parser.location_encoding)
       (req "node" micheline_printer_node_encoding))
    (function
      | Wrong_extra_big_maps (loc, node) -> Some (loc, node) | _ -> None)
    (fun (loc, node) -> Wrong_extra_big_maps (loc, node)) ;
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

let parse_expression ~source (node : Micheline_parser.node) :
    Script.expr tzresult =
  let open Result_syntax in
  let parsing_result = Michelson_v1_parser.expand_all ~source ~original:node in
  let* parsed = Micheline_parser.no_parsing_error parsing_result in
  return parsed.expanded

let printable node =
  Micheline_printer.printable Fun.id (Micheline.strip_locations node)

let parse_stack_item ~source =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Stack_elt", [ty; v], _annot) ->
      let* ty = parse_expression ~source ty in
      let* v = parse_expression ~source v in
      return (ty, v)
  | e -> tzfail (Wrong_stack_item (Micheline.location e, printable e))

let parse_other_contract_item ~source =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Contract", [address; ty], _annot) as e ->
      let* address = parse_expression ~source address in
      let* address =
        match Micheline.root address with
        | Micheline.String (_loc, s) -> (
            match Environment.Base58.decode s with
            | Some (Contract_hash.Data h) -> return h
            | Some _ | None -> tzfail (Invalid_address_for_smart_contract s))
        | _ ->
            tzfail
              (Wrong_other_contracts_item (Micheline.location e, printable e))
      in
      let* ty = parse_expression ~source ty in
      return RPC.Scripts.S.{address; ty}
  | e -> tzfail (Wrong_other_contracts_item (Micheline.location e, printable e))

let parse_extra_big_map_item ~source =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Big_map", [id; kty; vty; items], _annot) as e ->
      let* id = parse_expression ~source id in
      let* id =
        match Micheline.root id with
        | Micheline.Int (_loc, id) -> return (Big_map.Id.parse_z id)
        | _ ->
            tzfail
              (Wrong_other_contracts_item (Micheline.location e, printable e))
      in
      let* kty = parse_expression ~source kty in
      let* vty = parse_expression ~source vty in
      let* items = parse_expression ~source items in
      return RPC.Scripts.S.{id; kty; vty; items}
  | e -> tzfail (Wrong_extra_big_maps_item (Micheline.location e, printable e))

let parse_stack ~source = function
  | Micheline.Seq (loc, l) as e ->
      record_trace_eval (fun () -> Wrong_stack (loc, printable e))
      @@ List.map_e (parse_stack_item ~source) l
  | e -> Result_syntax.tzfail (Wrong_stack (Micheline.location e, printable e))

let parse_other_contracts ~source = function
  | Micheline.Seq (loc, l) as e ->
      record_trace_eval (fun () -> Wrong_other_contracts (loc, printable e))
      @@ List.map_e (parse_other_contract_item ~source) l
  | e ->
      Result_syntax.tzfail
        (Wrong_other_contracts (Micheline.location e, printable e))

let parse_extra_big_maps ~source = function
  | Micheline.Seq (loc, l) as e ->
      record_trace_eval (fun () -> Wrong_extra_big_maps (loc, printable e))
      @@ List.map_e (parse_extra_big_map_item ~source) l
  | e ->
      Result_syntax.tzfail
        (Wrong_extra_big_maps (Micheline.location e, printable e))
