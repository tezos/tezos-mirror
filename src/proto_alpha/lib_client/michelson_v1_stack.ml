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
    (fun (loc, node) -> Wrong_stack (loc, node))

let parse_expression ~source (node : Micheline_parser.node) :
    Script.expr tzresult =
  let open Result_syntax in
  let parsing_result = Michelson_v1_parser.expand_all ~source ~original:node in
  let+ parsed = Micheline_parser.no_parsing_error parsing_result in
  parsed.expanded

let printable node =
  Micheline_printer.printable Fun.id (Micheline.strip_locations node)

let parse_stack_item ~source =
  let open Result_syntax in
  function
  | Micheline.Prim (_loc, "Stack_elt", [ty; v], _annot) ->
      let* ty = parse_expression ~source ty in
      let+ v = parse_expression ~source v in
      (ty, v)
  | e -> error (Wrong_stack_item (Micheline.location e, printable e))

let parse_stack ~source = function
  | Micheline.Seq (loc, l) as e ->
      record_trace (Wrong_stack (loc, printable e))
      @@ List.map_e (parse_stack_item ~source) l
  | e -> error (Wrong_stack (Micheline.location e, printable e))
