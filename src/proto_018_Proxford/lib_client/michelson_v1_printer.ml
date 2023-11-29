(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Tezos_micheline
open Micheline
open Micheline_printer

let anon = {comment = None}

let node_of_expr expr =
  expr |> Michelson_v1_primitives.strings_of_prims
  |> Micheline.inject_locations (fun _ -> anon)

let node_of_stack_elt (ty, x) =
  let ty = node_of_expr ty in
  let x = node_of_expr x in
  Micheline.Prim (anon, "Stack_elt", [ty; x], [])

let print_typed_stack out l =
  print_expr out (Micheline.Seq (anon, List.map node_of_stack_elt l))

let print_expr ppf expr = print_expr ppf (node_of_expr expr)

let print_expr_unwrapped ppf expr = print_expr_unwrapped ppf (node_of_expr expr)

let print_stack ppf = function
  | [] -> Format.fprintf ppf "[]"
  | more ->
      Format.fprintf
        ppf
        "@[<hov 0>[ %a ]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ : ")
           print_expr_unwrapped)
        more

let print_execution_trace ppf (trace : Script_typed_ir.execution_trace) =
  Format.pp_print_list
    (fun ppf (loc, gas, stack) ->
      Format.fprintf
        ppf
        "- @[<v 0>location: %d (just consumed gas: %a)@,[ @[<v 0>%a ]@]@]"
        loc
        Gas.Arith.pp
        gas
        (Format.pp_print_list (fun ppf e ->
             Format.fprintf ppf "@[<v 0>%a@]" print_expr e))
        stack)
    ppf
    trace

let print_big_map_diff ppf lazy_storage_diff =
  let diff =
    Contract.Legacy_big_map_diff.of_lazy_storage_diff lazy_storage_diff
  in
  let pp_map ppf id =
    if Compare.Z.(id < Z.zero) then
      Format.fprintf ppf "temp(%s)" (Z.to_string (Z.neg id))
    else Format.fprintf ppf "map(%s)" (Z.to_string id)
  in
  Format.fprintf
    ppf
    "@[<v 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf -> function
       | Contract.Legacy_big_map_diff.Clear id ->
           Format.fprintf ppf "Clear %a" pp_map id
       | Contract.Legacy_big_map_diff.Alloc {big_map; key_type; value_type} ->
           Format.fprintf
             ppf
             "New %a of type (big_map %a %a)"
             pp_map
             big_map
             print_expr
             key_type
             print_expr
             value_type
       | Contract.Legacy_big_map_diff.Copy {src; dst} ->
           Format.fprintf ppf "Copy %a to %a" pp_map src pp_map dst
       | Contract.Legacy_big_map_diff.Update {big_map; diff_key; diff_value; _}
         ->
           Format.fprintf
             ppf
             "%s %a[%a]%a"
             (match diff_value with None -> "Unset" | Some _ -> "Set")
             pp_map
             big_map
             print_expr
             diff_key
             (fun ppf -> function
               | None -> ()
               | Some x -> Format.fprintf ppf " to %a" print_expr x)
             diff_value))
    (diff :> Contract.Legacy_big_map_diff.item list)

let inject_types type_map parsed =
  let rec inject_expr = function
    | Seq (loc, items) ->
        Seq (inject_loc `before loc, List.map inject_expr items)
    | Prim (loc, name, items, annot) ->
        Prim (inject_loc `after loc, name, List.map inject_expr items, annot)
    | Int (loc, value) -> Int (inject_loc `after loc, value)
    | String (loc, value) -> String (inject_loc `after loc, value)
    | Bytes (loc, value) -> Bytes (inject_loc `after loc, value)
  and inject_loc which loc =
    let comment =
      let ( >?? ) = Option.bind in
      List.assoc ~equal:Int.equal loc parsed.Michelson_v1_parser.expansion_table
      >?? fun (_, locs) ->
      let locs = List.sort compare locs in
      List.hd locs >?? fun head_loc ->
      List.assoc ~equal:Int.equal head_loc type_map >?? fun (bef, aft) ->
      let stack = match which with `before -> bef | `after -> aft in
      Some (Format.asprintf "%a" print_stack stack)
    in
    {comment}
  in
  inject_expr (root parsed.unexpanded)

let unparse ?type_map parse expanded =
  let source =
    match type_map with
    | Some type_map ->
        let unexpanded, unexpansion_table =
          expanded |> Michelson_v1_primitives.strings_of_prims |> root
          |> Michelson_v1_macros.unexpand_rec |> Micheline.extract_locations
        in
        let rec inject_expr = function
          | Seq (loc, items) ->
              Seq (inject_loc `before loc, List.map inject_expr items)
          | Prim (loc, name, items, annot) ->
              Prim
                (inject_loc `after loc, name, List.map inject_expr items, annot)
          | Int (loc, value) -> Int (inject_loc `after loc, value)
          | String (loc, value) -> String (inject_loc `after loc, value)
          | Bytes (loc, value) -> Bytes (inject_loc `after loc, value)
        and inject_loc which loc =
          let comment =
            let ( >?? ) = Option.bind in
            List.assoc ~equal:Int.equal loc unexpansion_table >?? fun loc ->
            List.assoc ~equal:Int.equal loc type_map >?? fun (bef, aft) ->
            let stack = match which with `before -> bef | `after -> aft in
            Some (Format.asprintf "%a" print_stack stack)
          in
          {comment}
        in
        unexpanded |> root |> inject_expr
        |> Format.asprintf "%a" Micheline_printer.print_expr
    | None ->
        expanded |> Michelson_v1_primitives.strings_of_prims |> root
        |> Michelson_v1_macros.unexpand_rec |> Micheline.strip_locations
        |> Micheline_printer.printable (fun n -> n)
        |> Format.asprintf "%a" Micheline_printer.print_expr
  in
  match parse source with
  | res, [] -> res
  | _, _ :: _ -> Stdlib.failwith "Michelson_v1_printer.unparse"

let unparse_toplevel ?type_map =
  unparse ?type_map Michelson_v1_parser.parse_toplevel

let unparse_expression = unparse Michelson_v1_parser.parse_expression

let unparse_invalid expanded =
  let source =
    expanded |> root |> Michelson_v1_macros.unexpand_rec
    |> Micheline.strip_locations
    |> Micheline_printer.printable (fun n -> n)
    |> Format.asprintf "%a" Micheline_printer.print_expr_unwrapped
  in
  fst (Michelson_v1_parser.parse_toplevel source)

let ocaml_constructor_of_prim prim =
  (* Assuming all the prim constructor prefixes match the
     [[Michelson_v1_primitives.namespace]]. *)
  let prefix =
    Michelson_v1_primitives.(namespace prim |> string_of_namespace)
  in
  Format.asprintf "%s_%s" prefix @@ Michelson_v1_primitives.string_of_prim prim

let micheline_string_of_expression ~zero_loc expression =
  let string_of_list : string list -> string =
   fun xs -> String.concat "; " xs |> Format.asprintf "[%s]"
  in
  let show_loc loc = if zero_loc then 0 else loc in
  let rec string_of_node = function
    | Int (loc, i) ->
        let z =
          match Z.to_int i with
          | 0 -> "Z.zero"
          | 1 -> "Z.one"
          | i -> Format.asprintf "Z.of_int %d" i
        in
        Format.asprintf "Int (%d, %s)" (show_loc loc) z
    | String (loc, s) -> Format.asprintf "String (%d, \"%s\")" (show_loc loc) s
    | Bytes (loc, b) ->
        Format.asprintf
          "Bytes (%d, Bytes.of_string \"%s\")"
          (show_loc loc)
          Bytes.(escaped b |> to_string)
    | Prim (loc, prim, nodes, annot) ->
        Format.asprintf
          "Prim (%d, %s, %s, %s)"
          (show_loc loc)
          (ocaml_constructor_of_prim prim)
          (string_of_list @@ List.map string_of_node nodes)
          (string_of_list @@ List.map (Format.asprintf "\"%s\"") annot)
    | Seq (loc, nodes) ->
        Format.asprintf
          "Seq (%d, %s)"
          (show_loc loc)
          (string_of_list @@ List.map string_of_node nodes)
  in
  string_of_node (root expression)
