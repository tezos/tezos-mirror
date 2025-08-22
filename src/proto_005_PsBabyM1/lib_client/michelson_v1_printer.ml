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

let print_expr ppf expr =
  expr |> Michelson_v1_primitives.strings_of_prims
  |> Micheline.inject_locations (fun _ -> anon)
  |> print_expr ppf

let print_expr_unwrapped ppf expr =
  expr |> Michelson_v1_primitives.strings_of_prims
  |> Micheline.inject_locations (fun _ -> anon)
  |> print_expr_unwrapped ppf

let print_var_annots ppf = List.iter (Format.fprintf ppf "%s ")

let print_annot_expr_unwrapped ppf (expr, annot) =
  Format.fprintf ppf "%a%a" print_var_annots annot print_expr_unwrapped expr

let print_stack ppf = function
  | [] -> Format.fprintf ppf "[]"
  | more ->
      Format.fprintf
        ppf
        "@[<hov 0>[ %a ]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ : ")
           print_annot_expr_unwrapped)
        more

let print_execution_trace ppf trace =
  Format.pp_print_list
    (fun ppf (loc, gas, stack) ->
      Format.fprintf
        ppf
        "- @[<v 0>location: %d (remaining gas: %a)@,[ @[<v 0>%a ]@]@]"
        loc
        Gas.pp
        gas
        (Format.pp_print_list (fun ppf (e, annot) ->
             Format.fprintf
               ppf
               "@[<v 0>%a  \t%s@]"
               print_expr
               e
               (match annot with None -> "" | Some a -> a)))
        stack)
    ppf
    trace

let print_big_map_diff ppf diff =
  let pp_map ppf id =
    if Compare.Z.(id < Z.zero) then
      Format.fprintf ppf "temp(%s)" (Z.to_string (Z.neg id))
    else Format.fprintf ppf "map(%s)" (Z.to_string id)
  in
  Format.fprintf
    ppf
    "@[<v 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf -> function
      | Contract.Clear id -> Format.fprintf ppf "Clear %a" pp_map id
      | Contract.Alloc {big_map; key_type; value_type} ->
          Format.fprintf
            ppf
            "New %a of type (big_map %a %a)"
            pp_map
            big_map
            print_expr
            key_type
            print_expr
            value_type
      | Contract.Copy (src, dst) ->
          Format.fprintf ppf "Copy %a to %a" pp_map src pp_map dst
      | Contract.Update {big_map; diff_key; diff_value; _} ->
          Format.fprintf
            ppf
            "%s %a[%a]%a"
            (match diff_value with None -> "Unset" | Some _ -> "Set")
            pp_map
            big_map
            print_expr
            diff_key
            (fun ppf -> function
              | None -> () | Some x -> Format.fprintf ppf " to %a" print_expr x)
            diff_value))
    diff

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

let ocaml_constructor_of_prim =
  let open Michelson_v1_primitives in
  function
  | K_parameter -> "K_parameter"
  | K_storage -> "K_storage"
  | K_code -> "K_code"
  | D_False -> "D_False"
  | D_Elt -> "D_Elt"
  | D_Left -> "D_Left"
  | D_None -> "D_None"
  | D_Pair -> "D_Pair"
  | D_Right -> "D_Right"
  | D_Some -> "D_Some"
  | D_True -> "D_True"
  | D_Unit -> "D_Unit"
  | I_PACK -> "I_PACK"
  | I_UNPACK -> "I_UNPACK"
  | I_BLAKE2B -> "I_BLAKE2B"
  | I_SHA256 -> "I_SHA256"
  | I_SHA512 -> "I_SHA512"
  | I_ABS -> "I_ABS"
  | I_ADD -> "I_ADD"
  | I_AMOUNT -> "I_AMOUNT"
  | I_AND -> "I_AND"
  | I_BALANCE -> "I_BALANCE"
  | I_CAR -> "I_CAR"
  | I_CDR -> "I_CDR"
  | I_CHAIN_ID -> "I_CHAIN_ID"
  | I_CHECK_SIGNATURE -> "I_CHECK_SIGNATURE"
  | I_COMPARE -> "I_COMPARE"
  | I_CONCAT -> "I_CONCAT"
  | I_CONS -> "I_CONS"
  | I_CREATE_ACCOUNT -> "I_CREATE_ACCOUNT"
  | I_CREATE_CONTRACT -> "I_CREATE_CONTRACT"
  | I_IMPLICIT_ACCOUNT -> "I_IMPLICIT_ACCOUNT"
  | I_DIP -> "I_DIP"
  | I_DROP -> "I_DROP"
  | I_DUP -> "I_DUP"
  | I_EDIV -> "I_EDIV"
  | I_EMPTY_BIG_MAP -> "I_EMPTY_BIG_MAP"
  | I_EMPTY_MAP -> "I_EMPTY_MAP"
  | I_EMPTY_SET -> "I_EMPTY_SET"
  | I_EQ -> "I_EQ"
  | I_EXEC -> "I_EXEC"
  | I_APPLY -> "I_APPLY"
  | I_FAILWITH -> "I_FAILWITH"
  | I_GE -> "I_GE"
  | I_GET -> "I_GET"
  | I_GT -> "I_GT"
  | I_HASH_KEY -> "I_HASH_KEY"
  | I_IF -> "I_IF"
  | I_IF_CONS -> "I_IF_CONS"
  | I_IF_LEFT -> "I_IF_LEFT"
  | I_IF_NONE -> "I_IF_NONE"
  | I_INT -> "I_INT"
  | I_LAMBDA -> "I_LAMBDA"
  | I_LE -> "I_LE"
  | I_LEFT -> "I_LEFT"
  | I_LOOP -> "I_LOOP"
  | I_LSL -> "I_LSL"
  | I_LSR -> "I_LSR"
  | I_LT -> "I_LT"
  | I_MAP -> "I_MAP"
  | I_MEM -> "I_MEM"
  | I_MUL -> "I_MUL"
  | I_NEG -> "I_NEG"
  | I_NEQ -> "I_NEQ"
  | I_NIL -> "I_NIL"
  | I_NONE -> "I_NONE"
  | I_NOT -> "I_NOT"
  | I_NOW -> "I_NOW"
  | I_OR -> "I_OR"
  | I_PAIR -> "I_PAIR"
  | I_PUSH -> "I_PUSH"
  | I_RIGHT -> "I_RIGHT"
  | I_SIZE -> "I_SIZE"
  | I_SOME -> "I_SOME"
  | I_SOURCE -> "I_SOURCE"
  | I_SENDER -> "I_SENDER"
  | I_SELF -> "I_SELF"
  | I_SLICE -> "I_SLICE"
  | I_STEPS_TO_QUOTA -> "I_STEPS_TO_QUOTA"
  | I_SUB -> "I_SUB"
  | I_SWAP -> "I_SWAP"
  | I_TRANSFER_TOKENS -> "I_TRANSFER_TOKENS"
  | I_SET_DELEGATE -> "I_SET_DELEGATE"
  | I_UNIT -> "I_UNIT"
  | I_UPDATE -> "I_UPDATE"
  | I_XOR -> "I_XOR"
  | I_ITER -> "I_ITER"
  | I_LOOP_LEFT -> "I_LOOP_LEFT"
  | I_ADDRESS -> "I_ADDRESS"
  | I_CONTRACT -> "I_CONTRACT"
  | I_ISNAT -> "I_ISNAT"
  | I_CAST -> "I_CAST"
  | I_RENAME -> "I_RENAME"
  | I_DIG -> "I_DIG"
  | I_DUG -> "I_DUG"
  | T_bool -> "T_bool"
  | T_contract -> "T_contract"
  | T_int -> "T_int"
  | T_key -> "T_key"
  | T_key_hash -> "T_key_hash"
  | T_lambda -> "T_lambda"
  | T_list -> "T_list"
  | T_map -> "T_map"
  | T_big_map -> "T_big_map"
  | T_nat -> "T_nat"
  | T_option -> "T_option"
  | T_or -> "T_or"
  | T_pair -> "T_pair"
  | T_set -> "T_set"
  | T_signature -> "T_signature"
  | T_string -> "T_string"
  | T_bytes -> "T_bytes"
  | T_mutez -> "T_mutez"
  | T_timestamp -> "T_timestamp"
  | T_unit -> "T_unit"
  | T_operation -> "T_operation"
  | T_address -> "T_address"
  | T_chain_id -> "T_chain_id"

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
