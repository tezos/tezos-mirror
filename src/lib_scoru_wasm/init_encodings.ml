(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_webassembly_interpreter.Eval
module Parser = Binary_parser_encodings
open Tezos_tree_encoding
open Kont_encodings
open Tezos_lazy_containers

let tag_encoding = value [] Data_encoding.string

let lazy_vec_encoding enc =
  Lazy_vector.Int32Vector.encoding (value [] Data_encoding.int32) enc

let eval_const_kont_encoding =
  tagged_union
    tag_encoding
    [
      case
        "EC_Next"
        Wasm_encoding.config_encoding
        (function EC_Next c -> Some c | _ -> None)
        (fun c -> EC_Next c);
      case
        "EC_Stop"
        Wasm_encoding.value_encoding
        (function EC_Stop v -> Some v | _ -> None)
        (fun v -> EC_Stop v);
    ]

let create_global_kont_encoding =
  tup2
    ~flatten:true
    (value ["global_type"] Interpreter_encodings.Types.global_type_encoding)
    (scope ["kont"] eval_const_kont_encoding)

let create_elem_kont_encoding =
  tick_map_kont_encoding
    eval_const_kont_encoding
    (lazy_vec_encoding (value [] Interpreter_encodings.Ast.const_encoding))
    (lazy_vec_encoding Wasm_encoding.value_ref_encoding)

type (_, _) eq = Eq : ('a, 'a) eq

let init_section_eq :
    type kont kont' a b c d.
    (kont, a, b) init_section ->
    (kont', c, d) init_section ->
    ((kont, a, b) init_section, (kont', c, d) init_section) eq option =
 fun sec1 sec2 ->
  match (sec1, sec2) with
  | Func, Func -> Some Eq
  | Global, Global -> Some Eq
  | Table, Table -> Some Eq
  | Memory, Memory -> Some Eq
  | _, _ -> None

let aggregate_cases :
    type kont a b.
    string ->
    (kont, a, b) init_section ->
    kont t ->
    a t ->
    b t ->
    (string, init_kont) case list =
 fun name sec enc_kont enc_a enc_b ->
  [
    case
      Format.(sprintf "IK_Aggregate_%s" name)
      (tup2
         ~flatten:true
         (scope ["module"] Wasm_encoding.module_instance_encoding)
         (scope
            ["kont"]
            (tick_map_kont_encoding
               enc_kont
               (lazy_vec_encoding enc_a)
               (lazy_vec_encoding enc_b))))
      (function
        | IK_Aggregate (m, sec', t) -> (
            match init_section_eq sec sec' with
            | Some Eq -> Some (m, t)
            | None -> None)
        | _ -> None)
      (function m, t -> IK_Aggregate (m, sec, t));
    case
      Format.(sprintf "IK_Aggregate_concat_%s" name)
      (tup2
         ~flatten:true
         (scope ["module"] Wasm_encoding.module_instance_encoding)
         (scope ["kont"] (concat_kont_encoding (lazy_vec_encoding enc_b))))
      (function
        | IK_Aggregate_concat (m, sec', t) -> (
            match init_section_eq sec sec' with
            | Some Eq -> Some (m, t)
            | None -> None)
        | _ -> None)
      (function m, t -> IK_Aggregate_concat (m, sec, t));
  ]

let aggregate_cases_either :
    type a b.
    string ->
    ((a, b) Either.t, a, b) init_section ->
    a t ->
    b t ->
    (string, init_kont) case list =
 fun name sec enc_a enc_b ->
  aggregate_cases name sec (either enc_a enc_b) enc_a enc_b

let join_kont_encoding enc_b =
  tagged_union
    tag_encoding
    [
      case
        "J_Init"
        (lazy_vec_encoding (lazy_vec_encoding enc_b))
        (function J_Init v -> Some v | _ -> None)
        (fun v -> J_Init v);
      case
        "J_Next"
        (tup2
           ~flatten:true
           (scope ["kont"] (concat_kont_encoding (lazy_vec_encoding enc_b)))
           (scope ["acc"] (lazy_vec_encoding (lazy_vec_encoding enc_b))))
        (function J_Next (kont, acc) -> Some (kont, acc) | _ -> None)
        (function kont, acc -> J_Next (kont, acc));
      case
        "J_Stop"
        (lazy_vec_encoding enc_b)
        (function J_Stop res -> Some res | _ -> None)
        (fun res -> J_Stop res);
    ]

let map_concat_kont_encoding enc_a enc_b =
  tagged_union
    tag_encoding
    [
      case
        "MC_Map"
        (map_kont_encoding
           (lazy_vec_encoding enc_a)
           (lazy_vec_encoding (lazy_vec_encoding enc_b)))
        (function MC_Map m -> Some m | _ -> None)
        (fun m -> MC_Map m);
      case
        "MC_Join"
        (join_kont_encoding enc_b)
        (function MC_Join j -> Some j | _ -> None)
        (fun j -> MC_Join j);
    ]

let exports_acc_encoding =
  conv
    (fun (exports_memory_0, exports) -> {exports_memory_0; exports})
    (fun {exports_memory_0; exports} -> (exports_memory_0, exports))
    (tup2
       ~flatten:true
       (value ["exports-memory-0"] Data_encoding.bool)
       (scope ["exports"] Wasm_encoding.extern_map_encoding))

let init_kont_encoding =
  tagged_union tag_encoding
  @@ [
       case
         "IK_Start"
         (scope ["externs"] (lazy_vec_encoding Wasm_encoding.extern_encoding))
         (function IK_Start exts -> Some exts | _ -> None)
         (fun exts -> IK_Start exts);
       case
         "IK_Add_import"
         (fold_right2_kont_encoding
            (lazy_vec_encoding Wasm_encoding.extern_encoding)
            (lazy_vec_encoding
               Parser.(no_region_encoding Import.import_encoding))
            Wasm_encoding.module_instance_encoding)
         (function IK_Add_import m -> Some m | _ -> None)
         (function m -> IK_Add_import m);
       case
         "IK_Types"
         (tup2
            ~flatten:true
            (scope ["module"] Wasm_encoding.module_instance_encoding)
            (scope
               ["kont"]
               (map_kont_encoding
                  (lazy_vec_encoding
                     Parser.(
                       no_region_encoding Wasm_encoding.func_type_encoding))
                  Wasm_encoding.function_type_vector_encoding)))
         (function IK_Type (m, t) -> Some (m, t) | _ -> None)
         (function m, t -> IK_Type (m, t));
     ]
  @ aggregate_cases_either
      "func"
      Func
      Parser.Code.func_encoding
      Wasm_encoding.function_encoding
  @ aggregate_cases
      "global"
      Global
      create_global_kont_encoding
      (value [] Interpreter_encodings.Ast.global_encoding)
      Wasm_encoding.global_encoding
  @ aggregate_cases_either
      "table"
      Table
      (value [] Interpreter_encodings.Ast.table_encoding)
      Wasm_encoding.table_encoding
  @ aggregate_cases_either
      "memory"
      Memory
      (value [] Interpreter_encodings.Ast.memory_encoding)
      Wasm_encoding.memory_encoding
  @ [
      case
        "IK_Exports"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope ["kont"]
           @@ fold_left_kont_encoding
                (lazy_vec_encoding
                   Parser.(no_region_encoding Export.export_encoding))
                exports_acc_encoding))
        (function IK_Exports (inst, fold) -> Some (inst, fold) | _ -> None)
        (function inst, fold -> IK_Exports (inst, fold));
      case
        "IK_Elems"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (tick_map_kont_encoding
                 create_elem_kont_encoding
                 (lazy_vec_encoding
                    Parser.(no_region_encoding Elem.elem_encoding))
                 (lazy_vec_encoding
                    (conv ref ( ! ) Wasm_encoding.value_ref_vector_encoding)))))
        (function IK_Elems (inst, map) -> Some (inst, map) | _ -> None)
        (function inst, map -> IK_Elems (inst, map));
      case
        "IK_Datas"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (map_kont_encoding
                 (lazy_vec_encoding
                    Parser.(no_region_encoding Data.data_segment_encoding))
                 (lazy_vec_encoding Wasm_encoding.data_label_ref_encoding))))
        (function IK_Datas (inst, map) -> Some (inst, map) | _ -> None)
        (function inst, map -> IK_Datas (inst, map));
      case
        "IK_Es_elem"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (map_concat_kont_encoding
                 Parser.(no_region_encoding Elem.elem_encoding)
                 Wasm_encoding.admin_instr_encoding)))
        (function IK_Es_elems (inst, map) -> Some (inst, map) | _ -> None)
        (function inst, map -> IK_Es_elems (inst, map));
      case
        "IK_Es_data"
        (tup3
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (map_concat_kont_encoding
                 Parser.(no_region_encoding Data.data_segment_encoding)
                 Wasm_encoding.admin_instr_encoding))
           (scope
              ["es_elem"]
              (lazy_vec_encoding Wasm_encoding.admin_instr_encoding)))
        (function
          | IK_Es_datas (inst, map, es_elem) -> Some (inst, map, es_elem)
          | _ -> None)
        (function inst, map, es_elem -> IK_Es_datas (inst, map, es_elem));
      case
        "IK_Join_admin"
        (tup2
           ~flatten:true
           (scope ["module"] Wasm_encoding.module_instance_encoding)
           (scope
              ["kont"]
              (join_kont_encoding Wasm_encoding.admin_instr_encoding)))
        (function IK_Join_admin (m, admin) -> Some (m, admin) | _ -> None)
        (function m, admin -> IK_Join_admin (m, admin));
      case
        "IK_Eval"
        (scope ["config"] Wasm_encoding.config_encoding)
        (function IK_Eval config -> Some config | _ -> None)
        (function config -> IK_Eval config);
      case
        "IK_Stop"
        (value [] Data_encoding.empty)
        (function IK_Stop -> Some () | _ -> None)
        (function () -> IK_Stop);
    ]
