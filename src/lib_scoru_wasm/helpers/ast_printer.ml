(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Tezos_lazy_containers
open Tezos_webassembly_interpreter

let pp_int32 out n = Format.fprintf out "%ld" n

let pp_int64 out n = Format.fprintf out "%Ld" n

let pp_f32 _out _f32 =
  Stdlib.failwith "32bit floating point values are not supported"

let pp_f64 _out _f64 =
  Stdlib.failwith "64bit floating point values are not supported"

let pp_var = Source.pp_phrase pp_int32

let pp_list pp out x =
  Format.fprintf
    out
    "@[<hv 2>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ";@;") pp)
    x

let pp_value_type_list = pp_list Types.pp_value_type

let pp_opt pp out = function
  | Some x -> Format.fprintf out "Some @[<hv 2>(%a)@]" pp x
  | None -> Format.fprintf out "None"

let pp_unit out () = Format.pp_print_string out "()"

let pp_pair pp1 pp2 out (x, y) = Format.fprintf out "(%a, %a)" pp1 x pp2 y

let pp_vector pp out v =
  (* Force evaluation of the vector. *)
  let _ = Lwt_main.run @@ Lazy_vector.Int32Vector.to_list v in
  Lazy_vector.Int32Vector.pp pp out v

let pp_vector_z pp out v =
  (* Force evaluation of the vector. *)
  let _ = Lwt_main.run @@ Lazy_vector.ZVector.to_list v in
  Lazy_vector.ZVector.pp pp out v

module Types = struct
  include Tezos_webassembly_interpreter.Types

  let pp_result_type = pp_vector Types.pp_value_type

  type func_type = [%import: Tezos_webassembly_interpreter.Types.func_type]
  [@@deriving show]
end

module Ast = struct
  include Tezos_webassembly_interpreter.Ast

  type func' =
    [%import:
      (Tezos_webassembly_interpreter.Ast.func'
      [@with
        Vector.t :=
          (Tezos_webassembly_interpreter.Ast.Vector.t
          [@printer Types.pp_result_type])])]
  [@@deriving show]

  type func = [%import: Tezos_webassembly_interpreter.Ast.func]
  [@@deriving show]
end

module Func = struct
  include Tezos_webassembly_interpreter.Func

  type 'inst func =
    [%import:
      ('inst Tezos_webassembly_interpreter.Func.func
      [@with
        Tezos_webassembly_interpreter.Types.func_type := Types.func_type ;
        Tezos_webassembly_interpreter.Ast.func := Ast.func])]
  [@@deriving show]

  type 'inst t = 'inst func [@@deriving show]
end

module Instance = struct
  include Tezos_webassembly_interpreter.Instance

  type func_inst =
    [%import:
      (Tezos_webassembly_interpreter.Instance.func_inst
      [@with Tezos_webassembly_interpreter.Func.t := Func.t])]
  [@@deriving show]
end

let pp_chunk_byte_vector out chunks =
  let bs = Lwt_main.run @@ Chunked_byte_vector.to_string chunks in
  (* We just show the hash of the chunk in order to avoid too much noise. *)
  let hash = Hashtbl.hash bs in
  Format.fprintf out "#%d" hash

module Values = struct
  include Tezos_webassembly_interpreter.Values

  let pp_ref_ out = function
    | Values.NullRef rt ->
        Format.fprintf out "NullRef (%a)" Types.pp_ref_type rt
    | Values.ExternRef n -> Format.fprintf out "ExternRef(%a)" pp_int32 n
    | _ -> Stdlib.failwith "Unsupported value ref"

  type value = [%import: Tezos_webassembly_interpreter.Values.value]
  [@@deriving show]
end

let pp_table out t =
  let ty = Partial_table.type_of t in
  let c = Partial_table.content t in
  Format.fprintf
    out
    "@[<hv 2>{ty = %a;@; content = (%a)}@]"
    Types.pp_table_type
    ty
    (pp_vector Values.pp_ref_)
    c

let pp_memory out memory =
  let ty = Memory.type_of memory in
  let content = Memory.content memory in
  Format.fprintf
    out
    "@[<hv 2>{ty = %a;@; content = %a}@]"
    Types.pp_memory_type
    ty
    pp_chunk_byte_vector
    content

let pp_global out global =
  let ty = Global.type_of global in
  let content = Global.load global in
  Format.fprintf
    out
    "@[<hv 2>{ty = %a;@; content = %a}@]"
    Types.pp_global_type
    ty
    Values.pp_value
    content

let pp_extern out = function
  | Instance.ExternFunc f ->
      Format.fprintf out "ExternFunc %a" Instance.pp_func_inst f
  | Instance.ExternTable t -> Format.fprintf out "ExternTable %a" pp_table t
  | Instance.ExternMemory m -> Format.fprintf out "ExternMemory %a" pp_memory m
  | Instance.ExternGlobal g -> Format.fprintf out "ExternGlobal %a" pp_global g

let pp_map pp out map =
  let pp_name out name =
    Format.fprintf
      out
      "%s"
      (Tezos_webassembly_interpreter.Ast.string_of_name name)
  in
  pp_list
    (pp_pair pp_name (Format.pp_print_option pp))
    out
    (Instance.NameMap.loaded_bindings map)

let pp_elems out ref = pp_vector Values.pp_ref_ out !ref

let pp_blocks_table = pp_vector (pp_vector Ast.pp_instr)

let pp_datas_table = pp_vector pp_chunk_byte_vector

let pp_allocations out allocations =
  Format.fprintf
    out
    "@[<v 2>{blocks = %a;@;datas = %a;@;}@]"
    pp_blocks_table
    allocations.Ast.blocks
    pp_datas_table
    allocations.Ast.datas

let pp_data_inst out ref = Ast.pp_data_label out !ref

let pp_module out
    {
      Instance.types;
      funcs;
      tables;
      memories;
      globals;
      exports;
      elems;
      datas;
      allocations;
    } =
  Format.fprintf
    out
    "@[<v 2>{types = %a;@;\
     funcs = %a;@;\
     tables = %a;@;\
     memories = %a;@;\
     globals = %a;@;\
     exports = %a;@;\
     elems = %a;@;\
     datas = %a;@;\
     allocations = %a;@;\
     }@]"
    (pp_vector Types.pp_func_type)
    types
    (pp_vector Instance.pp_func_inst)
    funcs
    (pp_vector pp_table)
    tables
    (pp_vector pp_memory)
    memories
    (pp_vector pp_global)
    globals
    (pp_map pp_extern)
    exports
    (pp_vector pp_elems)
    elems
    (pp_vector pp_data_inst)
    datas
    pp_allocations
    allocations

let pp_frame out frame =
  let open Eval in
  let (Module_key key) = frame.inst in
  let locals =
    Lwt_main.run
      (Tezos_lazy_containers.Lazy_vector.Int32Vector.to_list frame.locals)
  in
  Format.fprintf
    out
    "@[<v 2>{module = %s;@;locals = %a;@;}@]"
    key
    (pp_vector Values.pp_value)
    (Tezos_lazy_containers.Lazy_vector.Int32Vector.of_list locals)

let rec pp_admin_instr' out instr =
  let open Eval in
  match instr with
  | From_block (block, index) ->
      Format.fprintf
        out
        "From_block @[<hv 2>(%a,@; %li)@]"
        Ast.pp_block_label
        block
        index
  | Plain instr -> Format.fprintf out "Plain @[<hv 2>%a@]" Ast.pp_instr' instr
  | Refer ref_ -> Format.fprintf out "Refer @[<hv 2>%a@]" Values.pp_ref_ ref_
  | Invoke func ->
      Format.fprintf out "Invoke @[<hv 2>%a@]" Instance.pp_func_inst func
  | Trapping msg ->
      Format.fprintf out "Trapping @[<hv 2>%a@]" Format.pp_print_string msg
  | Returning values ->
      Format.fprintf
        out
        "Returning @[<hv 2>%a@]"
        (pp_vector Values.pp_value)
        values
  | Breaking (index, values) ->
      Format.fprintf
        out
        "Breaking @[<hv 2>(%li,@; %a)@]"
        index
        (pp_vector Values.pp_value)
        values
  | Table_init_meta (idx, value, d, s, n, x, y) ->
      Format.fprintf
        out
        "@[<v 2>Table_init_meta (%ld, %a, %ld, %ld, %ld, %a, %a)@]"
        idx
        Values.pp_ref_
        value
        d
        s
        n
        Ast.pp_var
        x
        Ast.pp_var
        y
  | Table_fill_meta (idx, i, n, r, x) ->
      Format.fprintf
        out
        "@[<v 2>Table_fill_meta (%ld, %ld, %ld, %a, %a)@]"
        idx
        i
        n
        Values.pp_ref_
        r
        Ast.pp_var
        x
  | Table_copy_meta (idx, d, s, n, x, y, case) ->
      Format.fprintf
        out
        "@[<v 2>Table_copy_meta (%ld, %ld, %ld, %ld, %a, %a, %a)@]"
        idx
        d
        s
        n
        Ast.pp_var
        x
        Ast.pp_var
        y
        Format.pp_print_bool
        case
  | Memory_init_meta (idx, d, b, n, s, x) ->
      Format.fprintf
        out
        "@[<v 2>Memory_init_meta (%ld, %ld, %ld, %ld, %ld, %a)@]"
        idx
        d
        b
        n
        s
        Ast.pp_var
        x
  | Memory_fill_meta (idx, i, k, n) ->
      Format.fprintf
        out
        "@[<v 2>Memory_fill_meta (%ld, %ld, %a, %ld)@]"
        idx
        i
        Values.pp_num
        k
        n
  | Memory_copy_meta (idx, d, s, n, case) ->
      Format.fprintf
        out
        "@[<v 2>Memory_copy_meta (%ld, %ld, %ld, %ld, %a)@]"
        idx
        d
        s
        n
        Format.pp_print_bool
        case

and pp_admin_instr out instr = pp_admin_instr' out instr.Source.it

let pp_label out Eval.{label_arity; label_break; label_code = vs, es} =
  Format.fprintf
    out
    "@[<v 2>{label_arity = %a;@;\
     label_break = %a;@;\
     instructions = %a; values = %a}@]"
    (pp_opt (fun out x -> Format.fprintf out "%ld" x))
    label_arity
    (pp_opt Ast.pp_instr)
    label_break
    (pp_vector pp_admin_instr)
    es
    (pp_vector Values.pp_value)
    vs

let pp_label_kont : type a. Format.formatter -> a Eval.label_kont -> unit =
 fun out -> function
  | Label_stack (label, stack) ->
      Format.fprintf
        out
        "@[<v 2>Label_stack (@;%a,@;%a@;)@]"
        pp_label
        label
        (pp_vector pp_label)
        stack
  | Label_result res ->
      Format.fprintf
        out
        "@[<v 2>Label_result %a@]"
        (pp_vector Values.pp_value)
        res
  | Label_trapped msg -> Format.fprintf out "@[<v 2>Label_trapped %s@]" msg.it

let pp_frame_stack out Eval.{frame_arity; frame_specs; frame_label_kont} =
  Format.fprintf
    out
    "@[<v 2>{frame_arity = %a;@;frame_specs = %a;@;frame_label_kont = %a}@]"
    (pp_opt (fun out x -> Format.fprintf out "%ld" x))
    frame_arity
    pp_frame
    frame_specs
    pp_label_kont
    frame_label_kont

let pp_map_kont pp_origin pp_destination out Eval.{origin; destination; offset}
    =
  Format.fprintf
    out
    "@[<v 2>{origin = %a;@;destination = %a;@;offset = %ld}@]"
    (pp_vector pp_origin)
    origin
    (pp_vector pp_destination)
    destination
    offset

let pp_concat_kont pp out Eval.{lv; rv; res; offset} =
  Format.fprintf
    out
    "@[<v 2>{lv = %a;@;rv = %a;@;res = %a;@;offset = %ld;@;}@]"
    (pp_vector pp)
    lv
    (pp_vector pp)
    rv
    (pp_vector pp)
    res
    offset

let pp_reveal out = function
  | Host_funcs.Reveal_raw_data hash ->
      Format.fprintf out "Reveal_raw_data (%s)" hash
  | Reveal_metadata -> Format.fprintf out "Reveal_metadata"

let pp_invoke_step_kont out = function
  | Eval.Inv_start {func; code = vs, es} ->
      Format.fprintf
        out
        "@[<v 2>Inv_start {func = %a;@;instructions = %a;@;values = %a}@]"
        Instance.pp_func_inst
        func
        (pp_vector pp_admin_instr)
        es
        (pp_vector Values.pp_value)
        vs
  | Inv_prepare_locals
      {arity; args; vs; instructions; inst = Module_key inst; func; locals_kont}
    ->
      Format.fprintf
        out
        "@[<v 2>Inv_prepare_locals {arity = %ld;@;\
         args = %a;@;\
         values = %a;@;\
         instructions = %a;@;\
         inst = %s;@;\
         func = %a;@;\
         locals_kont = %a;@;\
         }"
        arity
        (pp_vector Values.pp_value)
        args
        (pp_vector Values.pp_value)
        vs
        (pp_vector pp_admin_instr)
        instructions
        inst
        Ast.pp_func
        func
        (pp_map_kont Types.pp_value_type Values.pp_value)
        locals_kont
  | Inv_prepare_args
      {arity; vs; instructions; inst = Module_key inst; func; locals; args_kont}
    ->
      Format.fprintf
        out
        "@[<v 2>Inv_prepare_locals {arity = %ld;@;\
         values = %a;@;\
         instructions = %a;@;\
         inst = %s;@;\
         func = %a;@;\
         locals = %a;@;\
         args_kont = %a;@;\
         }"
        arity
        (pp_vector Values.pp_value)
        vs
        (pp_vector pp_admin_instr)
        instructions
        inst
        Ast.pp_func
        func
        (pp_vector Values.pp_value)
        locals
        (pp_map_kont Values.pp_value Values.pp_value)
        args_kont
  | Inv_concat
      {arity; vs; instructions; inst = Module_key inst; func; concat_kont} ->
      Format.fprintf
        out
        "@[<v 2>Inv_prepare_locals {arity = %ld;@;\
         values = %a;@;\
         instructions = %a;@;\
         inst = %s;@;\
         func = %a;@;\
         concat_kont = %a;@;\
         }"
        arity
        (pp_vector Values.pp_value)
        vs
        (pp_vector pp_admin_instr)
        instructions
        inst
        Ast.pp_func
        func
        (pp_concat_kont Values.pp_value)
        concat_kont
  | Inv_reveal_tick {reveal; base_destination; max_bytes; code = vs, es} ->
      Format.fprintf
        out
        "@[<v 2>Inv_reveal_tick {instructions = %a;@;\
         values = %a;@;\
         reveal = %a;@;\
         base_destination = %ld;@;\
         max_bytes = %ld;@;\
         }@]"
        (pp_vector pp_admin_instr)
        es
        (pp_vector Values.pp_value)
        vs
        pp_reveal
        reveal
        base_destination
        max_bytes
  | Inv_stop {code = vs, es; fresh_frame; remaining_ticks} ->
      Format.fprintf
        out
        "%@[<v 2>Inv_stop {values = %a;@;\
         instructions = %a;@;\
         fresh_frame = %a;@;\
         remaining_ticks = %s}@]"
        (pp_vector Values.pp_value)
        vs
        (pp_vector pp_admin_instr)
        es
        (pp_opt pp_frame_stack)
        fresh_frame
        (Z.to_string remaining_ticks)

let pp_label_step_kont out = function
  | Eval.LS_Start label_kont ->
      Format.fprintf out "@[<v 2>LS_Start %a@]" pp_label_kont label_kont
  | LS_Craft_frame (label_kont, kont) ->
      Format.fprintf
        out
        "@[<v 2>LS_Craft_frame (%a, %a)@]"
        pp_label_kont
        label_kont
        pp_invoke_step_kont
        kont
  | LS_Push_frame (label_kont, frame) ->
      Format.fprintf
        out
        "@[<v 2>LS_Push_frame (%a, %a)@]"
        pp_label_kont
        label_kont
        pp_frame_stack
        frame
  | LS_Consolidate_top (label, kont, es, labels) ->
      Format.fprintf
        out
        "@[<v 2>LS_Consolidate_top (%a, %a, %a, %a)@]"
        pp_label
        label
        (pp_concat_kont Values.pp_value)
        kont
        (pp_vector pp_admin_instr)
        es
        (pp_vector pp_label)
        labels
  | LS_Modify_top label_kont ->
      Format.fprintf out "@[<v 2>LS_Modify_top %a@]" pp_label_kont label_kont

let pp_step_kont out = function
  | Eval.SK_Start (frame, stack) ->
      Format.fprintf
        out
        "@[<v 2>SK_Start (%a; %a)@]"
        pp_frame_stack
        frame
        (pp_vector pp_frame_stack)
        stack
  | SK_Next (frame_stack, stack, kont) ->
      Format.fprintf
        out
        "@[<v 2>SK_Next (%a, %a, %a)@]"
        pp_frame_stack
        frame_stack
        (pp_vector pp_frame_stack)
        stack
        pp_label_step_kont
        kont
  | SK_Consolidate_label_result (frame, stack, label, kont, es, labels) ->
      Format.fprintf
        out
        "@[<v 2>SK_Consolidate_label_result (%a, %a, %a, %a, %a, %a)@]"
        pp_frame_stack
        frame
        (pp_vector pp_frame_stack)
        stack
        pp_label
        label
        (pp_concat_kont Values.pp_value)
        kont
        (pp_vector pp_admin_instr)
        es
        (pp_vector pp_label)
        labels
  | SK_Result vs ->
      Format.fprintf out "@[<v 2>SK_Result %a@]" (pp_vector Values.pp_value) vs
  | SK_Trapped msg -> Format.fprintf out "@[<v 2>SK_Trapped %s@]" msg.it

let pp_input_buffer out input =
  Format.fprintf
    out
    "@[<v 2>%a@]"
    (pp_vector_z Input_buffer.pp_message)
    (Lazy_vector.Mutable.ZVector.snapshot input)

let pp_messages out index_vector =
  Format.fprintf
    out
    "@[<v 2>%a@]"
    (pp_vector_z (fun o x -> Format.fprintf o "@[<v 2>%s@]" (Bytes.to_string x)))
    (Output_buffer.Messages.snapshot index_vector)

let pp_outboxes out map =
  let pp_key out key = Format.fprintf out "%ld" key in
  pp_list
    (pp_pair pp_key (Format.pp_print_option pp_messages))
    out
    (Output_buffer.Outboxes.Map.loaded_bindings map)

let pp_output_buffer out
    ({outboxes; last_level; validity_period; message_limit} : Output_buffer.t) =
  Format.fprintf
    out
    "@[<v 2>{outboxes = %a;@;\
     last_level = %s;@;\
     validity_period = %ld;@;\
     message_limit = %s;@;\
     }@]"
    pp_outboxes
    (Output_buffer.Outboxes.snapshot outboxes)
    (match last_level with Some l -> Int32.to_string l | None -> "None")
    validity_period
    (Z.to_string message_limit)

let pp_buffers out Eval.{input; output} =
  Format.fprintf
    out
    "@[<v 2>{input = %a;@;output = %a;@;}@]"
    pp_input_buffer
    input
    pp_output_buffer
    output

let pp_module_registry out reg =
  let reg = Instance.ModuleMap.snapshot reg in
  Instance.ModuleMap.Map.pp pp_module out reg

let pp_config out Eval.{step_kont; stack_size_limit} =
  Format.fprintf
    out
    "@[<v 2>{frame_kont = %a;@;budget = %i;@;}@]"
    pp_step_kont
    step_kont
    stack_size_limit
