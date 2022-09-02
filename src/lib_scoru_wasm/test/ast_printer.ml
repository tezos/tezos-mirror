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

open Lazy_containers
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

let pp_resul_type = pp_vector Types.pp_value_type

let pp_func_type out = function
  | Types.FuncType (pt, rt) ->
      Format.fprintf out "FuncType (%a, %a)" pp_resul_type pt pp_resul_type rt

let pp_func =
  Source.pp_phrase @@ fun out {Ast.ftype; locals; body} ->
  Format.fprintf
    out
    "@[<hv 2>{ftype = %a;@; locals = %a;@; body = %a}@]"
    pp_var
    ftype
    (pp_vector Types.pp_value_type)
    locals
    Ast.pp_block_label
    body

let pp_func out func =
  match func with
  | Func.AstFunc (ft, _, f) ->
      Format.fprintf
        out
        "AstFunc @[<hv 2>(%a,@; %a)@]"
        pp_func_type
        ft
        pp_func
        f
  | Func.HostFunc (ft, n) ->
      Format.fprintf out "HostFunc @[<hv 2>(%a,@; %s)@]" pp_func_type ft n

let pp_ref out = function
  | Values.NullRef rt -> Format.fprintf out "NullRef (%a)" Types.pp_ref_type rt
  | Values.ExternRef n -> Format.fprintf out "ExternRef(%a)" pp_int32 n
  | _ -> Stdlib.failwith "Unsupported value ref"

let pp_chunk_byte_vector out chunks =
  let bs = Lwt_main.run @@ Chunked_byte_vector.to_string chunks in
  (* We just show the hash of the chunk in order to avoid too much noise. *)
  let hash = Hashtbl.hash bs in
  Format.fprintf out "#%d" hash

let pp_table out t =
  let ty = Partial_table.type_of t in
  let c = Partial_table.content t in
  Format.fprintf
    out
    "@[<hv 2>{ty = %a;@; content = (%a)}@]"
    Types.pp_table_type
    ty
    (pp_vector pp_ref)
    c

let pp_value_num = Values.pp_op pp_int32 pp_int64 pp_f32 pp_f64

let pp_value out = function
  | Values.Num n -> Format.fprintf out "Num %a" pp_value_num n
  | Values.Ref r -> Format.fprintf out "Ref %a" pp_ref r
  | Values.Vec (V128 v) ->
      let hash = Hashtbl.hash @@ V128.to_string v in
      Format.fprintf out "Vec (V128 (#%d))" hash

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
    pp_value
    content

let pp_extern out = function
  | Instance.ExternFunc f -> Format.fprintf out "ExternFunc %a" pp_func f
  | Instance.ExternTable t -> Format.fprintf out "ExternTable %a" pp_table t
  | Instance.ExternMemory m -> Format.fprintf out "ExternMemory %a" pp_memory m
  | Instance.ExternGlobal g -> Format.fprintf out "ExternGlobal %a" pp_global g

let pp_map pp out map =
  let pp_name_list = pp_list Format.pp_print_int in
  pp_list (pp_pair pp_name_list pp) out (Instance.NameMap.loaded_bindings map)

let pp_elems out ref = pp_vector pp_ref out !ref

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
    (pp_vector pp_func_type)
    types
    (pp_vector pp_func)
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
  Format.fprintf
    out
    "@[<v 2>{module = %s;@;locals = %a;@;}@]"
    key
    (Format.pp_print_list pp_value)
    (List.map ( ! ) frame.locals)

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
  | Refer ref_ -> Format.fprintf out "Refer @[<hv 2>%a@]" pp_ref ref_
  | Invoke func -> Format.fprintf out "Invoke @[<hv 2>%a@]" pp_func func
  | Trapping msg ->
      Format.fprintf out "Trapping @[<hv 2>%a@]" Format.pp_print_string msg
  | Returning values ->
      Format.fprintf
        out
        "Returning @[<hv 2>%a@]"
        (Format.pp_print_list pp_value)
        values
  | Breaking (index, values) ->
      Format.fprintf
        out
        "Breaking @[<hv 2>(%li,@; %a)@]"
        index
        (Format.pp_print_list pp_value)
        values
  | Label (index, final_instrs, (values, instrs)) ->
      Format.fprintf
        out
        "Label @[<hv 2>(%li,@; %a,@; %a,@; %a)@]"
        index
        (Format.pp_print_list Ast.pp_instr)
        final_instrs
        (Format.pp_print_list pp_value)
        values
        (Format.pp_print_list pp_admin_instr)
        instrs
  | Frame (index, frame, (values, instrs)) ->
      Format.fprintf
        out
        "Frame @[<hv 2>(%li,@; %a,@; %a,@; %a)@]"
        index
        pp_frame
        frame
        (Format.pp_print_list pp_value)
        values
        (Format.pp_print_list pp_admin_instr)
        instrs

and pp_admin_instr out instr = pp_admin_instr' out instr.Source.it

let pp_input_buffer out input =
  let open Input_buffer in
  Format.fprintf
    out
    "@[<v 2>{content = %a;@;num_elements = %s;@;}@]"
    (pp_vector_z Input_buffer.pp_message)
    (Lazy_vector.Mutable.ZVector.snapshot input.content)
    (Z.to_string input.num_elements)

let pp_index_vector out index_vector =
  Format.fprintf
    out
    "@[<v 2>%a@]"
    (pp_vector_z (fun o x -> Format.fprintf o "@[<v 2>%s@]" (Bytes.to_string x)))
    (Output_buffer.Index_Vector.snapshot index_vector)

let pp_output_buffer out (output : Output_buffer.t) =
  Format.fprintf
    out
    "@[<v 2>%a@]"
    (pp_vector (fun o -> pp_index_vector o))
    (Output_buffer.Level_Vector.snapshot output)

let pp_config out
    Eval.{frame; input; output; code = values, instrs; host_funcs = _; budget} =
  Format.fprintf
    out
    "@[<v 2>{frame = %a;@;\
     input = %a;@;\
     output = %a;@;\
     instructions = %a;@;\
     values = %a;@;\
     budget = %i;@;\
     }@]"
    pp_frame
    frame
    pp_input_buffer
    input
    pp_output_buffer
    output
    (Format.pp_print_list pp_admin_instr)
    instrs
    (Format.pp_print_list pp_value)
    values
    budget
