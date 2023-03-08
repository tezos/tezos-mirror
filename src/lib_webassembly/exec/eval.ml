open Lwt.Syntax
open Values
open Types
open Instance
open Ast
open Source

(* Kontinuations *)

type ('a, 'b) map_kont = {
  origin : 'a Vector.t;
  destination : 'b Vector.t;
  offset : int32;
}

let map_kont v =
  {origin = v; destination = Vector.create (Vector.num_elements v); offset = 0l}

let map_completed {origin; offset; _} = offset = Vector.num_elements origin

(* Note: For a given [map_kont] value, it is expected that the same
   step function is systematically used. That is, if you start using
   [map_step] for instance, you cannot use [map_rev_step] for this
   value. *)

let map_step {origin; destination; offset} f =
  let open Lwt.Syntax in
  let+ x = Vector.get offset origin in
  let destination = Vector.set offset (f x) destination in
  {origin; destination; offset = Int32.succ offset}

let map_rev_step {origin; destination; offset} f =
  let open Lwt.Syntax in
  let+ x = Vector.get offset origin in
  let last = Int32.pred (Vector.num_elements origin) in
  let destination = Vector.set (Int32.sub last offset) (f x) destination in
  {origin; destination; offset = Int32.succ offset}

let map_i_step {origin; destination; offset} f =
  let open Lwt.Syntax in
  let+ x = Vector.get offset origin in
  let destination = Vector.set offset (f offset x) destination in
  {origin; destination; offset = Int32.succ offset}

let map_i_s_step {origin; destination; offset} f =
  let open Lwt.Syntax in
  let* x = Vector.get offset origin in
  let+ y = f offset x in
  let destination = Vector.set offset y destination in
  {origin; destination; offset = Int32.succ offset}

type 'a concat_kont = {
  lv : 'a Vector.t;
  rv : 'a Vector.t;
  res : 'a Vector.t;
  offset : int32;
}

let concat_kont lv rv =
  let lv_len = Vector.num_elements lv in
  let rv_len = Vector.num_elements rv in
  let len = Int32.(add lv_len rv_len) in
  if Int32.(unsigned_compare len lv_len < 0 || unsigned_compare len rv_len < 0)
  then raise Lazy_vector.SizeOverflow
  else {lv; rv; res = Vector.create len; offset = 0l}

let concat_step {lv; rv; res; offset} =
  let lv_len = Vector.num_elements lv in
  let+ x =
    if offset < lv_len then Vector.get offset lv
    else Vector.get Int32.(sub offset lv_len) rv
  in
  {lv; rv; res = Vector.set offset x res; offset = Int32.succ offset}

let concat_completed {lv; rv; offset; _} =
  let lv_len = Vector.num_elements lv in
  let rv_len = Vector.num_elements rv in
  Int32.add lv_len rv_len <= offset

(* Errors *)

module Link = Error.Make ()

module Trap = Error.Make ()

module Crash = Error.Make ()

module Exhaustion = Error.Make ()

exception Link = Link.Error

exception Trap = Trap.Error

exception Crash = Crash.Error (* failure that cannot happen in valid code *)

exception Exhaustion = Exhaustion.Error

type init_state =
  | Init_step
  | Map_step
  | Map_concat_step
  | Join_step
  | Section_step
  | Eval_const
  | Create_global_step
  | Run_data_step

exception Init_step_error of init_state

type eval_state = Invoke_step of string | Label_step | Frame_step | Eval_step

exception Evaluation_step_error of eval_state

let table_error at = function
  | Table.Bounds -> "out of bounds table access"
  | Table.SizeOverflow -> "table size overflow"
  | Table.SizeLimit -> "table size limit reached"
  | Table.Type -> Crash.error at "type mismatch at table access"
  | Lazy_map.UnexpectedAccess -> "unexpected access in lazy map"
  | exn -> raise exn

let memory_error at = function
  | Memory.Bounds -> "out of bounds memory access"
  | Memory.SizeOverflow -> "memory size overflow"
  | Memory.SizeLimit -> "memory size limit reached"
  | Memory.Type -> Crash.error at "type mismatch at memory access"
  | Lazy_map.UnexpectedAccess -> "unexpected access in lazy map"
  | exn -> raise exn

let numeric_error at = function
  | Ixx.Overflow -> "integer overflow"
  | Ixx.DivideByZero -> "integer divide by zero"
  | Ixx.InvalidConversion -> "invalid conversion to integer"
  | Values.TypeError (i, v, t) ->
      Crash.error
        at
        ("type error, expected " ^ Types.string_of_num_type t ^ " as operand "
       ^ string_of_int i ^ ", got "
        ^ Types.string_of_num_type (type_of_num v))
  | exn -> raise exn

(* Administrative Expressions & Configurations *)

type frame = {inst : module_key; mutable locals : value Vector.t}

type admin_instr = admin_instr' phrase

and admin_instr' =
  | From_block of block_label * int32
  | Plain of instr'
  | Refer of ref_
  | Invoke of func_inst
  | Trapping of string
  | Returning of value Vector.t
  | Breaking of int32 * value Vector.t
  | Table_init_meta of int32 * ref_ * int32 * int32 * int32 * var * var
  | Table_fill_meta of int32 * int32 * int32 * ref_ * var
  | Table_copy_meta of int32 * int32 * int32 * int32 * var * var * bool
  | Memory_init_meta of int32 * int32 * int32 * int32 * int32 * var
  | Memory_fill_meta of int32 * int32 * Values.num * int32
  | Memory_copy_meta of int32 * int32 * int32 * int32 * bool

let table_init_meta_instr idx value d s n x y at =
  let instrs =
    [
      Plain (Const (I32 d @@ at));
      Refer value;
      Plain (TableSet x);
      Plain (Const (I32 (I32.add d 1l) @@ at));
      Plain (Const (I32 (I32.add s 1l) @@ at));
      Plain (Const (I32 (I32.sub n 1l) @@ at));
      Plain (TableInit (x, y));
    ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Table_init_meta (Int32.succ idx, value, d, s, n, x, y) @@ at)
      )
  | None -> raise (Invalid_argument "table_fill_meta_instr")

let table_fill_meta_instr idx i n r x at =
  let instrs =
    [
      Plain (Const (I32 i @@ at));
      Refer r;
      Plain (TableSet x);
      Plain (Const (I32 (I32.add i 1l) @@ at));
      Refer r;
      Plain (Const (I32 (I32.sub n 1l) @@ at));
      Plain (TableFill x);
    ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Table_fill_meta (Int32.succ idx, i, n, r, x) @@ at) )
  | None -> raise (Invalid_argument "table_fill_meta_instr")

let table_copy_meta_instr idx d s n x y case at =
  let instrs =
    if case then
      [
        Plain (Const (I32 d @@ at));
        Plain (Const (I32 s @@ at));
        Plain (TableGet y);
        Plain (TableSet x);
        Plain (Const (I32 (I32.add d 1l) @@ at));
        Plain (Const (I32 (I32.add s 1l) @@ at));
        Plain (Const (I32 (I32.sub n 1l) @@ at));
        Plain (TableCopy (x, y));
      ]
    else
      [
        Plain (Const (I32 (I32.add d 1l) @@ at));
        Plain (Const (I32 (I32.add s 1l) @@ at));
        Plain (Const (I32 (I32.sub n 1l) @@ at));
        Plain (TableCopy (x, y));
        Plain (Const (I32 d @@ at));
        Plain (Const (I32 s @@ at));
        Plain (TableGet y);
        Plain (TableSet x);
      ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Table_copy_meta (Int32.succ idx, d, s, n, x, y, case) @@ at)
      )
  | None -> raise (Invalid_argument "table_copy_meta_instr")

let memory_init_meta_instr idx d b n s x at =
  let instrs =
    [
      Plain (Const (I32 d @@ at));
      Plain (Const (I32 b @@ at));
      Plain (Store {ty = I32Type; align = 0; offset = 0l; pack = Some Pack8});
      Plain (Const (I32 (I32.add d 1l) @@ at));
      Plain (Const (I32 (I32.add s 1l) @@ at));
      Plain (Const (I32 (I32.sub n 1l) @@ at));
      Plain (MemoryInit x);
    ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Memory_init_meta (Int32.succ idx, d, b, n, s, x) @@ at) )
  | None -> raise (Invalid_argument "memory_init_meta_instr")

let memory_fill_meta_instr idx i k n at =
  let instrs =
    [
      Plain (Const (I32 i @@ at));
      Plain (Const (k @@ at));
      Plain (Store {ty = I32Type; align = 0; offset = 0l; pack = Some Pack8});
      Plain (Const (I32 (I32.add i 1l) @@ at));
      Plain (Const (k @@ at));
      Plain (Const (I32 (I32.sub n 1l) @@ at));
      Plain MemoryFill;
    ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Memory_fill_meta (Int32.succ idx, i, k, n) @@ at) )
  | None -> raise (Invalid_argument "memory_init_meta_instr")

let memory_copy_meta_instr idx d s n case at =
  let instrs =
    if case then
      [
        Plain (Const (I32 d @@ at));
        Plain (Const (I32 s @@ at));
        Plain
          (Load {ty = I32Type; align = 0; offset = 0l; pack = Some (Pack8, ZX)});
        Plain (Store {ty = I32Type; align = 0; offset = 0l; pack = Some Pack8});
        Plain (Const (I32 (I32.add d 1l) @@ at));
        Plain (Const (I32 (I32.add s 1l) @@ at));
        Plain (Const (I32 (I32.sub n 1l) @@ at));
        Plain MemoryCopy;
      ]
    else
      [
        Plain (Const (I32 (I32.add d 1l) @@ at));
        Plain (Const (I32 (I32.add s 1l) @@ at));
        Plain (Const (I32 (I32.sub n 1l) @@ at));
        Plain MemoryCopy;
        Plain (Const (I32 d @@ at));
        Plain (Const (I32 s @@ at));
        Plain
          (Load {ty = I32Type; align = 0; offset = 0l; pack = Some (Pack8, ZX)});
        Plain (Store {ty = I32Type; align = 0; offset = 0l; pack = Some Pack8});
      ]
  in
  match List.nth_opt instrs (Int32.to_int idx) with
  | Some instr ->
      ( instr @@ at,
        if List.length instrs <= Int32.to_int idx + 1 then None
        else Some (Memory_copy_meta (Int32.succ idx, d, s, n, case) @@ at) )
  | None -> raise (Invalid_argument "memory_copy_meta_instr")

type code = value Vector.t * admin_instr Vector.t

type label = {
  label_arity : int32 option;
  label_break : instr option;
  label_code : code;
}

type ongoing = Ongoing_kind

type finished = Finished_kind

type _ label_kont =
  | Label_stack : label * label Vector.t -> ongoing label_kont
  | Label_result : value Vector.t -> finished label_kont
  | Label_trapped : string phrase -> finished label_kont

let label_kont label = Label_stack (label, Vector.empty ())

type 'a frame_stack = {
  frame_arity : int32 option;
  frame_specs : frame;
  frame_label_kont : 'a label_kont;
}

type invoke_step_kont =
  | Inv_start of {func : func_inst; code : code}
  | Inv_prepare_locals of {
      arity : int32;
      args : value Vector.t;
      vs : value Vector.t;
      instructions : admin_instr Vector.t;
      inst : module_key;
      func : func;
      locals_kont : (value_type, value) map_kont;
    }
  | Inv_prepare_args of {
      arity : int32;
      vs : value Vector.t;
      instructions : admin_instr Vector.t;
      inst : module_key;
      func : func;
      locals : value Vector.t;
      args_kont : (value, value) map_kont;
    }
  | Inv_concat of {
      arity : int32;
      vs : value Vector.t;
      instructions : admin_instr Vector.t;
      inst : module_key;
      func : func;
      concat_kont : value concat_kont;
    }
  | Inv_reveal_tick of {
      reveal : Host_funcs.reveal;
      base_destination : int32;
      max_bytes : int32;
      code : code;
    }
  | Inv_stop of {
      code : code;
      fresh_frame : ongoing frame_stack option;
      remaining_ticks : Z.t;
    }

let vector_pop_map v f at =
  if 1l <= Vector.num_elements v then
    let+ hd, v = Vector.pop v in
    match f hd with
    | Some r -> (r, v)
    | None -> Crash.error at "missing or ill-typed operand on stack"
  else Crash.error at "missing or ill-typed operand on stack"

let num = function Num n -> Some n | _ -> None

let num_i32 = function Num (I32 i) -> Some i | _ -> None

let ref_ = function Ref r -> Some r | _ -> None

let vec = function Vec v -> Some v | _ -> None

let vec_v128 = function Vec (V128 v) -> Some v | _ -> None

type label_step_kont =
  | LS_Start : ongoing label_kont -> label_step_kont
  | LS_Craft_frame of ongoing label_kont * invoke_step_kont
  | LS_Push_frame of ongoing label_kont * ongoing frame_stack
  | LS_Consolidate_top of
      label * value concat_kont * admin_instr Vector.t * label Vector.t
  | LS_Modify_top : 'a label_kont -> label_step_kont

type step_kont =
  | SK_Start : 'a frame_stack * ongoing frame_stack Vector.t -> step_kont
  | SK_Next :
      'a frame_stack * ongoing frame_stack Vector.t * label_step_kont
      -> step_kont
  | SK_Consolidate_label_result of
      ongoing frame_stack
      * ongoing frame_stack Vector.t
      * label
      * value concat_kont
      * admin_instr Vector.t
      * label Vector.t
  | SK_Result of value Vector.t
  | SK_Trapped of string phrase

type buffers = {input : input_inst; output : output_inst}

type config = {step_kont : step_kont; stack_size_limit : int}

let frame inst locals = {inst; locals}

let output_buffer_message_limit = Z.of_int32 100_000l

let default_output_buffer () =
  Output_buffer.alloc
    ~last_level:(Some 0l)
    ~validity_period:10l
    ~message_limit:output_buffer_message_limit

let buffers ?(input = Input_buffer.alloc ())
    ?(output = default_output_buffer ()) () =
  {input; output}

let config ~stack_size_limit ?frame_arity inst vs es =
  let frame = frame inst (Vector.empty ()) in
  let label_kont =
    label_kont
      {label_arity = frame_arity; label_code = (vs, es); label_break = None}
  in
  let frame_stack =
    {frame_arity; frame_specs = frame; frame_label_kont = label_kont}
  in
  {step_kont = SK_Start (frame_stack, Vector.empty ()); stack_size_limit}

let plain e = Plain e.it @@ e.at

let lookup_intmap category store x =
  Lwt.catch
    (fun () -> Instance.Vector.get x.it store)
    (function
      | Lazy_vector.Bounds ->
          Crash.error x.at ("undefined " ^ category ^ " " ^ Int32.to_string x.it)
      | Lazy_map.UnexpectedAccess ->
          Crash.error
            x.at
            ("unexpected access in lazy map for " ^ category ^ " "
           ^ Int32.to_string x.it)
      | exn -> Lwt.fail exn)

let type_ (inst : module_inst) x = lookup_intmap "type" inst.types x

let func (inst : module_inst) x = lookup_intmap "function" inst.funcs x

let table (inst : module_inst) x = lookup_intmap "table" inst.tables x

let memory (inst : module_inst) x = lookup_intmap "memory" inst.memories x

let global (inst : module_inst) x = lookup_intmap "global" inst.globals x

let elem (inst : module_inst) x = lookup_intmap "element segment" inst.elems x

let data (inst : module_inst) x = lookup_intmap "data segment" inst.datas x

let local (frame : frame) x = lookup_intmap "local" frame.locals x

let any_ref inst x i at =
  Lwt.catch
    (fun () ->
      let* tbl = table inst x in
      Table.load tbl i)
    (function
      | Table.Bounds -> Trap.error at ("undefined element " ^ Int32.to_string i)
      | exn -> Lwt.fail exn)

let func_ref inst x i at =
  let+ value = any_ref inst x i at in
  match value with
  | FuncRef f -> f
  | NullRef _ -> Trap.error at ("uninitialized element " ^ Int32.to_string i)
  | _ -> Crash.error at ("type mismatch for element " ^ Int32.to_string i)

let func_type_of = function
  | Func.AstFunc (t, _inst, _f) -> t
  | Func.HostFunc (t, _) -> t

let block_type inst bt =
  let empty () = Lazy_vector.Int32Vector.create 0l in
  let singleton i = Lazy_vector.Int32Vector.(create 1l |> set 0l i) in
  match bt with
  | VarBlockType x -> type_ inst x
  | ValBlockType None -> FuncType (empty (), empty ()) |> Lwt.return
  | ValBlockType (Some t) -> FuncType (empty (), singleton t) |> Lwt.return

let vmtake n vs = match n with Some n -> Vector.split vs n |> fst | None -> vs

let invoke_step ~init ~host_funcs ?(durable = Durable_storage.empty)
    (module_reg : module_reg) buffers frame at = function
  | Inv_stop {remaining_ticks; _} when remaining_ticks <= Z.zero ->
      raise (Evaluation_step_error (Invoke_step "Inv_stop cannot reduce"))
  | Inv_stop stop ->
      Lwt.return
        ( durable,
          Inv_stop {stop with remaining_ticks = Z.pred stop.remaining_ticks} )
  | Inv_start {func; code = vs, es} -> (
      let (FuncType (ins, out)) = func_type_of func in
      let n1, n2 =
        (Instance.Vector.num_elements ins, Instance.Vector.num_elements out)
      in
      let args, vs' = Vector.split vs n1 in
      match func with
      | Func.AstFunc (_, inst', f) ->
          Lwt.return
            ( durable,
              Inv_prepare_locals
                {
                  arity = n2;
                  args;
                  vs = vs';
                  instructions = es;
                  inst = inst';
                  func = f;
                  locals_kont = map_kont f.it.locals;
                } )
      | Func.HostFunc (_, global_name) ->
          Lwt.catch
            (fun () ->
              let host_func = Host_funcs.lookup ~global_name host_funcs in
              let* args = Vector.to_list args in
              let args = List.rev args in
              let* inst = resolve_module_ref module_reg frame.inst in
              let available_memories =
                if not init then Host_funcs.Available_memories inst.memories
                else Host_funcs.No_memories_during_init
              in
              match host_func with
              | Host_func f ->
                  let+ durable, res, remaining_ticks =
                    f
                      buffers.input
                      buffers.output
                      durable
                      available_memories
                      args
                  in
                  let vs' = Vector.prepend_list res vs' in
                  ( durable,
                    Inv_stop
                      {code = (vs', es); fresh_frame = None; remaining_ticks} )
              | Reveal_func f -> (
                  let* result = f available_memories args in
                  match result with
                  | Ok (reveal, {base; max_bytes}) ->
                      Lwt.return
                        ( durable,
                          Inv_reveal_tick
                            {
                              reveal;
                              max_bytes;
                              base_destination = base;
                              code = (vs', es);
                            } )
                  | Error err_code ->
                      let err_code = Num (I32 err_code) in
                      let vs' = Vector.prepend_list [err_code] vs' in
                      Lwt.return
                        ( durable,
                          Inv_stop
                            {
                              code = (vs', es);
                              fresh_frame = None;
                              remaining_ticks = Z.zero;
                            } )))
            (function Crash (_, msg) -> Crash.error at msg | exn -> raise exn))
  | Inv_prepare_locals
      {
        arity = n2;
        args;
        vs = vs';
        instructions = es;
        inst = inst';
        func = f;
        locals_kont;
      }
    when map_completed locals_kont ->
      Lwt.return
        ( durable,
          Inv_prepare_args
            {
              arity = n2;
              vs = vs';
              instructions = es;
              inst = inst';
              func = f;
              locals = locals_kont.destination;
              args_kont = map_kont args;
            } )
  | Inv_prepare_locals {arity; args; vs; instructions; inst; func; locals_kont}
    ->
      let+ locals_kont = map_step locals_kont default_value in
      ( durable,
        Inv_prepare_locals
          {arity; args; vs; instructions; inst; func; locals_kont} )
  | Inv_prepare_args {arity; vs; instructions; inst; func; locals; args_kont}
    when map_completed args_kont ->
      Lwt.return
        ( durable,
          Inv_concat
            {
              arity;
              vs;
              instructions;
              inst;
              func;
              concat_kont = concat_kont args_kont.destination locals;
            } )
  | Inv_prepare_args tick ->
      let+ args_kont = map_rev_step tick.args_kont Fun.id in
      (durable, Inv_prepare_args {tick with args_kont})
  | Inv_concat
      {
        arity = n2;
        vs = vs';
        instructions = es;
        inst = inst';
        func = f;
        concat_kont;
      }
    when concat_completed concat_kont ->
      let frame' = {inst = inst'; locals = concat_kont.res} in
      Lwt.return
        ( durable,
          Inv_stop
            {
              code = (vs', es);
              fresh_frame =
                Some
                  {
                    frame_arity = Some n2;
                    frame_specs = frame';
                    frame_label_kont =
                      label_kont
                        {
                          label_arity = Some n2;
                          label_break = None;
                          label_code =
                            ( Vector.empty (),
                              Vector.singleton
                                (From_block (f.it.body, 0l) @@ f.at) );
                        };
                  };
              remaining_ticks = Z.zero;
            } )
  | Inv_reveal_tick _ ->
      (* This is a reveal tick, not an evaluation tick. The PVM should
         prevent this execution path. *)
      raise
        (Evaluation_step_error
           (Invoke_step "The reveal tick cannot be evaluated as is"))
  | Inv_concat tick ->
      let+ concat_kont = concat_step tick.concat_kont in
      (durable, Inv_concat {tick with concat_kont})

(* Evaluation *)

(*
 * Conventions:
 *   e  : instr
 *   v  : value
 *   es : instr list
 *   vs : value stack
 *   c : config
 *)

let mem_oob module_reg frame x i n =
  let* inst = resolve_module_ref module_reg frame.inst in
  let+ mem = memory inst x in
  I64.gt_u
    (I64.add (I64_convert.extend_i32_u i) (I64_convert.extend_i32_u n))
    (Memory.bound mem)

let data_oob module_reg frame x i n =
  let* inst = resolve_module_ref module_reg frame.inst in
  let* data_label = data inst x in
  let+ data = Ast.get_data !data_label inst.allocations.datas in
  I64.gt_u
    (I64.add (I64_convert.extend_i32_u i) (I64_convert.extend_i32_u n))
    (Chunked_byte_vector.length data)

let table_oob module_reg frame x i n =
  let* inst = resolve_module_ref module_reg frame.inst in
  let+ tbl = table inst x in
  I64.gt_u
    (I64.add (I64_convert.extend_i32_u i) (I64_convert.extend_i32_u n))
    (I64_convert.extend_i32_u (Table.size tbl))

let elem_oob module_reg frame x i n =
  let* inst = resolve_module_ref module_reg frame.inst in
  let+ elem = elem inst x in
  I64.gt_u
    (I64.add (I64_convert.extend_i32_u i) (I64_convert.extend_i32_u n))
    (Int64.of_int32 (Instance.Vector.num_elements !elem))

(** [step_instr module_reg label vs at e es stack] returns the new
    state of the label stack [label, stack] for a given [frame], by
    executing the WASM instruction [e] on top of the admin instr
    stack [es] and value stack [vs]. *)
let step_instr module_reg frame label vs at e' es_rst stack :
    'a label_kont Lwt.t =
  let label_kont_with_code vs es' =
    Label_stack
      ({label with label_code = (vs, Vector.prepend_list es' es_rst)}, stack)
  in

  let return_label_kont_with_code vs es' =
    Lwt.return (label_kont_with_code vs es')
  in

  match e' with
  | Unreachable ->
      return_label_kont_with_code vs [Trapping "unreachable executed" @@ at]
  | Nop -> return_label_kont_with_code vs []
  | Block (bt, es') ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ (FuncType (ts1, ts2)) = block_type inst bt in
      let n1 = Lazy_vector.Int32Vector.num_elements ts1 in
      let n2 = Lazy_vector.Int32Vector.num_elements ts2 in
      let args, vs' = Vector.split vs n1 in
      let label' =
        {
          label_arity = Some n2;
          label_break = None;
          label_code = (args, Vector.singleton (From_block (es', 0l) @@ at));
        }
      in
      Label_stack
        (label', Vector.cons {label with label_code = (vs', es_rst)} stack)
  | Loop (bt, es') ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ (FuncType (ts1, _)) = block_type inst bt in
      let n1 = Lazy_vector.Int32Vector.num_elements ts1 in
      let args, vs' = Vector.split vs n1 in
      let label' =
        {
          label_arity = Some n1;
          label_break = Some (e' @@ at);
          label_code = (args, Vector.singleton (From_block (es', 0l) @@ at));
        }
      in
      Label_stack
        (label', Vector.cons {label with label_code = (vs', es_rst)} stack)
  | If (bt, es1, es2) ->
      (* Num (I32 i) :: vs' *)
      let+ i, vs' = vector_pop_map vs num_i32 at in
      label_kont_with_code
        vs'
        [
          (if i = 0l then Plain (Block (bt, es2)) @@ at
          else Plain (Block (bt, es1)) @@ at);
        ]
  | Br x ->
      return_label_kont_with_code (Vector.empty ()) [Breaking (x.it, vs) @@ at]
  | BrIf x ->
      (* Num (I32 i) :: vs' *)
      let+ i, vs' = vector_pop_map vs num_i32 at in
      label_kont_with_code vs' (if i = 0l then [] else [Plain (Br x) @@ at])
  | BrTable (xs, x) ->
      (* Num (I32 i) :: vs' *)
      let+ i, vs' = vector_pop_map vs num_i32 at in
      label_kont_with_code
        vs'
        (if I32.ge_u i (Lib.List32.length xs) then [Plain (Br x) @@ at]
        else [Plain (Br (Lib.List32.nth xs i)) @@ at])
  | Return -> return_label_kont_with_code (Vector.empty ()) [Returning vs @@ at]
  | Call x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ func = func inst x in
      label_kont_with_code vs [Invoke func @@ at]
  | CallIndirect (x, y) ->
      (* Num (I32 i) :: vs' *)
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* func = func_ref inst x i at in
      let* type_ = type_ inst y in
      let+ check_eq = Types.func_types_equal type_ (Func.type_of func) in
      label_kont_with_code
        vs'
        (if not check_eq then [Trapping "indirect call type mismatch" @@ at]
        else [Invoke func @@ at])
  | Drop ->
      (* _ :: vs' *)
      let+ _, vs' = vector_pop_map vs Option.some at in
      label_kont_with_code vs' []
  | Select _ ->
      (* Num (I32 i) :: v2 :: v1 :: vs' *)
      let* i, vs = vector_pop_map vs num_i32 at in
      let* v2, vs = vector_pop_map vs Option.some at in
      let+ v1, vs' = vector_pop_map vs Option.some at in
      label_kont_with_code
        (if i = 0l then Vector.cons v2 vs' else Vector.cons v1 vs')
        []
  | LocalGet x ->
      let+ r = local frame x in
      label_kont_with_code (Vector.cons r vs) []
  | LocalSet x ->
      (* v :: vs' *)
      let+ v, vs' = vector_pop_map vs Option.some at in
      frame.locals <- Vector.set x.it v frame.locals ;
      label_kont_with_code vs' []
  | LocalTee x ->
      (* v :: vs' *)
      let+ v, vs' = vector_pop_map vs Option.some at in
      frame.locals <- Vector.set x.it v frame.locals ;
      label_kont_with_code (Vector.cons v vs') []
  | GlobalGet x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ glob = global inst x in
      let value = Global.load glob in
      label_kont_with_code (Vector.cons value vs) []
  | GlobalSet x ->
      (* v :: vs' *)
      let* v, vs' = vector_pop_map vs Option.some at in
      Lwt.catch
        (fun () ->
          let* inst = resolve_module_ref module_reg frame.inst in
          let+ glob = global inst x in
          Global.store glob v ;
          label_kont_with_code vs' [])
        (function
          | Global.NotMutable -> Crash.error at "write to immutable global"
          | Global.Type -> Crash.error at "type mismatch at global write"
          | exn -> Lwt.fail exn)
  | TableGet x ->
      (* Num (I32 i) :: vs' *)
      let* i, vs' = vector_pop_map vs num_i32 at in
      Lwt.catch
        (fun () ->
          let* inst = resolve_module_ref module_reg frame.inst in
          let* tbl = table inst x in
          let+ value = Table.load tbl i in
          label_kont_with_code (Vector.cons (Ref value) vs') [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (table_error at exn) @@ at])
  | TableSet x ->
      (* Ref r :: Num (I32 i) :: vs' *)
      let* r, vs = vector_pop_map vs ref_ at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      Lwt.catch
        (fun () ->
          let* inst = resolve_module_ref module_reg frame.inst in
          let+ tbl = table inst x in
          Table.store tbl i r ;
          label_kont_with_code vs' [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (table_error at exn) @@ at])
  | TableSize x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ tbl = table inst x in
      label_kont_with_code (Vector.cons (Num (I32 (Table.size tbl))) vs) []
  | TableGrow x ->
      (* Num (I32 delta) :: Ref r :: vs' *)
      let* delta, vs = vector_pop_map vs num_i32 at in
      let* r, vs' = vector_pop_map vs ref_ at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ tab = table inst x in
      let old_size = Table.size tab in
      let result =
        try
          Table.grow tab delta r ;
          old_size
        with Table.SizeOverflow | Table.SizeLimit | Table.OutOfMemory -> -1l
      in
      label_kont_with_code (Vector.cons (Num (I32 result)) vs') []
  | TableFill x ->
      (* Num (I32 n) :: Ref r :: Num (I32 i) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* r, vs = vector_pop_map vs ref_ at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let+ oob = table_oob module_reg frame x i n in
      if oob then
        label_kont_with_code vs' [Trapping (table_error at Table.Bounds) @@ at]
      else if n = 0l then label_kont_with_code vs' []
      else
        let _ = assert (I32.lt_u i 0xffff_ffffl) in
        label_kont_with_code vs' [Table_fill_meta (0l, i, n, r, x) @@ at]
  | TableCopy (x, y) ->
      (* Num (I32 n) :: Num (I32 s) :: Num (I32 d) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* s, vs = vector_pop_map vs num_i32 at in
      let* d, vs' = vector_pop_map vs num_i32 at in
      let+ oob_d = table_oob module_reg frame x d n
      and+ oob_s = table_oob module_reg frame y s n in
      label_kont_with_code
        vs'
        (if oob_d || oob_s then [Trapping (table_error at Table.Bounds) @@ at]
        else if n = 0l then []
        else if I32.le_u d s then
          [Table_copy_meta (0l, d, s, n, x, y, true) @@ at]
        else (* d > s *)
          [Table_copy_meta (0l, d, s, n, x, y, false) @@ at])
  | TableInit (x, y) ->
      (* Num (I32 n) :: Num (I32 s) :: Num (I32 d) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* s, vs = vector_pop_map vs num_i32 at in
      let* d, vs' = vector_pop_map vs num_i32 at in
      let* oob_d = table_oob module_reg frame x d n in
      let* oob_s = elem_oob module_reg frame y s n in
      if oob_d || oob_s then
        return_label_kont_with_code
          vs'
          [Trapping (table_error at Table.Bounds) @@ at]
      else if n = 0l then return_label_kont_with_code vs' []
      else
        let* inst = resolve_module_ref module_reg frame.inst in
        let* seg = elem inst y in
        let+ value = Instance.Vector.get s !seg in
        label_kont_with_code
          vs'
          [Table_init_meta (0l, value, d, s, n, x, y) @@ at]
  | ElemDrop x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ seg = elem inst x in
      seg := Instance.Vector.create 0l ;
      label_kont_with_code vs []
  | Load {offset; ty; pack; _} ->
      (* Num (I32 i) :: vs' *)
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ n =
            match pack with
            | None -> Memory.load_num mem i offset ty
            | Some (sz, ext) -> Memory.load_num_packed sz ext mem i offset ty
          in
          label_kont_with_code (Vector.cons (Num n) vs') [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | Store {offset; pack; _} ->
      (* Num n :: Num (I32 i) :: vs' *)
      let* n, vs = vector_pop_map vs num at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ () =
            match pack with
            | None -> Memory.store_num mem i offset n
            | Some sz -> Memory.store_num_packed sz mem i offset n
          in
          label_kont_with_code vs' [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | VecLoad {offset; ty; pack; _} ->
      (* Num (I32 i) :: vs' *)
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ v =
            match pack with
            | None -> Memory.load_vec mem i offset ty
            | Some (sz, ext) -> Memory.load_vec_packed sz ext mem i offset ty
          in
          label_kont_with_code (Vector.cons (Vec v) vs') [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | VecStore {offset; _} ->
      (* Vec v :: Num (I32 i) :: vs' *)
      let* v, vs = vector_pop_map vs vec at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ () = Memory.store_vec mem i offset v in
          label_kont_with_code vs' [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | VecLoadLane ({offset; pack; _}, j) ->
      (* Vec (V128 v) :: Num (I32 i) :: vs' *)
      let* v, vs = vector_pop_map vs vec_v128 at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ v =
            match pack with
            | Pack8 ->
                let+ mem =
                  Memory.load_num_packed Pack8 SX mem i offset I32Type
                in
                V128.I8x16.replace_lane j v (I32Num.of_num 0 mem)
            | Pack16 ->
                let+ mem =
                  Memory.load_num_packed Pack16 SX mem i offset I32Type
                in
                V128.I16x8.replace_lane j v (I32Num.of_num 0 mem)
            | Pack32 ->
                let+ mem = Memory.load_num mem i offset I32Type in
                V128.I32x4.replace_lane j v (I32Num.of_num 0 mem)
            | Pack64 ->
                let+ mem = Memory.load_num mem i offset I64Type in
                V128.I64x2.replace_lane j v (I64Num.of_num 0 mem)
          in
          label_kont_with_code (Vector.cons (Vec (V128 v)) vs') [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | VecStoreLane ({offset; pack; _}, j) ->
      (* Vec (V128 v) :: Num (I32 i) :: vs' *)
      let* v, vs = vector_pop_map vs vec_v128 at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let* mem = memory inst (0l @@ at) in
      Lwt.catch
        (fun () ->
          let+ () =
            match pack with
            | Pack8 ->
                Memory.store_num_packed
                  Pack8
                  mem
                  i
                  offset
                  (I32 (V128.I8x16.extract_lane_s j v))
            | Pack16 ->
                Memory.store_num_packed
                  Pack16
                  mem
                  i
                  offset
                  (I32 (V128.I16x8.extract_lane_s j v))
            | Pack32 ->
                Memory.store_num
                  mem
                  i
                  offset
                  (I32 (V128.I32x4.extract_lane_s j v))
            | Pack64 ->
                Memory.store_num
                  mem
                  i
                  offset
                  (I64 (V128.I64x2.extract_lane_s j v))
          in
          label_kont_with_code vs' [])
        (fun exn ->
          return_label_kont_with_code vs' [Trapping (memory_error at exn) @@ at])
  | MemorySize ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ mem = memory inst (0l @@ at) in
      label_kont_with_code (Vector.cons (Num (I32 (Memory.size mem))) vs) []
  | MemoryGrow ->
      (* Num (I32 delta) :: vs' *)
      let* delta, vs' = vector_pop_map vs num_i32 at in
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ mem = memory inst (0l @@ at) in
      let old_size = Memory.size mem in
      let result =
        try
          Memory.grow mem delta ;
          old_size
        with Memory.SizeOverflow | Memory.SizeLimit | Memory.OutOfMemory ->
          -1l
      in
      label_kont_with_code (Vector.cons (Num (I32 result)) vs') []
  | MemoryFill ->
      (* Num (I32 n) :: Num k :: Num (I32 i) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* k, vs = vector_pop_map vs num at in
      let* i, vs' = vector_pop_map vs num_i32 at in
      let+ oob = mem_oob module_reg frame (0l @@ at) i n in
      label_kont_with_code
        vs'
        (if oob then [Trapping (memory_error at Memory.Bounds) @@ at]
        else if n = 0l then []
        else [Memory_fill_meta (0l, i, k, n) @@ at])
  | MemoryCopy ->
      (* Num (I32 n) :: Num (I32 s) :: Num (I32 d) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* s, vs = vector_pop_map vs num_i32 at in
      let* d, vs' = vector_pop_map vs num_i32 at in
      let+ oob_s = mem_oob module_reg frame (0l @@ at) s n
      and+ oob_d = mem_oob module_reg frame (0l @@ at) d n in
      label_kont_with_code
        vs'
        (if oob_s || oob_d then [Trapping (memory_error at Memory.Bounds) @@ at]
        else if n = 0l then []
        else if I32.le_u d s then [Memory_copy_meta (0l, d, s, n, true) @@ at]
        else (* d > s *)
          [Memory_copy_meta (0l, d, s, n, false) @@ at])
  | MemoryInit x ->
      (* Num (I32 n) :: Num (I32 s) :: Num (I32 d) :: vs' *)
      let* n, vs = vector_pop_map vs num_i32 at in
      let* s, vs = vector_pop_map vs num_i32 at in
      let* d, vs' = vector_pop_map vs num_i32 at in
      let* mem_oob = mem_oob module_reg frame (0l @@ at) d n in
      let* data_oob = data_oob module_reg frame x s n in
      if mem_oob || data_oob then
        return_label_kont_with_code
          vs'
          [Trapping (memory_error at Memory.Bounds) @@ at]
      else if n = 0l then return_label_kont_with_code vs' []
      else
        let* inst = resolve_module_ref module_reg frame.inst in
        let* seg = data inst x in
        let* seg = Ast.get_data !seg inst.allocations.datas in
        let+ b = Chunked_byte_vector.load_byte seg (Int64.of_int32 s) in
        let b = Int32.of_int b in
        label_kont_with_code vs' [Memory_init_meta (0l, d, b, n, s, x) @@ at]
  | DataDrop x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ seg = data inst x in
      seg := Data_label 0l ;
      label_kont_with_code vs []
  | RefNull t ->
      return_label_kont_with_code (Vector.cons (Ref (NullRef t)) vs) []
  | RefIsNull -> (
      (* Ref r :: vs' *)
      let+ r, vs' = vector_pop_map vs ref_ at in
      match r with
      | NullRef _ -> label_kont_with_code (Vector.cons (Num (I32 1l)) vs') []
      | _ -> label_kont_with_code (Vector.cons (Num (I32 0l)) vs') [])
  | RefFunc x ->
      let* inst = resolve_module_ref module_reg frame.inst in
      let+ f = func inst x in
      label_kont_with_code (Vector.cons (Ref (FuncRef f)) vs) []
  | Const n -> return_label_kont_with_code (Vector.cons (Num n.it) vs) []
  | Test testop -> (
      (* Num n :: vs' *)
      let+ n, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (value_of_bool (Eval_num.eval_testop testop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | Compare relop -> (
      (* Num n2 :: Num n1 :: vs' *)
      let* n2, vs = vector_pop_map vs num at in
      let+ n1, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (value_of_bool (Eval_num.eval_relop relop n1 n2)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | Unary unop -> (
      (* Num n :: vs' *)
      let+ n, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (Num (Eval_num.eval_unop unop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | Binary binop -> (
      (* Num n2 :: Num n1 :: vs' *)
      let* n2, vs = vector_pop_map vs num at in
      let+ n1, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (Num (Eval_num.eval_binop binop n1 n2)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | Convert cvtop -> (
      (* Num n :: vs' *)
      let+ n, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (Num (Eval_num.eval_cvtop cvtop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecConst v -> return_label_kont_with_code (Vector.cons (Vec v.it) vs) []
  | VecTest testop -> (
      (* Vec n :: vs' *)
      let+ n, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (value_of_bool (Eval_vec.eval_testop testop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecUnary unop -> (
      (* Vec n :: vs' *)
      let+ n, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_unop unop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecBinary binop -> (
      (* Vec n2 :: Vec n1 :: vs' *)
      let* n2, vs = vector_pop_map vs vec at in
      let+ n1, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_binop binop n1 n2)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecCompare relop -> (
      (* Vec n2 :: Vec n1 :: vs' *)
      let* n2, vs = vector_pop_map vs vec at in
      let+ n1, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_relop relop n1 n2)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecConvert cvtop -> (
      (* Vec n :: vs' *)
      let+ n, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_cvtop cvtop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecShift shiftop -> (
      (* Num s :: Vec v :: vs' *)
      let* s, vs = vector_pop_map vs num at in
      let+ v, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_shiftop shiftop v s)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecBitmask bitmaskop -> (
      (* Vec v :: vs' *)
      let+ v, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Num (Eval_vec.eval_bitmaskop bitmaskop v)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecTestBits vtestop -> (
      (* Vec n :: vs' *)
      let+ n, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (value_of_bool (Eval_vec.eval_vtestop vtestop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecUnaryBits vunop -> (
      (* Vec n :: vs' *)
      let+ n, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_vunop vunop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecBinaryBits vbinop -> (
      (* Vec n2 :: Vec n1 :: vs' *)
      let* n2, vs = vector_pop_map vs vec at in
      let+ n1, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_vbinop vbinop n1 n2)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecTernaryBits vternop -> (
      (* Vec v3 :: Vec v2 :: Vec v1 :: vs' *)
      let* v3, vs = vector_pop_map vs vec at in
      let* v2, vs = vector_pop_map vs vec at in
      let+ v1, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_vternop vternop v1 v2 v3)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecSplat splatop -> (
      (* Num n :: vs' *)
      let+ n, vs' = vector_pop_map vs num at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_splatop splatop n)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecExtract extractop -> (
      (* Vec v :: vs' *)
      let+ v, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Num (Eval_vec.eval_extractop extractop v)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])
  | VecReplace replaceop -> (
      (* Num r :: Vec v :: vs' *)
      let* r, vs = vector_pop_map vs num at in
      let+ v, vs' = vector_pop_map vs vec at in
      try
        label_kont_with_code
          (Vector.cons (Vec (Eval_vec.eval_replaceop replaceop v r)) vs')
          []
      with exn ->
        label_kont_with_code vs' [Trapping (numeric_error at exn) @@ at])

(* This function is used to implement the logic of the “meta”
   instruction for memory and memory manipulation. *)
let push_admin_instr label es vs instr next stack =
  LS_Modify_top
    (Label_stack
       ( {
           label with
           label_code =
             (vs, Vector.prepend_list (instr :: Option.to_list next) es);
         },
         stack ))

let label_step :
    init:bool ->
    host_funcs:Host_funcs.registry ->
    Durable_storage.t ->
    module_reg ->
    buffers ->
    frame ->
    label_step_kont ->
    (Durable_storage.t * label_step_kont) Lwt.t =
 fun ~init ~host_funcs durable module_reg buffers frame label_kont ->
  match label_kont with
  | LS_Push_frame _ | LS_Modify_top _ ->
      raise (Evaluation_step_error Label_step)
  | LS_Start (Label_stack (label, stack)) ->
      let vs, es = label.label_code in
      if 0l < Vector.num_elements es then
        let* e, es = Vector.pop es in
        let+ kont =
          match e.it with
          | Plain e' ->
              let+ kont =
                step_instr module_reg frame label vs e.at e' es stack
              in
              LS_Modify_top kont
          | From_block (Block_label b, i) ->
              let* inst = resolve_module_ref module_reg frame.inst in
              let* block = Vector.get b inst.allocations.blocks in
              let length = Vector.num_elements block in
              if i = length then
                Lwt.return
                  (LS_Modify_top
                     (Label_stack ({label with label_code = (vs, es)}, stack)))
              else
                let+ instr = Vector.get i block in
                LS_Modify_top
                  (Label_stack
                     ( {
                         label with
                         label_code =
                           ( vs,
                             Vector.prepend_list
                               [
                                 Plain instr.it @@ instr.at;
                                 From_block (Block_label b, Int32.succ i)
                                 @@ e.at;
                               ]
                               es );
                       },
                       stack ))
          | Refer r ->
              Lwt.return
                (LS_Modify_top
                   (Label_stack
                      ( {label with label_code = (Vector.cons (Ref r) vs, es)},
                        stack )))
          | Trapping msg ->
              Lwt.return (LS_Modify_top (Label_trapped (msg @@ e.at)))
          | Returning vs0 -> Lwt.return (LS_Modify_top (Label_result vs0))
          | Breaking (0l, vs0) ->
              let vs0 = vmtake label.label_arity vs0 in
              if Vector.num_elements stack = 0l then
                Lwt.return (LS_Modify_top (Label_result vs0))
              else
                let+ label', stack = Vector.pop stack in
                let vs, es = label'.label_code in
                LS_Consolidate_top
                  ( label',
                    concat_kont vs0 vs,
                    Vector.prepend_list
                      (List.map plain (Option.to_list label.label_break))
                      es,
                    stack )
          | Breaking (k, vs0) ->
              if Vector.num_elements stack = 0l then
                Crash.error e.at "undefined label" ;
              let+ label', stack = Vector.pop stack in
              let vs', es' = label'.label_code in
              LS_Modify_top
                (Label_stack
                   ( {
                       label' with
                       label_code =
                         ( vs',
                           Vector.cons
                             (Breaking (Int32.pred k, vs0) @@ e.at)
                             es' );
                     },
                     stack ))
          | Invoke func ->
              Lwt.return
                (LS_Craft_frame
                   ( Label_stack (label, stack),
                     Inv_start {func; code = (vs, es)} ))
          | Table_init_meta (idx, value, d, s, n, x, y) ->
              let instr, next =
                table_init_meta_instr idx value d s n x y e.at
              in
              Lwt.return (push_admin_instr label es vs instr next stack)
          | Table_fill_meta (idx, i, n, r, x) ->
              let instr, next = table_fill_meta_instr idx i n r x e.at in
              Lwt.return (push_admin_instr label es vs instr next stack)
          | Table_copy_meta (idx, d, s, n, y, x, case) ->
              let instr, next = table_copy_meta_instr idx d s n y x case e.at in
              Lwt.return (push_admin_instr label es vs instr next stack)
          | Memory_init_meta (idx, d, b, n, s, x) ->
              let instr, next = memory_init_meta_instr idx d b n s x e.at in
              Lwt.return (push_admin_instr label es vs instr next stack)
          | Memory_fill_meta (idx, i, k, n) ->
              let instr, next = memory_fill_meta_instr idx i k n e.at in
              Lwt.return (push_admin_instr label es vs instr next stack)
          | Memory_copy_meta (idx, d, s, n, case) ->
              let instr, next = memory_copy_meta_instr idx d s n case e.at in
              Lwt.return (push_admin_instr label es vs instr next stack)
        in
        (durable, kont)
      else if Vector.num_elements stack = 0l then
        Lwt.return (durable, LS_Modify_top (Label_result vs))
      else
        let+ label', stack = Vector.pop stack in
        let vs', es' = label'.label_code in
        (durable, LS_Consolidate_top (label', concat_kont vs vs', es', stack))
  | LS_Consolidate_top (label', tick, es', stack) when concat_completed tick ->
      Lwt.return
        ( durable,
          LS_Modify_top
            (Label_stack ({label' with label_code = (tick.res, es')}, stack)) )
  | LS_Consolidate_top (label', tick, es', stack) ->
      let+ tick = concat_step tick in
      (durable, LS_Consolidate_top (label', tick, es', stack))
  | LS_Craft_frame
      (Label_stack (label, stack), Inv_stop {code; fresh_frame; remaining_ticks})
    when remaining_ticks <= Z.zero ->
      let label_kont = Label_stack ({label with label_code = code}, stack) in
      Lwt.return
        ( durable,
          match fresh_frame with
          | Some frame_stack -> LS_Push_frame (label_kont, frame_stack)
          | None -> LS_Modify_top label_kont )
  | LS_Craft_frame (label, istep) ->
      let+ durable, istep =
        invoke_step
          ~init
          ~host_funcs
          ~durable
          module_reg
          buffers
          frame
          no_region
          istep
      in
      (durable, LS_Craft_frame (label, istep))

(** [dup_frame frame] copies [frame], such as the two copies do not
    share mutable fields. *)
let dup_frame {frame_arity; frame_specs = {inst; locals}; frame_label_kont} =
  {frame_arity; frame_specs = {inst; locals}; frame_label_kont}

let frame_step ~init ~host_funcs durable module_reg c buffers = function
  | SK_Result _ | SK_Trapped _ -> raise (Evaluation_step_error Frame_step)
  | SK_Start (frame, stack) ->
      let+ kont =
        match frame.frame_label_kont with
        | Label_trapped msg -> Lwt.return (SK_Trapped msg)
        | Label_result vs0 ->
            if Vector.num_elements stack = 0l then
              let vs0 = vmtake frame.frame_arity vs0 in
              Lwt.return (SK_Result vs0)
            else
              let+ frame', stack = Vector.pop stack in
              (*

                 Duplicating the frame to make sure modifying [frame']
                 does not modify the contents of [stack].

                 More precisely, the scenario we want to avoid is a 1
                 N-tick execution where [frame'] is modified, which
                 modifies the local copy of [frame'] in the in-memory
                 map of the lazy vector [stack], due to mutability.

                 Because of how [tree-encoding] works at the moment,
                 such a scenario would lead to a diverging context
                 hash between 1 N-tick and N 1-ticks executions.

               *)
              let frame' = dup_frame frame' in
              let label, lstack =
                match frame'.frame_label_kont with
                | Label_stack (label, lstack) -> (label, lstack)
              in
              let vs, es = label.label_code in
              SK_Consolidate_label_result
                (frame', stack, label, concat_kont vs0 vs, es, lstack)
        | Label_stack _ as label ->
            Lwt.return (SK_Next (frame, stack, LS_Start label))
      in
      (durable, kont)
  | SK_Consolidate_label_result (frame', stack, label, tick, es, lstack)
    when concat_completed tick ->
      let label_kont =
        Label_stack ({label with label_code = (tick.res, es)}, lstack)
      in
      Lwt.return
        (durable, SK_Start ({frame' with frame_label_kont = label_kont}, stack))
  | SK_Consolidate_label_result (frame', stack, label, tick, es, lstack) ->
      let+ tick = concat_step tick in
      ( durable,
        SK_Consolidate_label_result (frame', stack, label, tick, es, lstack) )
  | SK_Next (frame, stack, LS_Modify_top label_kont) ->
      let frame = {frame with frame_label_kont = label_kont} in
      Lwt.return (durable, SK_Start (frame, stack))
  | SK_Next (frame, stack, LS_Push_frame (label_kont, frame')) ->
      let stack_size = Int32.(succ (Vector.num_elements stack) |> to_int) in
      if c.stack_size_limit <= stack_size then
        Exhaustion.error no_region "call stack exhausted" ;
      let frame = {frame with frame_label_kont = label_kont} in
      Lwt.return (durable, SK_Start (frame', Vector.cons frame stack))
  | SK_Next (frame, stack, istep) ->
      let+ durable, istep =
        label_step
          ~init
          ~host_funcs
          durable
          module_reg
          buffers
          frame.frame_specs
          istep
      in
      (durable, SK_Next (frame, stack, istep))

let step ?(init = false) ?(durable = Durable_storage.empty) ~host_funcs
    module_reg c buffers =
  match c.step_kont with
  | SK_Result _ | SK_Trapped _ -> raise (Evaluation_step_error Eval_step)
  | kont ->
      let+ durable, step_kont =
        frame_step ~init ~host_funcs durable module_reg c buffers kont
      in
      (durable, {c with step_kont})

let rec eval ?(init = false) ~host_funcs durable module_reg (c : config) buffers
    : (Durable_storage.t * value list) Lwt.t =
  match c.step_kont with
  | SK_Result vs ->
      let+ values = Vector.to_list vs in
      (durable, values)
  | SK_Trapped {it = msg; at} -> Trap.error at msg
  | _ ->
      let* durable, c = step ~init ~host_funcs ~durable module_reg c buffers in
      eval ~init ~host_funcs durable module_reg c buffers

type reveal_error =
  | Reveal_step
  | Reveal_hash_decoding of string
  | Reveal_payload_decoding of string

exception Reveal_error of reveal_error

let is_reveal_tick = function
  | {
      step_kont = SK_Next (_, _, LS_Craft_frame (_, Inv_reveal_tick {reveal; _}));
      _;
    } ->
      Some reveal
  | _ -> None

let reveal_step reveal module_reg payload =
  let open Lwt.Syntax in
  function
  | {
      step_kont =
        SK_Next (frame, top, LS_Craft_frame (label, Inv_reveal_tick inv));
      _;
    } as config ->
      let* inst = resolve_module_ref module_reg frame.frame_specs.inst in
      let* memory = memory inst (0l @@ no_region) in
      let+ bytes_count =
        reveal
          ~memory
          ~dst:inv.base_destination
          ~max_bytes:inv.max_bytes
          ~payload
      in
      let vs, es = inv.code in
      let vs = Vector.cons (Num (I32 bytes_count)) vs in
      (* The number of bytes cannot be negative per construction. *)
      let ticks_to_consume =
        Tick_model.(of_int32_exn bytes_count * ticks_per_byte_written)
      in
      {
        config with
        step_kont =
          SK_Next
            ( frame,
              top,
              LS_Craft_frame
                ( label,
                  Inv_stop
                    {
                      code = (vs, es);
                      fresh_frame = None;
                      remaining_ticks = Tick_model.to_z ticks_to_consume;
                    } ) );
      }
  | _ -> raise (Reveal_error Reveal_step)

(* Functions & Constants *)

let invoke ?(stack_size_limit = 300) ~module_reg ~caller
    ?(input = Input_buffer.alloc ()) ?(output = default_output_buffer ())
    ?(durable = Durable_storage.empty) ?(init = false) host_funcs
    (func : func_inst) (vs : value list) :
    (Durable_storage.t * value list) Lwt.t =
  let at = match func with Func.AstFunc (_, _, f) -> f.at | _ -> no_region in
  let (FuncType (ins, out)) = Func.type_of func in
  let* ins_l = Lazy_vector.Int32Vector.to_list ins in
  if List.length vs <> (Lazy_vector.Int32Vector.num_elements ins |> Int32.to_int)
  then Crash.error at "wrong number of arguments" ;
  (* Invoke is only used to call individual functions from a module,
     and never used by the PVM. Thus is does not need to be
     tickified and going from and to list is acceptable. *)
  if not (List.for_all2 (fun v -> ( = ) (type_of_value v)) vs ins_l) then
    Crash.error at "wrong types of arguments" ;
  let inst =
    match func with
    | Func.AstFunc (_, inst, _) -> inst
    | Func.HostFunc (_, _) -> caller
  in
  let n = Vector.num_elements out in
  let c =
    config
      ~stack_size_limit
      ~frame_arity:n
      inst
      (Vector.of_list (List.rev vs))
      (Vector.singleton (Invoke func @@ at))
  in
  let buffers = buffers ~input ~output () in
  Lwt.catch
    (fun () ->
      let+ durable, values =
        eval ~init ~host_funcs durable module_reg c buffers
      in
      (durable, List.rev values))
    (function
      | Stack_overflow -> Exhaustion.error at "call stack exhausted"
      | exn -> Lwt.fail exn)

type eval_const_kont = EC_Next of config | EC_Stop of value

let eval_const_kont ~stack_size_limit inst (const : const) =
  let c =
    config
      ~stack_size_limit
      inst
      (Vector.empty ())
      (Vector.singleton (From_block (const.it, 0l) @@ const.at))
  in
  EC_Next c

let eval_const_completed = function EC_Stop v -> Some v | _ -> None

let eval_const_step ~host_funcs module_reg buffers = function
  | EC_Next {step_kont = SK_Result vs; _} ->
      if Vector.num_elements vs = 1l then
        let+ v, _ = Vector.pop vs in
        EC_Stop v
      else Crash.error Source.no_region "wrong number of results on stack"
  | EC_Next c ->
      let+ _, c = step ~init:true ~host_funcs module_reg c buffers in
      EC_Next c
  | EC_Stop _ -> raise (Init_step_error Eval_const)

(* Modules *)

let create_func module_reg (inst_ref : module_key) (f : func) : func_inst Lwt.t
    =
  let* inst = resolve_module_ref module_reg inst_ref in
  let+ type_ = type_ inst f.it.ftype in
  Func.alloc type_ inst_ref f

let create_table (tab : table) : table_inst =
  let {ttype} = tab.it in
  let (TableType (_lim, t)) = ttype in
  Table.alloc ttype (NullRef t)

let create_memory (mem : memory) : memory_inst =
  let {mtype} = mem.it in
  Memory.alloc mtype

type create_global_kont = global_type * eval_const_kont

let create_global_kont ~stack_size_limit inst glob =
  (glob.it.gtype, eval_const_kont ~stack_size_limit inst glob.it.ginit)

let create_global_completed (gtype, kont) =
  match eval_const_completed kont with
  | Some v -> Some (Global.alloc gtype v)
  | None -> None

let create_global_step ~host_funcs module_reg buffers ((gtype, ekont) as kont) =
  match create_global_completed kont with
  | Some _ -> raise (Init_step_error Create_global_step)
  | None ->
      let+ ekont = eval_const_step ~host_funcs module_reg buffers ekont in
      (gtype, ekont)

let create_export (inst : module_inst) (ex : export) : export_inst Lwt.t =
  let {name; edesc} = ex.it in
  let+ ext =
    match edesc.it with
    | FuncExport x ->
        let+ func = func inst x in
        ExternFunc func
    | TableExport x ->
        let+ tbl = table inst x in
        ExternTable tbl
    | MemoryExport x ->
        let+ mem = memory inst x in
        ExternMemory mem
    | GlobalExport x ->
        let+ glob = global inst x in
        ExternGlobal glob
  in
  (name, ext)

let create_data (seg : data_segment) : data_inst =
  let {dinit; _} = seg.it in
  ref dinit

let add_import (m : module_) (ext : extern) (im : import) (inst : module_inst) :
    module_inst Lwt.t =
  let* t = import_type m im in
  let+ type_match = match_extern_type (extern_type_of ext) t in
  (if not type_match then
   let module_name = im.it.module_name in
   let item_name = im.it.item_name in
   Link.error
     im.at
     ("incompatible import type for " ^ "\"" ^ module_name ^ "\" " ^ "\""
    ^ item_name ^ "\": " ^ "expected "
     ^ Types.string_of_extern_type t
     ^ ", got "
     ^ Types.string_of_extern_type (extern_type_of ext))) ;

  match ext with
  | ExternFunc func -> {inst with funcs = Vector.cons func inst.funcs}
  | ExternTable tab -> {inst with tables = Vector.cons tab inst.tables}
  | ExternMemory mem -> {inst with memories = Vector.cons mem inst.memories}
  | ExternGlobal glob -> {inst with globals = Vector.cons glob inst.globals}

let run_elem i elem =
  let at = elem.it.emode.at in
  let x = i @@ at in
  match elem.it.emode.it with
  | Passive -> []
  | Active {index; offset} ->
      (From_block (offset.it, 0l) @@ offset.at)
      :: List.map
           plain
           [
             Const (I32 0l @@ at) @@ at;
             Const
               (I32 (Lazy_vector.Int32Vector.num_elements elem.it.einit) @@ at)
             @@ at;
             TableInit (index, x) @@ at;
             ElemDrop x @@ at;
           ]
  | Declarative -> List.map plain [ElemDrop x @@ at]

let run_data (inst : module_inst) i data =
  let at = data.it.dmode.at in
  let x = i @@ at in
  match data.it.dmode.it with
  | Passive -> Lwt.return []
  | Active {index; offset} ->
      assert (index.it = 0l) ;
      let+ data = Ast.get_data data.it.dinit inst.allocations.datas in
      (From_block (offset.it, 0l) @@ offset.at)
      :: List.map
           plain
           [
             Const (I32 0l @@ at) @@ at;
             Const
               (I32
                  (Int32.of_int
                     (Int64.to_int (Chunked_byte_vector.length data)))
               @@ at)
             @@ at;
             MemoryInit x @@ at;
             DataDrop x @@ at;
           ]
  | Declarative -> raise (Init_step_error Run_data_step)

let run_start start = [plain (Call start.it.sfunc @@ start.at)]

type ('a, 'b, 'acc) fold_right2_kont = {
  acc : 'acc;
  lv : 'a Vector.t;
  rv : 'b Vector.t;
  offset : int32;
}

let fold_right2_kont (m : module_) acc lv rv =
  if Vector.num_elements lv <> Vector.num_elements rv then
    Link.error m.at "wrong number of imports provided for initialisation"
  else {acc; lv; rv; offset = Int32.pred (Vector.num_elements lv)}

let fold_right2_completed {offset; _} = offset = -1l

let fold_right2_step {acc; lv; rv; offset} f =
  let open Lwt.Syntax in
  let* x = Vector.get offset lv in
  let* y = Vector.get offset rv in
  let+ acc = f x y acc in
  {acc; lv; rv; offset = Int32.pred offset}

type ('a, 'b) fold_left_kont = {origin : 'a Vector.t; acc : 'b; offset : int32}

let fold_left_kont origin acc = {origin; acc; offset = 0l}

let fold_left_completed {origin; offset; _} =
  offset = Vector.num_elements origin

let fold_left_s_step {origin; acc; offset} f =
  let open Lwt.Syntax in
  let* x = Vector.get offset origin in
  let+ acc = f acc x in
  {origin; acc; offset = Int32.succ offset}

type ('kont, 'a, 'b) tick_map_kont = {
  tick : 'kont option;
  map : ('a, 'b) map_kont;
}

let tick_map_completed {map; _} = map_completed map

let tick_map_kont v = {tick = None; map = map_kont v}

let tick_map_step first_kont kont_completed kont_step = function
  | {map; _} when map_completed map -> raise (Init_step_error Map_step)
  | {tick = None; map} ->
      let+ x = Vector.get map.offset map.origin in
      let tick = first_kont x in
      {tick = Some tick; map}
  | {tick = Some tick; map} -> (
      match kont_completed tick with
      | Some v ->
          let map =
            {
              map with
              destination = Vector.set map.offset v map.destination;
              offset = Int32.succ map.offset;
            }
          in
          Lwt.return {tick = None; map}
      | None ->
          let+ tick = kont_step tick in
          {tick = Some tick; map})

type (_, _, _) init_section =
  | Func : ((func, func_inst) Either.t, func, func_inst) init_section
  | Global : (create_global_kont, global, global_inst) init_section
  | Table : ((table, table_inst) Either.t, table, table_inst) init_section
  | Memory : ((memory, memory_inst) Either.t, memory, memory_inst) init_section

let section_fetch_vec :
    type kont a b. module_inst -> (kont, a, b) init_section -> b Vector.t =
 fun inst sec ->
  match sec with
  | Func -> inst.funcs
  | Global -> inst.globals
  | Table -> inst.tables
  | Memory -> inst.memories

let section_set_vec :
    type kont a b.
    module_inst -> (kont, a, b) init_section -> b Vector.t -> module_inst =
 fun inst sec vec ->
  match (sec, vec) with
  | Func, funcs -> {inst with funcs}
  | Global, globals -> {inst with globals}
  | Table, tables -> {inst with tables}
  | Memory, memories -> {inst with memories}

type 'b join_kont =
  | J_Init of 'b Vector.t Vector.t
  | J_Next of 'b concat_kont * 'b Vector.t Vector.t
  | J_Stop of 'b Vector.t

let join_kont vec = J_Init vec

let join_completed = function
  | J_Init v when Vector.num_elements v = 0l -> Some (Vector.create 0l)
  | J_Stop v -> Some v
  | _ -> None

let join_step =
  let open Lwt.Syntax in
  function
  | J_Init v when 1l < Vector.num_elements v ->
      let* x, v = Vector.pop v in
      let+ y, v = Vector.pop v in
      J_Next (concat_kont x y, v)
  | J_Init v when Vector.num_elements v = 1l ->
      let+ v = Vector.get 0l v in
      J_Stop v
  | J_Next (tick, acc)
    when concat_completed tick && Vector.num_elements acc = 0l ->
      Lwt.return (J_Stop tick.res)
  | J_Next (tick, acc) when concat_completed tick ->
      let+ x, acc = Vector.pop acc in
      J_Next (concat_kont tick.res x, acc)
  | J_Next (tick, acc) ->
      let+ tick = concat_step tick in
      J_Next (tick, acc)
  | J_Init _ ->
      (* [num_elements = 0l], so [join_completed] returns [Some], should not be called in this state *)
      raise (Init_step_error Join_step)
  | J_Stop _ -> raise (Init_step_error Join_step)

type ('a, 'b) map_concat_kont =
  | MC_Map of ('a, 'b Vector.t) map_kont
  | MC_Join of 'b join_kont

let map_concat_kont v = MC_Map (map_kont v)

let map_concat_completed = function MC_Join v -> join_completed v | _ -> None

let map_concat_step f = function
  | MC_Map map when map_completed map ->
      Lwt.return (MC_Join (join_kont map.destination))
  | MC_Map map ->
      let+ map = f map in
      MC_Map map
  | MC_Join tick -> (
      match join_completed tick with
      | Some _ ->
          (* [map_concat_completed] would have returned [Some], so
             illegal state to call this function *)
          raise (Init_step_error Map_concat_step)
      | None ->
          let+ tick = join_step tick in
          MC_Join tick)

type create_elem_kont = (eval_const_kont, const, ref_) tick_map_kont

let create_elem_kont seg = tick_map_kont seg.it.einit

let create_elem_completed : create_elem_kont -> elem_inst option =
 fun kont ->
  if tick_map_completed kont then Some (ref kont.map.destination) else None

let create_elem_step ~host_funcs ~stack_size_limit ~module_reg buffers inst :
    create_elem_kont -> create_elem_kont Lwt.t =
 fun tick ->
  tick_map_step
    (eval_const_kont ~stack_size_limit inst)
    (fun x ->
      match eval_const_completed x with
      | Some x -> Some (as_ref x)
      | None -> None)
    (eval_const_step ~host_funcs module_reg buffers)
    tick

type exports_acc = {exports : extern NameMap.t; exports_memory_0 : bool}

type init_kont =
  | IK_Start of extern Vector.t
  | IK_Add_import of (extern, import, module_inst) fold_right2_kont
  | IK_Type of module_inst * (type_, func_type) map_kont
  | IK_Aggregate :
      module_inst * ('kont, 'a, 'b) init_section * ('kont, 'a, 'b) tick_map_kont
      -> init_kont
  | IK_Aggregate_concat :
      module_inst * ('kont, 'a, 'b) init_section * 'b concat_kont
      -> init_kont
  | IK_Exports of module_inst * (Ast.export, exports_acc) fold_left_kont
  | IK_Elems of
      module_inst
      * (create_elem_kont, Ast.elem_segment, elem_inst) tick_map_kont
  | IK_Datas of module_inst * (data_segment, data_inst) map_kont
  | IK_Es_elems of module_inst * (elem_segment, admin_instr) map_concat_kont
  | IK_Es_datas of
      module_inst
      * (data_segment, admin_instr) map_concat_kont
      * admin_instr Vector.t
  | IK_Join_admin of module_inst * admin_instr join_kont
  | IK_Eval of config
  | IK_Stop

let section_next_init_kont :
    type kont a b.
    module_ -> module_inst -> (kont, a, b) init_section -> init_kont =
 fun m inst0 sec ->
  match sec with
  | Func -> IK_Aggregate (inst0, Global, tick_map_kont m.it.globals)
  | Global -> IK_Aggregate (inst0, Table, tick_map_kont m.it.tables)
  | Table -> IK_Aggregate (inst0, Memory, tick_map_kont m.it.memories)
  | Memory ->
      IK_Exports
        ( inst0,
          fold_left_kont
            m.it.exports
            {exports = NameMap.create (); exports_memory_0 = false} )

let section_inner_kont :
    type kont a b.
    stack_size_limit:int -> module_key -> (kont, a, b) init_section -> a -> kont
    =
 fun ~stack_size_limit self sec x ->
  match sec with
  | Func -> Either.Left x
  | Global -> create_global_kont ~stack_size_limit self x
  | Table -> Left x
  | Memory -> Left x

let section_inner_completed :
    type kont a b. (kont, a, b) init_section -> kont -> b option =
 fun sec kont ->
  match (sec, kont) with
  | Func, Right y -> Some y
  | Global, kont -> create_global_completed kont
  | Table, Right y -> Some y
  | Memory, Right y -> Some y
  | _ -> None

let section_inner_step :
    type kont a b.
    host_funcs:Host_funcs.registry ->
    module_inst ModuleMap.t ->
    module_key ->
    buffers ->
    (kont, a, b) init_section ->
    kont ->
    kont Lwt.t =
 fun ~host_funcs module_reg self buffers ->
  let lift_either f =
    let open Either in
    function
    | Left x ->
        let+ y = f x in
        Right y
    | Right _ -> raise (Init_step_error Section_step)
  in
  function
  | Func -> lift_either (create_func module_reg self)
  | Global -> create_global_step ~host_funcs module_reg buffers
  | Table -> lift_either (fun x -> Lwt.return (create_table x))
  | Memory -> lift_either (fun x -> Lwt.return (create_memory x))

let section_update_module_ref : type kont a b. (kont, a, b) init_section -> bool
    = function
  | Func -> true
  | Global -> false
  | Table -> false
  | Memory -> true

let is_memory_0_export (export : export) =
  match export.it.edesc.it with
  | MemoryExport var ->
      (* Modules may currently only have 1 memory which has index 0. This means
         if memory at index 0 is exported, the entire memory is exported. *)
      Int32.(equal var.it zero)
  | _ -> false

type memory_export_rules = Exports_memory_0 | No_memory_export_rules

exception Missing_memory_0_export

let init_step ~stack_size_limit ~filter_exports
    ?(check_module_exports = No_memory_export_rules) ~module_reg ~self buffers
    host_funcs (m : module_) = function
  | IK_Start exts ->
      (* Initialize as empty module. *)
      update_module_ref module_reg self empty_module_inst ;
      Lwt.return
        (IK_Add_import
           (fold_right2_kont
              m
              empty_module_inst
              (* This is safe as long as we provide an empty list in
                 the PVM. *)
              exts
              m.it.imports))
  | IK_Add_import tick when fold_right2_completed tick ->
      update_module_ref module_reg self tick.acc ;
      Lwt.return (IK_Type (tick.acc, map_kont m.it.types))
  | IK_Add_import tick ->
      let+ tick = fold_right2_step tick (add_import m) in
      IK_Add_import tick
  | IK_Type (inst0, tick) when map_completed tick ->
      let inst0 =
        {inst0 with types = tick.destination; allocations = m.it.allocations}
      in
      update_module_ref module_reg self inst0 ;
      Lwt.return (IK_Aggregate (inst0, Func, tick_map_kont m.it.funcs))
  | IK_Type (inst0, tick) ->
      let+ tick = map_step tick (fun x -> x.it) in
      IK_Type (inst0, tick)
  | IK_Aggregate (inst0, sec, tick) when tick_map_completed tick ->
      Lwt.return
        (IK_Aggregate_concat
           ( inst0,
             sec,
             concat_kont (section_fetch_vec inst0 sec) tick.map.destination ))
  | IK_Aggregate (inst0, sec, tick) ->
      let+ tick =
        tick_map_step
          (section_inner_kont ~stack_size_limit self sec)
          (section_inner_completed sec)
          (section_inner_step ~host_funcs module_reg self buffers sec)
          tick
      in
      IK_Aggregate (inst0, sec, tick)
  | IK_Aggregate_concat (inst0, sec, tick) when concat_completed tick ->
      let inst1 = section_set_vec inst0 sec tick.res in
      if section_update_module_ref sec then
        update_module_ref module_reg self inst1 ;
      Lwt.return (section_next_init_kont m inst1 sec)
  | IK_Aggregate_concat (inst0, sec, tick) ->
      let+ tick = concat_step tick in
      IK_Aggregate_concat (inst0, sec, tick)
  | IK_Exports (inst0, tick) when fold_left_completed tick ->
      (match check_module_exports with
      | Exports_memory_0 ->
          if not tick.acc.exports_memory_0 then raise Missing_memory_0_export
      | No_memory_export_rules -> ()) ;
      let inst0 = {inst0 with exports = tick.acc.exports} in
      Lwt.return (IK_Elems (inst0, tick_map_kont m.it.elems))
  | IK_Exports (inst0, tick) ->
      let+ tick =
        fold_left_s_step tick (fun acc export ->
            let+ k, v = create_export inst0 export in
            let is_func = function ExternFunc _ -> true | _ -> false in
            let exports =
              let filter = filter_exports && not (is_func v) in
              if filter then acc.exports else NameMap.set k v acc.exports
            in
            {
              exports;
              exports_memory_0 =
                acc.exports_memory_0 || is_memory_0_export export;
            })
      in
      IK_Exports (inst0, tick)
  | IK_Elems (inst0, tick) when tick_map_completed tick ->
      let inst0 = {inst0 with elems = tick.map.destination} in
      Lwt.return (IK_Datas (inst0, map_kont m.it.datas))
  | IK_Elems (inst0, tick) ->
      let+ tick =
        tick_map_step
          create_elem_kont
          create_elem_completed
          (create_elem_step
             ~host_funcs
             ~stack_size_limit
             ~module_reg
             buffers
             self)
          tick
      in
      IK_Elems (inst0, tick)
  | IK_Datas (inst0, tick) when map_completed tick ->
      let inst = {inst0 with datas = tick.destination} in
      update_module_ref module_reg self inst ;
      Lwt.return (IK_Es_elems (inst, map_concat_kont m.it.elems))
  | IK_Datas (inst0, tick) ->
      let+ tick = map_step tick create_data in
      IK_Datas (inst0, tick)
  | IK_Es_elems (inst0, tick) -> (
      match map_concat_completed tick with
      | Some es_elem ->
          Lwt.return (IK_Es_datas (inst0, map_concat_kont m.it.datas, es_elem))
      | None ->
          let+ tick =
            map_concat_step
              (fun tick ->
                map_i_step tick (fun i x -> run_elem i x |> Vector.of_list))
              tick
          in
          IK_Es_elems (inst0, tick))
  | IK_Es_datas (inst0, tick, es_elem) -> (
      match map_concat_completed tick with
      | Some es_data ->
          let es_start =
            Vector.of_list
              (Lib.Option.get (Lib.Option.map run_start m.it.start) [])
          in
          let v = Vector.of_list [es_elem; es_data; es_start] in
          Lwt.return (IK_Join_admin (inst0, join_kont v))
      | None ->
          let+ tick =
            map_concat_step
              (fun tick ->
                map_i_s_step tick (fun i x ->
                    (* [of_list] is safe, because [run_elem] always
                       produce lists of length 0 or 5. *)
                    let+ x = run_data inst0 i x in
                    Vector.of_list x))
              tick
          in
          IK_Es_datas (inst0, tick, es_elem))
  | IK_Join_admin (inst0, tick) -> (
      match join_completed tick with
      | Some res ->
          Lwt.return
            (IK_Eval (config ~stack_size_limit self (Vector.empty ()) res))
      | None ->
          let+ tick = join_step tick in
          IK_Join_admin (inst0, tick))
  | IK_Eval {step_kont = SK_Result _; _} ->
      (* No more admin instr, which means that we have returned from
         the _start function. *)
      Lwt.return IK_Stop
  | IK_Eval {step_kont = SK_Trapped {it = msg; at}; _} -> Trap.error at msg
  | IK_Eval config ->
      let+ _, config = step ~init:true ~host_funcs module_reg config buffers in
      IK_Eval config
  | IK_Stop -> raise (Init_step_error Init_step)

let init ?(stack_size_limit = 300) ~module_reg ~self buffers host_funcs
    (m : module_) (exts : extern list) : module_inst Lwt.t =
  let open Lwt.Syntax in
  let rec go = function
    | IK_Stop -> resolve_module_ref module_reg self
    | kont ->
        let* kont =
          init_step
            ~stack_size_limit
            ~filter_exports:false
            ~module_reg
            ~self
            buffers
            host_funcs
            m
            kont
        in
        go kont
  in
  go (IK_Start (Vector.of_list exts))
