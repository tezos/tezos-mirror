open Values
open Instance

(* Kontinuation *)

type ('a, 'b) map_kont = {
  origin : 'a Vector.t;
  destination : 'b Vector.t;
  offset : int32;
}

type 'a concat_kont = {
  lv : 'a Vector.t;
  rv : 'a Vector.t;
  res : 'a Vector.t;
  offset : int32;
}

exception Link of Source.region * string

exception Trap of Source.region * string

exception Crash of Source.region * string

exception Exhaustion of Source.region * string

(** Possible states of the small-step initializer, used for error reporting. *)
type init_state =
  | Init_step
  | Map_step
  | Map_concat_step
  | Join_step
  | Section_step

(** Exception raised on irreducible states of the small step
    initialization. *)
exception Init_step_error of init_state

type frame = {inst : module_key; locals : value ref Vector.t}

type admin_instr = admin_instr' Source.phrase

and admin_instr' =
  | From_block of Ast.block_label * int32
  | Plain of Ast.instr'
  | Refer of ref_
  | Invoke of func_inst
  | Trapping of string
  | Returning of value Vector.t
  | Breaking of int32 * value Vector.t

type code = value Vector.t * admin_instr Vector.t

type label = {
  label_arity : int32 option;
  label_frame_specs : frame;
  label_break : Ast.instr option;
  label_code : code;
}

type ongoing = Ongoing_kind

type finished = Finished_kind

type _ label_kont =
  | Label_stack : label * label Vector.t -> ongoing label_kont
  | Label_result : value Vector.t -> finished label_kont
  | Label_trapped : string Source.phrase -> finished label_kont

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
      func : Ast.func;
      locals_kont : (Types.value_type, value ref) map_kont;
    }
  | Inv_prepare_args of {
      arity : int32;
      vs : value Vector.t;
      instructions : admin_instr Vector.t;
      inst : module_key;
      func : Ast.func;
      locals : value ref Vector.t;
      args_kont : (value, value ref) map_kont;
    }
  | Inv_concat of {
      arity : int32;
      vs : value Vector.t;
      instructions : admin_instr Vector.t;
      inst : module_key;
      func : Ast.func;
      concat_kont : value ref concat_kont;
    }
  | Inv_stop of {code : code; fresh_frame : ongoing frame_stack option}

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
  | SK_Trapped of string Source.phrase

type config = {
  input : input_inst;
  output : output_inst;
  step_kont : step_kont;
  host_funcs : Host_funcs.registry;
  stack_size_limit : int;
}

type ('a, 'b, 'acc) fold_right2_kont = {
  acc : 'acc;
  lv : 'a Vector.t;
  rv : 'b Vector.t;
  offset : int32;
}

type ('a, 'b) fold_left_kont = {origin : 'a Vector.t; acc : 'b; offset : int32}

type eval_const_kont = EC_Next of config | EC_Stop of value

type create_global_kont = Types.global_type * eval_const_kont

type (_, _, _) init_section =
  | Func : ((Ast.func, func_inst) Either.t, Ast.func, func_inst) init_section
  | Global : (create_global_kont, Ast.global, global_inst) init_section
  | Table
      : ((Ast.table, table_inst) Either.t, Ast.table, table_inst) init_section
  | Memory
      : ( (Ast.memory, memory_inst) Either.t,
          Ast.memory,
          memory_inst )
        init_section

type 'b join_kont =
  | J_Init of 'b Vector.t Vector.t
  | J_Next of 'b concat_kont * 'b Vector.t Vector.t
  | J_Stop of 'b Vector.t

type ('a, 'b) map_concat_kont =
  | MC_Map of ('a, 'b Vector.t) map_kont
  | MC_Join of 'b join_kont

type ('kont, 'a, 'b) tick_map_kont = {
  tick : 'kont option;
  map : ('a, 'b) map_kont;
}

type create_elem_kont = (eval_const_kont, Ast.const, ref_) tick_map_kont

type init_kont =
  | IK_Start  (** Very first tick of the [init] function *)
  | IK_Add_import of (extern, Ast.import, module_inst) fold_right2_kont
  | IK_Type of module_inst * (Ast.type_, Types.func_type) map_kont
  | IK_Aggregate :
      module_inst * ('kont, 'a, 'b) init_section * ('kont, 'a, 'b) tick_map_kont
      -> init_kont
  | IK_Aggregate_concat :
      module_inst * ('kont, 'a, 'b) init_section * 'b concat_kont
      -> init_kont
  | IK_Exports of module_inst * (Ast.export, extern NameMap.t) fold_left_kont
  | IK_Elems of
      module_inst
      * (create_elem_kont, Ast.elem_segment, elem_inst) tick_map_kont
  | IK_Datas of module_inst * (Ast.data_segment, data_inst) map_kont
  | IK_Es_elems of module_inst * (Ast.elem_segment, admin_instr) map_concat_kont
  | IK_Es_datas of
      module_inst
      * (Ast.data_segment, admin_instr) map_concat_kont
      * admin_instr Vector.t
  | IK_Join_admin of module_inst * admin_instr join_kont
  | IK_Eval of module_inst * config
  | IK_Stop of module_inst
      (** Witness that there is no more tick to execute to complete
          the [init] process. *)

(** Small-step execution of the [init] process. See {!init}.

    @raise Invalid_argument if called with [IK_Stop]. There is no
    transition from the terminal state. *)
val init_step :
  module_reg:module_reg ->
  self:module_key ->
  Host_funcs.registry ->
  Ast.module_ ->
  extern list ->
  init_kont ->
  init_kont Lwt.t

val init :
  module_reg:module_reg ->
  self:module_key ->
  Host_funcs.registry ->
  Ast.module_ ->
  extern list ->
  module_inst Lwt.t (* raises Link, Trap *)

val invoke :
  module_reg:module_reg ->
  caller:module_key ->
  ?input:Input_buffer.t ->
  ?output:Output_buffer.t ->
  Host_funcs.registry ->
  func_inst ->
  value list ->
  value list Lwt.t (* raises Trap *)

val step : module_reg -> config -> config Lwt.t

val config :
  ?input:input_inst ->
  ?output:output_inst ->
  Host_funcs.registry ->
  ?frame_arity:int32 (* The number of values returned by the computation *) ->
  module_key ->
  value Vector.t ->
  admin_instr Vector.t ->
  config
