open Values
open Instance

exception Link of Source.region * string

exception Trap of Source.region * string

exception Crash of Source.region * string

exception Exhaustion of Source.region * string

type ('a, 'b, 'acc) fold_right2_kont = {
  acc : 'acc;
  lv : 'a Vector.t;
  rv : 'b Vector.t;
  offset : int32;
}

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

type ('a, 'b) fold_left_kont = {origin : 'a Vector.t; acc : 'b; offset : int32}

type (_, _) init_section =
  | Func : (Ast.func, func_inst) init_section
  | Global : (Ast.global, global_inst) init_section
  | Table : (Ast.table, table_inst) init_section
  | Memory : (Ast.memory, memory_inst) init_section

type init_kont =
  | IK_Start  (** Very first tick of the [init] function *)
  | IK_Add_import of (extern, Ast.import, module_inst) fold_right2_kont
  | IK_Type of module_inst * (Ast.type_, Types.func_type) map_kont
  | IK_Aggregate :
      module_inst * ('a, 'b) init_section * ('a, 'b) map_kont
      -> init_kont
  | IK_Aggregate_concat :
      module_inst * ('a, 'b) init_section * 'b concat_kont
      -> init_kont
  | IK_Exports of module_inst * (Ast.export, extern NameMap.t) fold_left_kont
  | IK_Remaining of module_inst
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
  Host_funcs.registry ->
  func_inst ->
  value list ->
  value list Lwt.t (* raises Trap *)

type frame = {inst : module_key; locals : value ref list}

type code = value list * admin_instr list

and admin_instr = admin_instr' Source.phrase

and admin_instr' =
  | From_block of Ast.block_label * int32
  | Plain of Ast.instr'
  | Refer of ref_
  | Invoke of func_inst
  | Trapping of string
  | Returning of value list
  | Breaking of int32 * value list
  | Label of int32 * Ast.instr list * code
  | Frame of int32 * frame * code

type config = {
  frame : frame;
  input : input_inst;
  code : code;
  host_funcs : Host_funcs.registry;
  budget : int; (* to model stack overflow *)
}

val step : module_reg -> config -> config Lwt.t

val config :
  ?input:input_inst ->
  Host_funcs.registry ->
  module_key ->
  value list ->
  admin_instr list ->
  config
