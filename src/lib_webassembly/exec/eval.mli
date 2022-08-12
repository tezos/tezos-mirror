open Values
open Instance

exception Link of Source.region * string

exception Trap of Source.region * string

exception Crash of Source.region * string

exception Exhaustion of Source.region * string

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
