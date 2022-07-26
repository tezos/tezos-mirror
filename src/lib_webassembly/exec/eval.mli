open Values
open Instance

exception Link of Source.region * string

exception Trap of Source.region * string

exception Crash of Source.region * string

exception Exhaustion of Source.region * string

val init :
  self:module_ref ->
  Host_funcs.registry ->
  Ast.module_ ->
  extern list ->
  module_inst Lwt.t (* raises Link, Trap *)

val invoke :
  ?caller:module_ref ->
  ?input:Input_buffer.t ->
  Host_funcs.registry ->
  func_inst ->
  value list ->
  value list Lwt.t (* raises Trap *)
