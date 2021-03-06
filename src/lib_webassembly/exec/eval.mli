open Values
open Instance

exception Link of Source.region * string

exception Trap of Source.region * string

exception Crash of Source.region * string

exception Exhaustion of Source.region * string

val init :
  Host_funcs.registry ->
  Ast.module_ ->
  extern list ->
  module_inst Lwt.t (* raises Link, Trap *)

val invoke :
  ?module_inst:module_inst ->
  ?input:Input_buffer.t ->
  Host_funcs.registry ->
  func_inst ->
  value list ->
  (module_inst * value list) Lwt.t (* raises Trap *)
