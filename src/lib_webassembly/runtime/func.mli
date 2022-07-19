open Types
open Values

type ('input, 'inst) host_func_desc = {
  module_name : string;
  func_name : string;
  func_type : func_type;
  implem : 'input -> 'inst -> value list -> value list Lwt.t;
}

type ('input, 'inst) t = ('input, 'inst) func

and ('input, 'inst) func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of ('input, 'inst) host_func_desc

val alloc : func_type -> 'inst -> Ast.func -> ('input, 'inst) func

val alloc_host :
  module_name:string ->
  func_name:string ->
  func_type ->
  ('input -> 'inst -> value list -> value list Lwt.t) ->
  ('input, 'inst) func

val type_of : ('input, 'inst) func -> func_type
