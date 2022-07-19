open Types

type host_func_desc = {
  module_name : string;
  func_name : string;
  func_type : func_type;
      (*  implem : 'input -> 'inst -> value list -> value list Lwt.t;*)
}

type 'inst t = 'inst func

and 'inst func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of host_func_desc

val alloc : func_type -> 'inst -> Ast.func -> 'inst func

val alloc_host :
  module_name:string -> func_name:string -> func_type -> 'inst func

val type_of : 'inst func -> func_type
