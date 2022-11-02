open Types

type 'inst t = 'inst func

and 'inst func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of func_type * string

val alloc : func_type -> 'inst -> Ast.func -> 'inst func

val alloc_host : global_name:string -> func_type -> 'inst func

val type_of : 'inst func -> func_type
