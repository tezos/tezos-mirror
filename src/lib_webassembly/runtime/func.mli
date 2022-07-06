open Types
open Values

type ('input, 'inst) t = ('input, 'inst) func

and ('input, 'inst) func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of func_type * ('input -> 'inst -> value list -> value list Lwt.t)

val alloc : func_type -> 'inst -> Ast.func -> ('input, 'inst) func

val alloc_host :
  func_type ->
  ('input -> 'inst -> value list -> value list Lwt.t) ->
  ('input, 'inst) func

val type_of : ('input, 'inst) func -> func_type
