open Types
open Values

type ('input, 'inst) t = ('input, 'inst) func

and ('input, 'inst) func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of func_type * ('input -> 'inst -> value list -> value list Lwt.t)

let alloc ft inst f = AstFunc (ft, inst, f)

let alloc_host ft f = HostFunc (ft, f)

let type_of = function AstFunc (ft, _, _) -> ft | HostFunc (ft, _) -> ft
