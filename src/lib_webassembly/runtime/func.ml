open Types

type 'inst t = 'inst func

and 'inst func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of func_type * string

let alloc ft inst f = AstFunc (ft, inst, f)

let alloc_host ~global_name ft = HostFunc (ft, global_name)

let type_of = function AstFunc (ft, _, _) -> ft | HostFunc (ft, _) -> ft
