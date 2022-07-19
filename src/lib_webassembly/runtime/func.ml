open Types

type host_func_desc = {
  module_name : string;
  func_name : string;
  func_type : func_type;
}

type 'inst t = 'inst func

and 'inst func =
  | AstFunc of func_type * 'inst * Ast.func
  | HostFunc of host_func_desc

let alloc ft inst f = AstFunc (ft, inst, f)

let alloc_host ~module_name ~func_name ft =
  HostFunc {module_name; func_name; func_type = ft}

let type_of = function
  | AstFunc (ft, _, _) -> ft
  | HostFunc {func_type = ft; _} -> ft
