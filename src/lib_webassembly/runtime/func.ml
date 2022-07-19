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

let alloc ft inst f = AstFunc (ft, inst, f)

let alloc_host ~module_name ~func_name ft f =
  HostFunc {module_name; func_name; func_type = ft; implem = f}

let type_of = function
  | AstFunc (ft, _, _) -> ft
  | HostFunc {func_type = ft; _} -> ft
