type typ = Void | Int | Buffer | Uint8 | Uint32 | Bool

type arg_name = string

type arg = {
  name : arg_name;
  kind : [`Input | `Output];
  typ : typ;
  index : int option;
  size : [`Absolute of int | `Relative of arg_name * int] option;
}

type t = {
  js_mod_name : string;
  js_fun_name : string;
  wasm_fun_name : string;
  return : typ;
  args : arg list;
}

val parse_file : string -> t list
