exception Code of Source.region * string

val version : int32

val encode : Ast.module_ -> string Lwt.t

val encode_custom : Ast.name -> string -> string
