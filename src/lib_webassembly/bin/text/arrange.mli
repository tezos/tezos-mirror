open Sexpr
module Vector := Lazy_vector.Int32Vector

val instr : Ast.instr Vector.t Vector.t -> Ast.instr -> sexpr

val func : Ast.instr Vector.t Vector.t -> Ast.func -> sexpr

val module_ : Ast.module_ -> sexpr Lwt.t

val script : [`Textual | `Binary] -> Script.script -> sexpr list Lwt.t
