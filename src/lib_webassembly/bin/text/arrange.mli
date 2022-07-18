open Sexpr
module Vector := Lazy_vector.Int32Vector

type block_table = Ast.instr Vector.t Vector.t

val instr : block_table -> Ast.instr -> sexpr

val func : block_table -> Ast.func -> sexpr

val module_ : Ast.module_ -> sexpr Lwt.t

val script : [`Textual | `Binary] -> Script.script -> sexpr list Lwt.t
