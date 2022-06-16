open Sexpr

val instr : (Ast.block_label -> Ast.instr list) -> Ast.instr -> sexpr

val func : (Ast.block_label -> Ast.instr list) -> Ast.func -> sexpr

val module_ : Ast.module_ -> sexpr Lwt.t

val script : [`Textual | `Binary] -> Script.script -> sexpr list Lwt.t
