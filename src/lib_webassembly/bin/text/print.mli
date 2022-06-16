val instr :
  Lwt_io.output_channel ->
  (Ast.block_label -> Ast.instr list) ->
  int ->
  Ast.instr ->
  unit Lwt.t

val func :
  Lwt_io.output_channel ->
  (Ast.block_label -> Ast.instr list) ->
  int ->
  Ast.func ->
  unit Lwt.t

val module_ : Lwt_io.output_channel -> int -> Ast.module_ -> unit Lwt.t

val script :
  Lwt_io.output_channel ->
  int ->
  [`Textual | `Binary] ->
  Script.script ->
  unit Lwt.t
