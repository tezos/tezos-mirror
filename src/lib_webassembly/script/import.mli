exception Unknown of Source.region * string

val link : Ast.module_ -> Instance.extern list Lwt.t (* raises Unknown *)

val register :
  module_name:Ast.name ->
  (Ast.name -> Instance.extern Lwt.t (* raises Not_found *)) ->
  unit
