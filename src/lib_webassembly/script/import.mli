exception Unknown of Source.region * string

val link : Ast.module_ -> Instance.extern list Lwt.t (* raises Unknown *)

val register :
  Ast.name ->
  (Ast.name ->
  Types.extern_type ->
  Instance.extern Lwt.t (* raises Not_found *)) ->
  unit
