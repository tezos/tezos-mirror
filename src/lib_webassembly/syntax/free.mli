module Set : Set.S with type elt = int32

type t = {
  types : Set.t;
  globals : Set.t;
  tables : Set.t;
  memories : Set.t;
  funcs : Set.t;
  elems : Set.t;
  datas : Set.t;
  locals : Set.t;
  labels : Set.t;
}

val empty : t

val func : Ast.block_table -> Ast.func -> t Lwt.t

val module_ : Ast.module_ -> t Lwt.t

val list : ('a -> t) -> 'a list -> t
