open Types
open Values

type size = int32
type index = int32
type count = int32

module Map = Lazy_map.Mutable.Make (Int32)

type table = {mutable ty : table_type; mutable content : ref_ Map.t}
type t = table

include Memory_exn

let valid_limits {min; max} =
  match max with
  | None -> true
  | Some m -> I32.le_u min m

let create size r =
  try Map.create ~produce_value:(fun _ -> r) size
  with Out_of_memory | Invalid_argument _ -> raise OutOfMemory

let alloc (TableType (lim, _) as ty) r =
  if not (valid_limits lim) then raise Type;
  {ty; content = create lim.min r}

let size tab =
  Map.num_elements tab.content

let type_of tab =
  tab.ty

let grow tab delta r =
  let TableType (lim, t) = tab.ty in
  assert (lim.min = size tab);
  let old_size = lim.min in
  let new_size = Int32.add old_size delta in
  if I32.gt_u old_size new_size then raise SizeOverflow else
  let lim' = {lim with min = new_size} in
  if not (valid_limits lim') then raise SizeLimit else
  Map.grow delta ~produce_value:(fun _ -> r) tab.content;
  tab.ty <- TableType (lim', t);
  ()

let load tab i = Map.get i tab.content

let store tab i r =
  let TableType (lim, t) = tab.ty in
  if type_of_ref r <> t then raise Type;
  Map.set i r tab.content

let blit tab offset rs =
  List.iteri (fun i r -> store tab Int32.(of_int i |> add offset) r) rs
