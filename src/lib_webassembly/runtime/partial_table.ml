open Types
open Values

type size = int32

type index = int32

type count = int32

module Vector = Lazy_vector.Mutable.LwtInt32Vector

type table = {mutable ty : table_type; content : ref_ Vector.t}

type t = table

let content {content; _} = Vector.snapshot content

include Memory_exn

let valid_limits {min; max} =
  match max with None -> true | Some m -> I32.le_u min m

let create size r =
  try Vector.create ~produce_value:(fun _ -> Lwt.return r) size
  with Out_of_memory | Invalid_argument _ -> raise OutOfMemory

let create_shallow size =
  try Vector.create size
  with Out_of_memory | Invalid_argument _ -> raise OutOfMemory

let alloc (TableType (lim, _) as ty) r =
  if not (valid_limits lim) then raise Type ;
  {ty; content = create lim.min r}

let of_lazy_vector (TableType (lim, _) as ty) content =
  if not (valid_limits lim) then raise Type ;
  {ty; content = Vector.of_immutable content}

let alloc_shallow (TableType (lim, _) as ty) =
  if not (valid_limits lim) then raise Type ;
  {ty; content = create_shallow lim.min}

let size tab = Vector.num_elements tab.content

let type_of tab = tab.ty

let grow tab delta r =
  let (TableType (lim, t)) = tab.ty in
  assert (lim.min = size tab) ;
  let old_size = lim.min in
  let new_size = Int32.add old_size delta in
  if I32.gt_u old_size new_size then raise SizeOverflow
  else
    let lim' = {lim with min = new_size} in
    if not (valid_limits lim') then raise SizeLimit
    else Vector.grow delta ~produce_value:(fun _ -> Lwt.return r) tab.content ;
    tab.ty <- TableType (lim', t) ;
    ()

let load tab i = Vector.get i tab.content

let store tab i r =
  let (TableType (lim, t)) = tab.ty in
  if type_of_ref r <> t then raise Type ;
  Vector.set i r tab.content

let blit tab offset rs =
  List.iteri (fun i r -> store tab Int32.(of_int i |> add offset) r) rs
