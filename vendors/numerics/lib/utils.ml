let int_compare x y =
  if x < y then
    -1
  else if x = y then
    0
  else
    1

let all_equal (l : int list) : int option =
  let result =
    List.sort_uniq int_compare l in
  match result with
  | [ uniq ] -> Some uniq
  | _        -> None
