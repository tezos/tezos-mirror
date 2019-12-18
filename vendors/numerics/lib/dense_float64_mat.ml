open Bigarray

type elt = float
type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t

let create_ lines cols =
  Array2.create Bigarray.Float64 Bigarray.C_layout lines cols

let create ~(lines : int) ~(cols : int) =
  let mat = create_ lines cols in
  Array2.fill mat 0.0 ;
  mat

let init ~(lines : int) ~(cols : int) ~f =
  let mat = Array2.create Bigarray.Float64 Bigarray.C_layout lines cols in
  for i = 0 to lines - 1 do
    for j = 0 to cols - 1 do
      Array2.set mat i j (f i j)
    done
  done ;
  mat

let dim1 (mat : t) =
  Array2.dim1 mat

let dim2 (mat : t) =
  Array2.dim2 mat

let shape (mat : t) = (dim1 mat, dim2 mat)

let set (mat : t) i j v =
  Array2.set mat i j v

let get (mat : t) i j =
  Array2.get mat i j

let unsafe_set (mat : t) i j v =
  Array2.unsafe_set mat i j v

let unsafe_get (mat : t) i j =
  Array2.unsafe_get mat i j

let map (mat : t) (f : float -> float) =
  let res = create ~lines:(dim1 mat) ~cols:(dim2 mat) in
  for i = 0 to dim1 mat - 1 do
    for j = 0 to dim2 mat - 1 do
      set res i j (f (get mat i j))
    done
  done ;
  res

let column (mat : t) (i : int) =
  let lines, cols = shape mat in
  if i >= cols then
    Errors.(raise (Column_out_of_bounds { shape = (lines, cols) ; col = i }))
  else
    let col = create ~lines ~cols:1 in
    for k = 0 to lines - 1 do
      set col k 0 (get mat k i)
    done ;
    col

let is_column (m : t) : bool =
  let _, cols = shape m in
  cols = 1

let concat_columns_horiz (columns : t list) : t =
  let cols = List.length columns in
  if cols = 0 then
    invalid_arg "concat_columns_horiz: empty list of columns" ;
  if not (List.for_all is_column columns) then
    invalid_arg "concat_columns_horiz: invalid argument" ;
  let row_dims = List.map dim1 columns in
  match Utils.all_equal row_dims with
  | None ->
    invalid_arg "concat_columns_horiz: invalid argument"
  | Some rows ->
    let columns = Array.of_list columns in
    init ~lines:rows ~cols ~f:(fun l c ->
        get columns.(c) l 0
      )

let to_columns (mat : t) : t list =
  let lines, cols = shape mat in
  List.init cols (fun c ->
      init ~lines ~cols:1 ~f:(fun l _c -> unsafe_get mat l c)
    )

let concat_horiz mat1 mat2 =
  let cols1 = to_columns mat1 in
  let cols2 = to_columns mat2 in
  concat_columns_horiz (cols1 @ cols2)

let add mat1 mat2 =
  let lines1, cols1 = shape mat1 in
  let dst = create_ lines1 cols1 in
  Stubs.float64_mat_add ~dst ~src1:mat1 ~src2:mat2 ;
  dst

let mul mat1 mat2 =
  let lines1, cols1 = shape mat1 in
  let dst = create_ lines1 cols1 in
  Stubs.float64_mat_mul ~dst ~src1:mat1 ~src2:mat2 ;
  dst

let copy mat =
  let lines, cols = shape mat in
  let mat' = create ~lines ~cols in
  Array2.blit mat mat' ;
  mat'
