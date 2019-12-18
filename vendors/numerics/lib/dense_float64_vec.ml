open Bigarray

type elt = float
type t = (float, float64_elt, c_layout) Array1.t

let create_ dim =
  Array1.create Float64 C_layout dim

let create dim =
  let arr = create_ dim in
  for i = 0 to dim - 1 do
    Array1.unsafe_set arr i 0.0
  done ;
  arr

let length = Array1.dim

let copy arr =
  let len  = length arr in
  let arr' = create len in
  Array1.blit arr arr' ;
  arr'

let init dim f =
  let arr = create dim in
  for i = 0 to dim - 1 do
    Array1.unsafe_set arr i (f i)
  done ;
  arr

let get = Array1.get

let unsafe_get = Array1.unsafe_get

let set = Array1.set

let unsafe_set = Array1.unsafe_set

let concat x y =
  let xlen = length x
  and ylen = length y in
  let res  = create (xlen + ylen) in
  let tgtx = let offs = 0 and len = xlen in Array1.sub res offs len in
  let tgty = let offs = xlen and len = ylen in Array1.sub res offs len in
  Array1.blit x tgtx ;
  Array1.blit y tgty ;
  res

let of_array (x : float array) =
  Array1.of_array Float64 C_layout x

let to_array (x : t) =
  Array.init (length x) (fun i -> unsafe_get x i)

let alloc_same_dims (x : t) =
  create_ (length x)

let add x y =
  let dst = alloc_same_dims x in
  Stubs.float64_vec_add ~dst ~src1:x ~src2:y ;
  dst

let mul x y =
  let dst = alloc_same_dims x in
  Stubs.float64_vec_mul ~dst ~src1:x ~src2:y ;
  dst

let linspace start stop count =
  assert (count > 1) ;
  let res = create_ count in
  let delta = (stop -. start) /. (float (count - 1)) in
  let f = ref start in
  for i = 0 to count - 1 do
    unsafe_set res i !f ;
    f := !f +. delta
  done ;
  res
