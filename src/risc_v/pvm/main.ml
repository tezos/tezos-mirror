open Api
open Ctypes

let add (x : int) (y : int) =
  let x_ptr = Uintptr.of_int x in
  let y_ptr = Uintptr.of_int y in
  let res = Functions.add x_ptr y_ptr in
  Uintptr.to_int res
