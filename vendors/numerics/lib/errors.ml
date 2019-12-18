
type error =
  | Column_out_of_bounds of { shape : int * int ; col : int }


exception Numerics_error of error

let raise err = raise (Numerics_error err)
