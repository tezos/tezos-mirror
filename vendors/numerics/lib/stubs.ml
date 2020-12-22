open Bigarray

type float64_vec = (float, Bigarray.float64_elt, Bigarray.c_layout) Array1.t
type float64_mat = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t
type complex64_vec = (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Array1.t
type complex64_mat = (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Array2.t

external float64_vec_add : dst:float64_vec -> src1:float64_vec -> src2:float64_vec -> unit = "caml_float64_vec_add_"
external float64_mat_add : dst:float64_mat -> src1:float64_mat -> src2:float64_mat -> unit = "caml_float64_vec_add_"
external complex64_vec_add : dst:complex64_vec -> src1:complex64_vec -> src2:complex64_vec -> unit = "caml_complex64_vec_add_"
external complex64_mat_add : dst:complex64_mat -> src1:complex64_mat -> src2:complex64_mat -> unit = "caml_complex64_vec_add_"

external float64_vec_mul : dst:float64_vec -> src1:float64_vec -> src2:float64_vec -> unit = "caml_float64_vec_mul_"
external float64_mat_mul : dst:float64_mat -> src1:float64_mat -> src2:float64_mat -> unit = "caml_float64_vec_mul_"
external complex64_vec_mul : dst:complex64_vec -> src1:complex64_vec -> src2:complex64_vec -> unit = "caml_complex64_vec_mul_"
external complex64_mat_mul : dst:complex64_mat -> src1:complex64_mat -> src2:complex64_mat -> unit = "caml_complex64_vec_mul_"

external float64_vec_div_scalar : v:float64_vec -> x:float -> unit = "caml_float64_vec_div_scalar_"
external complex64_vec_div_scalar : v:complex64_vec -> x:float -> unit = "caml_complex64_vec_div_scalar_"

(* forward *)
external owl_float64_rfftf : float64_vec -> complex64_vec -> int -> unit = "float64_rfftf"

(* backward *)
external owl_float64_rfftb : complex64_vec -> float64_vec -> int -> unit = "float64_rfftb"
