open Sigs
open Bigarray

module Float64 =
struct
  module Vec : (Vector_sig with type elt = float
                            and type t = (float, float64_elt , c_layout) Array1.t) = Dense_float64_vec

  module Mat : (Matrix_sig with type elt = float
                            and type t = (float, float64_elt , c_layout) Array2.t) = Dense_float64_mat

  let vec : (float, (float, float64_elt, c_layout) Array1.t) vector_impl = (module Vec)
  let mat : (float, (float, float64_elt, c_layout) Array2.t) matrix_impl = (module Mat)
end

module Complex64 =
struct
  module Vec : (Vector_sig with type elt = Complex.t
                            and type t = (Complex.t, complex64_elt , c_layout) Array1.t) = Dense_complex64_vec

  module Mat : (Matrix_sig with type elt = Complex.t
                            and type t = (Complex.t, complex64_elt , c_layout) Array2.t) = Dense_complex64_mat

  let vec : (Complex.t, (Complex.t, complex64_elt, c_layout) Array1.t) vector_impl = (module Vec)
  let mat : (Complex.t, (Complex.t, complex64_elt, c_layout) Array2.t) matrix_impl = (module Mat)
end

module Fft = Fft

module Sigs = Sigs
