module F = Dense_float64_vec
module C = Dense_complex64_vec

let rfft : F.t -> C.t =
  fun x ->
  let dim = F.length x / 2 + 1 in
  let y = C.create_ dim in
  Stubs.owl_float64_rfftf x y 0;
  y


let irfft : C.t -> F.t =
  fun x ->
  let dim = (F.length x - 1) * 2 in
  let y = F.create_ dim in
  Stubs.owl_float64_rfftb x y 0;
  let norm = float_of_int dim in
  Stubs.float64_vec_div_scalar ~v:y ~x:norm;
  y
