module Fq12_stubs = Rustc_bls12_381_bindings.Fq12 (Rustc_bls12_381_stubs)

let size = 576

type t = Bytes.t

let empty () = Bytes.create size

let order =
  let fq_order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
  in
  Z.pow fq_order 12

let check_bytes bs =
  if Bytes.length bs = size then
    Fq12_stubs.check_bytes (Ctypes.ocaml_bytes_start bs)
  else false

let of_bytes_opt bs = if check_bytes bs then Some bs else None

let of_bytes g =
  assert (Bytes.length g = size) ;
  g

let to_bytes s = s

let zero () =
  let g = Bytes.create size in
  Fq12_stubs.zero (Ctypes.ocaml_bytes_start g) ;
  g

let one () =
  let g = Bytes.create size in
  Fq12_stubs.one (Ctypes.ocaml_bytes_start g) ;
  g

let is_zero g = Fq12_stubs.is_zero (Ctypes.ocaml_bytes_start g)

let is_one g = Fq12_stubs.is_one (Ctypes.ocaml_bytes_start g)

let random () =
  let g = Bytes.create size in
  Fq12_stubs.random (Ctypes.ocaml_bytes_start g) ;
  g

let add g1 g2 =
  assert (Bytes.length g1 = size) ;
  assert (Bytes.length g2 = size) ;
  let g = Bytes.create size in
  Fq12_stubs.add
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start g1)
    (Ctypes.ocaml_bytes_start g2) ;
  g

let mul g1 g2 =
  assert (Bytes.length g1 = size) ;
  assert (Bytes.length g2 = size) ;
  let g = Bytes.create size in
  Fq12_stubs.mul
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start g1)
    (Ctypes.ocaml_bytes_start g2) ;
  g

let eq g1 g2 =
  assert (Bytes.length g1 = size) ;
  assert (Bytes.length g2 = size) ;
  Fq12_stubs.eq (Ctypes.ocaml_bytes_start g1) (Ctypes.ocaml_bytes_start g2)

let negate g =
  assert (Bytes.length g = size) ;
  let opposite_buffer = Bytes.create size in
  Fq12_stubs.negate
    (Ctypes.ocaml_bytes_start opposite_buffer)
    (Ctypes.ocaml_bytes_start g) ;
  opposite_buffer

let square g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fq12_stubs.square
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  buffer

let double g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fq12_stubs.double
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  buffer

let inverse g =
  assert (Bytes.length g = size) ;
  let inverse_buffer = Bytes.create size in
  Fq12_stubs.unsafe_inverse
    (Ctypes.ocaml_bytes_start inverse_buffer)
    (Ctypes.ocaml_bytes_start g) ;
  inverse_buffer

let inverse_opt g =
  if is_zero g then None
  else
    let inverse_buffer = Bytes.create size in
    Fq12_stubs.unsafe_inverse
      (Ctypes.ocaml_bytes_start inverse_buffer)
      (Ctypes.ocaml_bytes_start g) ;
    Some inverse_buffer

let pow x n =
  let res = empty () in
  let n = Z.erem n (Z.pred order) in
  (* sign is removed by to_bits, but that's fine because we used mod before *)
  let n = Bytes.of_string (Z.to_bits n) in
  let bytes_size_n = Bytes.length n in
  let padded_n =
    Bytes.init size (fun i ->
        if i < bytes_size_n then Bytes.get n i else char_of_int 0)
  in
  Fq12_stubs.pow
    (Ctypes.ocaml_bytes_start res)
    (Ctypes.ocaml_bytes_start (to_bytes x))
    (Ctypes.ocaml_bytes_start padded_n) ;
  res

let of_z x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
  let x0 = Bytes.of_string (Z.to_bits x0) in
  let x1 = Bytes.of_string (Z.to_bits x1) in
  let x2 = Bytes.of_string (Z.to_bits x2) in
  let x3 = Bytes.of_string (Z.to_bits x3) in
  let x4 = Bytes.of_string (Z.to_bits x4) in
  let x5 = Bytes.of_string (Z.to_bits x5) in
  let x6 = Bytes.of_string (Z.to_bits x6) in
  let x7 = Bytes.of_string (Z.to_bits x7) in
  let x8 = Bytes.of_string (Z.to_bits x8) in
  let x9 = Bytes.of_string (Z.to_bits x9) in
  let x10 = Bytes.of_string (Z.to_bits x10) in
  let x11 = Bytes.of_string (Z.to_bits x11) in
  let g = empty () in
  Bytes.blit x0 0 g 0 (min (Bytes.length x0) 48) ;
  Bytes.blit x1 0 g 48 (min (Bytes.length x1) 48) ;
  Bytes.blit x2 0 g 96 (min (Bytes.length x2) 48) ;
  Bytes.blit x3 0 g 144 (min (Bytes.length x3) 48) ;
  Bytes.blit x4 0 g 192 (min (Bytes.length x4) 48) ;
  Bytes.blit x5 0 g 240 (min (Bytes.length x5) 48) ;
  Bytes.blit x6 0 g 288 (min (Bytes.length x6) 48) ;
  Bytes.blit x7 0 g 336 (min (Bytes.length x7) 48) ;
  Bytes.blit x8 0 g 384 (min (Bytes.length x8) 48) ;
  Bytes.blit x9 0 g 432 (min (Bytes.length x9) 48) ;
  Bytes.blit x10 0 g 480 (min (Bytes.length x10) 48) ;
  Bytes.blit x11 0 g 528 (min (Bytes.length x11) 48) ;
  of_bytes g

let of_string x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
  let x0 = Bytes.of_string (Z.to_bits (Z.of_string x0)) in
  let x1 = Bytes.of_string (Z.to_bits (Z.of_string x1)) in
  let x2 = Bytes.of_string (Z.to_bits (Z.of_string x2)) in
  let x3 = Bytes.of_string (Z.to_bits (Z.of_string x3)) in
  let x4 = Bytes.of_string (Z.to_bits (Z.of_string x4)) in
  let x5 = Bytes.of_string (Z.to_bits (Z.of_string x5)) in
  let x6 = Bytes.of_string (Z.to_bits (Z.of_string x6)) in
  let x7 = Bytes.of_string (Z.to_bits (Z.of_string x7)) in
  let x8 = Bytes.of_string (Z.to_bits (Z.of_string x8)) in
  let x9 = Bytes.of_string (Z.to_bits (Z.of_string x9)) in
  let x10 = Bytes.of_string (Z.to_bits (Z.of_string x10)) in
  let x11 = Bytes.of_string (Z.to_bits (Z.of_string x11)) in
  let g = empty () in
  Bytes.blit x0 0 g 0 (min (Bytes.length x0) 48) ;
  Bytes.blit x1 0 g 48 (min (Bytes.length x1) 48) ;
  Bytes.blit x2 0 g 96 (min (Bytes.length x2) 48) ;
  Bytes.blit x3 0 g 144 (min (Bytes.length x3) 48) ;
  Bytes.blit x4 0 g 192 (min (Bytes.length x4) 48) ;
  Bytes.blit x5 0 g 240 (min (Bytes.length x5) 48) ;
  Bytes.blit x6 0 g 288 (min (Bytes.length x6) 48) ;
  Bytes.blit x7 0 g 336 (min (Bytes.length x7) 48) ;
  Bytes.blit x8 0 g 384 (min (Bytes.length x8) 48) ;
  Bytes.blit x9 0 g 432 (min (Bytes.length x9) 48) ;
  Bytes.blit x10 0 g 480 (min (Bytes.length x10) 48) ;
  Bytes.blit x11 0 g 528 (min (Bytes.length x11) 48) ;
  of_bytes g
