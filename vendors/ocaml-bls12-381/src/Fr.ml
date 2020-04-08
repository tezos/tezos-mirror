module Fr_stubs = Rustc_bls12_381_bindings.Fr (Rustc_bls12_381_stubs)

(** High level (OCaml) definitions/logic *)
let size = 32

let order =
  Z.of_string
    "52435875175126190479447740508185965837690552500527637822603658699938581184513"

type t = Bytes.t

let empty () = Bytes.create size

let check_bytes bs =
  if Bytes.length bs = size then
    Fr_stubs.check_bytes (Ctypes.ocaml_bytes_start bs)
  else false

let of_bytes_opt bs = if check_bytes bs then Some bs else None

let of_bytes (g : Bytes.t) : t =
  assert (Bytes.length g = size) ;
  g

let to_bytes g = g

let is_zero g =
  assert (Bytes.length g = size) ;
  Fr_stubs.is_zero (Ctypes.ocaml_bytes_start g)

let is_one g =
  assert (Bytes.length g = size) ;
  Fr_stubs.is_one (Ctypes.ocaml_bytes_start g)

let zero () =
  let g = Bytes.create size in
  Fr_stubs.zero (Ctypes.ocaml_bytes_start g) ;
  of_bytes g

let one () =
  let g = Bytes.create size in
  Fr_stubs.one (Ctypes.ocaml_bytes_start g) ;
  of_bytes g

let random () =
  let g = Bytes.create size in
  Fr_stubs.random (Ctypes.ocaml_bytes_start g) ;
  of_bytes g

let add x y =
  assert (Bytes.length x = size) ;
  assert (Bytes.length y = size) ;
  let g = Bytes.create size in
  Fr_stubs.add
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start x)
    (Ctypes.ocaml_bytes_start y) ;
  of_bytes g

let mul x y =
  assert (Bytes.length x = size) ;
  assert (Bytes.length y = size) ;
  let g = Bytes.create size in
  Fr_stubs.mul
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start x)
    (Ctypes.ocaml_bytes_start y) ;
  of_bytes g

let inverse g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fr_stubs.unsafe_inverse
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  of_bytes buffer

let inverse_opt g =
  assert (Bytes.length g = size) ;
  if is_zero g then None
  else
    let buffer = Bytes.create size in
    Fr_stubs.unsafe_inverse
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g) ;
    Some (of_bytes buffer)

let negate g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fr_stubs.negate
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  of_bytes buffer

let square g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fr_stubs.square
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  of_bytes buffer

let double g =
  assert (Bytes.length g = size) ;
  let buffer = Bytes.create size in
  Fr_stubs.double
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  of_bytes buffer

let eq x y =
  (* IMPORTANT: DO NOT USE THE BYTES representation because we use 384 bits
     instead of 381 bits. We trust the binding offered by the library *)
  Fr_stubs.eq (Ctypes.ocaml_bytes_start x) (Ctypes.ocaml_bytes_start y)

let to_string a = Z.to_string (Z.of_bits (Bytes.to_string a))

let to_z a = Z.of_bits (Bytes.to_string a)

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
  Fr_stubs.pow
    (Ctypes.ocaml_bytes_start res)
    (Ctypes.ocaml_bytes_start (to_bytes x))
    (Ctypes.ocaml_bytes_start padded_n) ;
  res

let of_string s =
  let g = empty () in
  let s = Bytes.of_string (Z.to_bits (Z.erem (Z.of_string s) order)) in
  Bytes.blit s 0 g 0 (min (Bytes.length s) size) ;
  of_bytes s

let of_z z =
  let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
  let x = empty () in
  Bytes.blit z 0 x 0 (min (Bytes.length z) size) ;
  x
