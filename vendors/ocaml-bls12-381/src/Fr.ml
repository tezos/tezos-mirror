module Fr_stubs = Rustc_bls12_381_bindings.Fr (Rustc_bls12_381_stubs)

exception Not_in_field of Bytes.t

(** High level (OCaml) definitions/logic *)
let size_in_bytes = 32

let order =
  Z.of_string
    "52435875175126190479447740508185965837690552500527637822603658699938581184513"

type t = Bytes.t

let empty () = Bytes.make size_in_bytes '\000'

let pad_if_require bs =
  if Bytes.length bs < size_in_bytes then (
    let padded_bytes = Bytes.make size_in_bytes '\000' in
    Bytes.blit bs 0 padded_bytes 0 (Bytes.length bs) ;
    padded_bytes )
  else bs

let check_bytes bs =
  if Bytes.length bs = size_in_bytes then
    Fr_stubs.check_bytes (Ctypes.ocaml_bytes_start bs)
  else false

let of_bytes_opt bs =
  let bs = pad_if_require bs in
  if check_bytes bs then Some bs else None

let of_bytes_exn (g : Bytes.t) : t =
  let g = pad_if_require g in
  if check_bytes g then g else raise (Not_in_field g)

let to_bytes g = g

let is_zero g =
  assert (Bytes.length g = size_in_bytes) ;
  Fr_stubs.is_zero (Ctypes.ocaml_bytes_start g)

let is_one g =
  assert (Bytes.length g = size_in_bytes) ;
  Fr_stubs.is_one (Ctypes.ocaml_bytes_start g)

let zero =
  let g = empty () in
  Fr_stubs.zero (Ctypes.ocaml_bytes_start g) ;
  g

let one =
  let g = empty () in
  Fr_stubs.one (Ctypes.ocaml_bytes_start g) ;
  g

let random ?state () =
  ignore state ;
  let g = empty () in
  Fr_stubs.random (Ctypes.ocaml_bytes_start g) ;
  g

let rec non_null_random ?state () =
  ignore state ;
  let r = random () in
  if is_zero r then non_null_random () else r

let add x y =
  assert (Bytes.length x = size_in_bytes) ;
  assert (Bytes.length y = size_in_bytes) ;
  let g = empty () in
  Fr_stubs.add
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start x)
    (Ctypes.ocaml_bytes_start y) ;
  g

let ( + ) = add

let mul x y =
  assert (Bytes.length x = size_in_bytes) ;
  assert (Bytes.length y = size_in_bytes) ;
  let g = empty () in
  Fr_stubs.mul
    (Ctypes.ocaml_bytes_start g)
    (Ctypes.ocaml_bytes_start x)
    (Ctypes.ocaml_bytes_start y) ;
  g

let ( * ) = mul

let inverse_exn g =
  assert (Bytes.length g = size_in_bytes) ;
  let buffer = empty () in
  Fr_stubs.unsafe_inverse
    (Ctypes.ocaml_bytes_start buffer)
    (Ctypes.ocaml_bytes_start g) ;
  buffer

let inverse_opt g =
  assert (Bytes.length g = size_in_bytes) ;
  if is_zero g then None
  else
    let buffer = empty () in
    Fr_stubs.unsafe_inverse
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g) ;
    Some buffer

let negate g =
  assert (Bytes.length g = size_in_bytes) ;
  let buffer = empty () in
  Fr_stubs.negate (Ctypes.ocaml_bytes_start buffer) (Ctypes.ocaml_bytes_start g) ;
  buffer

let ( - ) = negate

let square g =
  assert (Bytes.length g = size_in_bytes) ;
  let buffer = empty () in
  Fr_stubs.square (Ctypes.ocaml_bytes_start buffer) (Ctypes.ocaml_bytes_start g) ;
  buffer

let double g =
  assert (Bytes.length g = size_in_bytes) ;
  let buffer = empty () in
  Fr_stubs.double (Ctypes.ocaml_bytes_start buffer) (Ctypes.ocaml_bytes_start g) ;
  buffer

let eq x y =
  (* IMPORTANT: DO NOT USE THE BYTES representation because we use 384 bits
     instead of 381 bits. We trust the binding offered by the library *)
  assert (Bytes.length x = size_in_bytes) ;
  assert (Bytes.length y = size_in_bytes) ;
  Fr_stubs.eq (Ctypes.ocaml_bytes_start x) (Ctypes.ocaml_bytes_start y)

let ( = ) = eq

let to_string a = Z.to_string (Z.of_bits (Bytes.to_string a))

let to_z a = Z.of_bits (Bytes.to_string a)

let pow x n =
  let res = empty () in
  let n = Z.erem n (Z.pred order) in
  (* sign is removed by to_bits, but that's fine because we used mod before *)
  let n = Bytes.of_string (Z.to_bits n) in
  let bytes_size_n = Bytes.length n in
  let padded_n =
    Bytes.init size_in_bytes (fun i ->
        if i < bytes_size_n then Bytes.get n i else char_of_int 0)
  in
  Fr_stubs.pow
    (Ctypes.ocaml_bytes_start res)
    (Ctypes.ocaml_bytes_start (to_bytes x))
    (Ctypes.ocaml_bytes_start padded_n) ;
  res

let ( ** ) = pow

let of_string s =
  let g = empty () in
  let s = Bytes.of_string (Z.to_bits (Z.erem (Z.of_string s) order)) in
  Bytes.blit s 0 g 0 (min (Bytes.length s) size_in_bytes) ;
  of_bytes_exn g

let of_z z =
  let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
  let x = empty () in
  Bytes.blit z 0 x 0 (min (Bytes.length z) size_in_bytes) ;
  of_bytes_exn x

let div_exn a b =
  if b = zero then raise Division_by_zero else mul a (inverse_exn b)

let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

let ( / ) = div_exn
