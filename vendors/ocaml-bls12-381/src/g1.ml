module G1_stubs = Rustc_bls12_381_bindings.G1 (Rustc_bls12_381_stubs)

module Uncompressed = struct
  type t = Bytes.t

  let size = 96

  module Scalar = Fr

  let empty () = Bytes.create size

  let check_bytes bs =
    if Bytes.length bs = size then
      G1_stubs.uncompressed_check_bytes (Ctypes.ocaml_bytes_start bs)
    else false

  let of_bytes_opt bs = if check_bytes bs then Some bs else None

  let of_bytes (g : Bytes.t) : t = g

  let of_z_opt ~x ~y =
    let x = Bytes.of_string (Z.to_bits x) in
    let y = Bytes.of_string (Z.to_bits y) in
    let buffer = empty () in
    let res =
      G1_stubs.build_from_components
        (Ctypes.ocaml_bytes_start buffer)
        (Ctypes.ocaml_bytes_start x)
        (Ctypes.ocaml_bytes_start y)
    in
    if res = true then Some (of_bytes buffer) else None

  let to_bytes g = g

  let zero () =
    let g = empty () in
    G1_stubs.zero (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let one () =
    let g = empty () in
    G1_stubs.one (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let random () =
    let g = empty () in
    G1_stubs.random (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let add g1 g2 =
    assert (Bytes.length g1 = size) ;
    assert (Bytes.length g2 = size) ;
    let g = empty () in
    G1_stubs.add
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2) ;
    of_bytes g

  let negate g =
    assert (Bytes.length g = size) ;
    let buffer = empty () in
    G1_stubs.negate
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g) ;
    of_bytes buffer

  let eq g1 g2 =
    assert (Bytes.length g1 = size) ;
    assert (Bytes.length g2 = size) ;
    G1_stubs.eq (Ctypes.ocaml_bytes_start g1) (Ctypes.ocaml_bytes_start g2)

  let is_zero g =
    assert (Bytes.length g = size) ;
    G1_stubs.is_zero (Ctypes.ocaml_bytes_start g)

  let mul (g : t) (a : Scalar.t) : t =
    assert (Bytes.length g = size) ;
    assert (Bytes.length (Scalar.to_bytes a) = Scalar.size) ;
    let buffer = empty () in
    G1_stubs.mul
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start (Scalar.to_bytes a)) ;
    of_bytes buffer
end

module Compressed = struct
  type t = Bytes.t

  let size = 48

  module Scalar = Fr

  let empty () = Bytes.create size

  let to_uncompressed (compressed : t) : Uncompressed.t =
    let g = Uncompressed.empty () in
    G1_stubs.uncompressed_of_compressed
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start compressed) ;
    g

  let of_uncompressed (uncompressed : Uncompressed.t) : t =
    let g = empty () in
    G1_stubs.compressed_of_uncompressed
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start uncompressed) ;
    g

  let check_bytes bs =
    if Bytes.length bs = size then
      G1_stubs.compressed_check_bytes (Ctypes.ocaml_bytes_start bs)
    else false

  let of_bytes_opt bs = if check_bytes bs then Some bs else None

  let of_bytes g = g

  let to_bytes g = g

  let is_zero g =
    assert (Bytes.length g = 48) ;
    G1_stubs.compressed_is_zero (Ctypes.ocaml_bytes_start g)

  let zero () =
    let g = empty () in
    G1_stubs.compressed_zero (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let one () =
    let g = empty () in
    G1_stubs.compressed_one (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let random () =
    let g = empty () in
    G1_stubs.compressed_random (Ctypes.ocaml_bytes_start g) ;
    of_bytes g

  let add g1 g2 =
    assert (Bytes.length g1 = 48) ;
    assert (Bytes.length g2 = 48) ;
    let g = empty () in
    G1_stubs.compressed_add
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2) ;
    of_bytes g

  let negate g =
    assert (Bytes.length g = 48) ;
    let buffer = empty () in
    G1_stubs.compressed_negate
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g) ;
    of_bytes buffer

  let eq g1 g2 =
    assert (Bytes.length g1 = 48) ;
    assert (Bytes.length g2 = 48) ;
    G1_stubs.compressed_eq
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2)

  let mul (g : t) (a : Scalar.t) : t =
    assert (Bytes.length g = 48) ;
    assert (Bytes.length (Fr.to_bytes a) = 32) ;
    let buffer = empty () in
    G1_stubs.compressed_mul
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g)
      (Ctypes.ocaml_bytes_start (Scalar.to_bytes a)) ;
    of_bytes buffer
end
