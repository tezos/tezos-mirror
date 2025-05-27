(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Stubs = struct
  type affine

  type affine_array

  type jacobian

  external allocate_g2 : unit -> jacobian = "allocate_p2_stubs"

  external allocate_g2_affine : unit -> affine = "allocate_p2_affine_stubs"

  external allocate_g2_affine_contiguous_array : int -> affine_array
    = "allocate_p2_affine_array_stubs"

  external p2_affine_array_set_p2_points :
    affine_array -> jacobian array -> int -> int
    = "caml_blst_p2_affine_array_set_p2_points_stubs"

  external from_affine : jacobian -> affine -> int
    = "caml_blst_p2_from_affine_stubs"

  external to_affine : affine -> jacobian -> int
    = "caml_blst_p2_to_affine_stubs"

  external double : jacobian -> jacobian -> int = "caml_blst_p2_double_stubs"

  external dadd : jacobian -> jacobian -> jacobian -> int
    = "caml_blst_p2_add_or_double_stubs"

  external is_zero : jacobian -> bool = "caml_blst_p2_is_inf_stubs"

  external in_g2 : jacobian -> bool = "caml_blst_p2_in_g2_stubs"

  external equal : jacobian -> jacobian -> bool = "caml_blst_p2_equal_stubs"

  external cneg : jacobian -> bool -> int = "caml_blst_p2_cneg_stubs"

  external mult : jacobian -> jacobian -> Bytes.t -> Unsigned.Size_t.t -> int
    = "caml_blst_p2_mult_stubs"

  external deserialize : affine -> Bytes.t -> int
    = "caml_blst_p2_deserialize_stubs"

  external serialize : Bytes.t -> jacobian -> int
    = "caml_blst_p2_serialize_stubs"

  external compress : Bytes.t -> jacobian -> int = "caml_blst_p2_compress_stubs"

  external uncompress : affine -> Bytes.t -> int
    = "caml_blst_p2_uncompress_stubs"

  external hash_to_curve :
    jacobian ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    int
    = "caml_blst_p2_hash_to_curve_stubs_bytecode"
      "caml_blst_p2_hash_to_curve_stubs"

  external memcpy : jacobian -> jacobian -> int = "caml_blst_p2_memcpy_stubs"

  external set_affine_coordinates : affine -> Fq2.t -> Fq2.t -> int
    = "caml_blst_p2_set_coordinates_stubs"

  external affine_array_of_compressed_bytes :
    affine_array -> Bytes.t array -> int -> bool -> int
    = "caml_blst_p2_affine_array_of_compressed_bytes_stubs"

  external affine_add_bulk : jacobian -> affine_array -> int -> int
    = "caml_blst_p2s_add_stubs"

  external pippenger :
    jacobian ->
    jacobian array ->
    Fr.t array ->
    Unsigned.Size_t.t ->
    Unsigned.Size_t.t ->
    int = "caml_blst_g2_pippenger_stubs"

  external continuous_array_get : jacobian -> affine_array -> int -> int
    = "caml_blst_p2_affine_array_get_stubs"

  external pippenger_with_affine_array :
    jacobian ->
    affine_array ->
    Fr.t array ->
    Unsigned.Size_t.t ->
    Unsigned.Size_t.t ->
    int = "caml_blst_g2_pippenger_contiguous_affine_array_stubs"
end

module G2 = struct
  type t = Stubs.jacobian

  type affine = Stubs.affine

  type affine_array = Stubs.affine_array * int

  exception Not_on_curve of Bytes.t

  let size_in_bytes = 192

  let compressed_size_in_bytes = 96

  let affine_of_jacobian j =
    let b = Stubs.allocate_g2_affine () in
    ignore @@ Stubs.to_affine b j ;
    b

  let jacobian_of_affine a =
    let b = Stubs.allocate_g2 () in
    ignore @@ Stubs.from_affine b a ;
    b

  let memcpy dst src = ignore @@ Stubs.memcpy dst src

  let to_affine_array l =
    let length = Array.length l in
    let buffer = Stubs.allocate_g2_affine_contiguous_array length in
    ignore @@ Stubs.p2_affine_array_set_p2_points buffer l length ;
    (buffer, length)

  let of_affine_array (l, n) =
    Array.init n (fun i ->
        let p = Stubs.allocate_g2 () in
        ignore @@ Stubs.continuous_array_get p l i ;
        p)

  let affine_array_of_compressed_bytes_opt ~subgroup_check points_in_bytes =
    let npoints = Array.length points_in_bytes in
    let buffer = Stubs.allocate_g2_affine_contiguous_array npoints in
    let res =
      Stubs.affine_array_of_compressed_bytes
        buffer
        points_in_bytes
        npoints
        subgroup_check
    in
    if res = 0 then Some (buffer, npoints) else None

  let size_of_affine_array (_, n) = n

  let copy src =
    let dst = Stubs.allocate_g2 () in
    memcpy dst src ;
    dst

  module Scalar = Fr

  let check_bytes bs =
    let buffer = Stubs.allocate_g2_affine () in
    Stubs.deserialize buffer bs = 0

  let of_bytes_opt bs =
    let buffer_affine = Stubs.allocate_g2_affine () in
    if Bytes.length bs <> size_in_bytes then None
    else
      let res = Stubs.deserialize buffer_affine bs in
      if res = 0 then (
        let buffer = Stubs.allocate_g2 () in
        ignore @@ Stubs.from_affine buffer buffer_affine ;
        let is_in_prime_subgroup = Stubs.in_g2 buffer in
        if is_in_prime_subgroup then Some buffer else None)
      else None

  let of_bytes_exn bs =
    match of_bytes_opt bs with None -> raise (Not_on_curve bs) | Some p -> p

  let of_compressed_bytes_opt bs =
    let buffer_affine = Stubs.allocate_g2_affine () in
    let res = Stubs.uncompress buffer_affine bs in
    if res = 0 then (
      let buffer = Stubs.allocate_g2 () in
      ignore @@ Stubs.from_affine buffer buffer_affine ;
      let is_in_prime_subgroup = Stubs.in_g2 buffer in
      if is_in_prime_subgroup then Some buffer else None)
    else None

  let of_compressed_bytes_exn bs =
    match of_compressed_bytes_opt bs with
    | None -> raise (Not_on_curve bs)
    | Some p -> p

  let zero =
    let bytes =
      Bytes.of_string
        "\192\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    in
    of_compressed_bytes_exn bytes

  let one =
    let bytes =
      Bytes.of_string
        "\147\224+`Rq\159`}\172\211\160\136'OeYk\208\208\153 \
         \182\026\181\218a\187\220\127PI3L\241\018\019\148]W\229\172}\005]\004+~\002J\162\178\240\143\n\
         \145&\b\005'-\197\016Q\198\228z\212\250@;\
         \002\180Q\011dz\227\209w\011\172\003&\168\005\187\239\212\128V\200\193!\189\184"
    in
    of_compressed_bytes_exn bytes

  let size_in_memory = Obj.reachable_words (Obj.repr one) * 8

  let to_bytes p =
    let buffer = Bytes.make size_in_bytes '\000' in
    ignore @@ Stubs.serialize buffer p ;
    buffer

  let to_compressed_bytes p =
    let buffer = Bytes.make (size_in_bytes / 2) '\000' in
    ignore @@ Stubs.compress buffer p ;
    buffer

  let add x y =
    (* dadd must be used to be complete. add does not work when it is the same
       point *)
    let buffer = Stubs.allocate_g2 () in
    ignore @@ Stubs.dadd buffer x y ;
    buffer

  let add_inplace res x y = ignore @@ Stubs.dadd res x y

  let add_bulk xs =
    let buffer = Stubs.allocate_g2 () in
    List.iter (fun x -> ignore @@ Stubs.dadd buffer buffer x) xs ;
    buffer

  let affine_add_bulk (affine_points, npoints) =
    let buffer = Stubs.allocate_g2 () in
    ignore @@ Stubs.affine_add_bulk buffer affine_points npoints ;
    buffer

  let double x =
    let buffer = Stubs.allocate_g2 () in
    ignore @@ Stubs.double buffer x ;
    buffer

  let mul_bits g bytes =
    let buffer = Stubs.allocate_g2 () in
    ignore
    @@ Stubs.mult
         buffer
         g
         bytes
         (Unsigned.Size_t.of_int (Bytes.length bytes * 8)) ;
    buffer

  let mul g n =
    let bytes = Fr.to_bytes n in
    mul_bits g bytes

  let mul_inplace res g n =
    let bytes = Fr.to_bytes n in
    ignore
    @@ Stubs.mult res g bytes (Unsigned.Size_t.of_int (Bytes.length bytes * 8))

  let b =
    let buffer = Fq2.Stubs.allocate_fp2 () in
    let fq_four = Fq.(one + one + one + one) in
    let bytes = Fq.to_bytes fq_four in
    ignore @@ Fq2.Stubs.of_bytes_components buffer bytes bytes ;
    buffer

  let clear_cofactor p =
    let bytes =
      Z.of_string_base
        16
        "5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5"
    in
    let bytes = Bytes.of_string (Z.to_bits bytes) in
    let res = mul_bits p bytes in
    res

  let rec random ?state () =
    let x = Fq2.random ?state () in
    let xx = Fq2.(x * x) in
    let xxx = Fq2.(x * xx) in
    let xxx_plus_b = Fq2.(xxx + b) in
    let y_opt = Fq2.sqrt_opt xxx_plus_b in
    match y_opt with
    | None -> random ?state ()
    | Some y ->
        let random_bool =
          match state with
          | None -> Random.bool ()
          | Some state -> Random.State.bool state
        in
        let y = if random_bool then y else Fq2.negate y in
        let p_affine = Stubs.allocate_g2_affine () in
        ignore @@ Stubs.set_affine_coordinates p_affine x y ;
        let p = Stubs.allocate_g2 () in
        ignore @@ Stubs.from_affine p p_affine ;
        let p = clear_cofactor p in
        p

  let eq g1 g2 = Stubs.equal g1 g2

  let is_zero x = eq x zero

  let order_minus_one = Scalar.(negate one)

  let negate g =
    let buffer = copy g in
    ignore @@ Stubs.cneg buffer true ;
    buffer

  let of_z_opt ~x ~y =
    let x1, x2 = x in
    let y1, y2 = y in
    let x1_bytes = Bytes.of_string (Z.to_bits x1) in
    let x2_bytes = Bytes.of_string (Z.to_bits x2) in
    let y1_bytes = Bytes.of_string (Z.to_bits y1) in
    let y2_bytes = Bytes.of_string (Z.to_bits y2) in
    let x = Fq2.Stubs.allocate_fp2 () in
    let y = Fq2.Stubs.allocate_fp2 () in
    ignore @@ Fq2.Stubs.of_bytes_components x x1_bytes x2_bytes ;
    ignore @@ Fq2.Stubs.of_bytes_components y y1_bytes y2_bytes ;
    let p_affine = Stubs.allocate_g2_affine () in
    ignore @@ Stubs.set_affine_coordinates p_affine x y ;
    let p = Stubs.allocate_g2 () in
    ignore @@ Stubs.from_affine p p_affine ;
    let is_ok = Stubs.in_g2 p in
    if is_ok then Some p else None

  let hash_to_curve message dst =
    let message_length = Bytes.length message in
    let dst_length = Bytes.length dst in
    let buffer = Stubs.allocate_g2 () in
    ignore
    @@ Stubs.hash_to_curve
         buffer
         message
         (Unsigned.Size_t.of_int message_length)
         dst
         (Unsigned.Size_t.of_int dst_length)
         Bytes.empty
         Unsigned.Size_t.zero ;
    buffer

  let pippenger ?(start = 0) ?len ps ss =
    let l_ss = Array.length ss in
    let l_ps = Array.length ps in
    let l = min l_ss l_ps in
    let len = Option.value ~default:(l - start) len in
    if start < 0 || len < 1 || start + len > l then
      raise @@ Invalid_argument (Format.sprintf "start %i len %i" start len) ;
    if len = 1 then mul ps.(start) ss.(start)
    else
      let buffer = Stubs.allocate_g2 () in
      let res =
        Stubs.pippenger
          buffer
          ps
          ss
          (Unsigned.Size_t.of_int start)
          (Unsigned.Size_t.of_int len)
      in
      assert (res = 0) ;
      buffer

  let pippenger_with_affine_array ?(start = 0) ?len (ps, n) ss =
    let l = min n (Array.length ss) in
    let buffer = Stubs.allocate_g2 () in
    let len = Option.value ~default:(l - start) len in
    if start < 0 || len < 1 || start + len > n then
      raise @@ Invalid_argument (Format.sprintf "start %i len %i" start len) ;
    (if len = 1 then (
       let tmp = Stubs.allocate_g2 () in
       ignore @@ Stubs.continuous_array_get tmp ps start ;
       mul_inplace buffer tmp ss.(start))
     else
       let res =
         Stubs.pippenger_with_affine_array
           buffer
           ps
           ss
           (Unsigned.Size_t.of_int start)
           (Unsigned.Size_t.of_int len)
       in
       assert (res = 0)) ;
    buffer

  let pippenger_with_compressed_bytes_array_opt ~subgroup_check points_in_bytes
      scalars =
    let affine_points =
      affine_array_of_compressed_bytes_opt ~subgroup_check points_in_bytes
    in
    Option.map
      (fun affine_points ->
        let res = pippenger_with_affine_array affine_points scalars in
        to_compressed_bytes res)
      affine_points

  let add_bulk_with_compressed_bytes_array_opt ~subgroup_check points_in_bytes =
    let affine_points =
      affine_array_of_compressed_bytes_opt ~subgroup_check points_in_bytes
    in
    Option.map
      (fun affine_points ->
        let res = affine_add_bulk affine_points in
        to_compressed_bytes res)
      affine_points
end

include G2
