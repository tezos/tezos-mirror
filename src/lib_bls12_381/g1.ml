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
  type affine_array

  type affine

  type jacobian

  external allocate_g1 : unit -> jacobian = "allocate_p1_stubs"

  external allocate_g1_affine_contiguous_array : int -> affine_array
    = "allocate_p1_affine_array_stubs"

  external p1_affine_array_set_p1_points :
    affine_array -> jacobian array -> int -> int
    = "caml_blst_p1_affine_array_set_p1_points_stubs"

  external allocate_g1_affine : unit -> affine = "allocate_p1_affine_stubs"

  external from_affine : jacobian -> affine -> int
    = "caml_blst_p1_from_affine_stubs"

  external to_affine : affine -> jacobian -> int
    = "caml_blst_p1_to_affine_stubs"

  external double : jacobian -> jacobian -> int = "caml_blst_p1_double_stubs"

  external dadd : jacobian -> jacobian -> jacobian -> int
    = "caml_blst_p1_add_or_double_stubs"

  external is_zero : jacobian -> bool = "caml_blst_p1_is_inf_stubs"

  external in_g1 : jacobian -> bool = "caml_blst_p1_in_g1_stubs"

  external equal : jacobian -> jacobian -> bool = "caml_blst_p1_equal_stubs"

  external cneg : jacobian -> bool -> int = "caml_blst_p1_cneg_stubs"

  external mult : jacobian -> jacobian -> Bytes.t -> Unsigned.Size_t.t -> int
    = "caml_blst_p1_mult_stubs"

  external deserialize : affine -> Bytes.t -> int
    = "caml_blst_p1_deserialize_stubs"

  external serialize : Bytes.t -> jacobian -> int
    = "caml_blst_p1_serialize_stubs"

  external compress : Bytes.t -> jacobian -> int = "caml_blst_p1_compress_stubs"

  external uncompress : affine -> Bytes.t -> int
    = "caml_blst_p1_uncompress_stubs"

  external hash_to_curve :
    jacobian ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    Bytes.t ->
    Unsigned.Size_t.t ->
    int
    = "caml_blst_p1_hash_to_curve_stubs_bytecode"
      "caml_blst_p1_hash_to_curve_stubs"

  external memcpy : jacobian -> jacobian -> int = "caml_blst_p1_memcpy_stubs"

  external set_affine_coordinates : affine -> Fq.t -> Fq.t -> int
    = "caml_blst_p1_set_coordinates_stubs"

  external affine_array_of_compressed_bytes :
    affine_array -> Bytes.t array -> int -> bool -> int
    = "caml_blst_p1_affine_array_of_compressed_bytes_stubs"

  external affine_add_bulk : jacobian -> affine_array -> int -> int
    = "caml_blst_p1s_add_stubs"

  external pippenger :
    jacobian ->
    jacobian array ->
    Fr.t array ->
    Unsigned.Size_t.t ->
    Unsigned.Size_t.t ->
    int = "caml_blst_g1_pippenger_stubs"

  external continuous_array_get : jacobian -> affine_array -> int -> int
    = "caml_blst_p1_affine_array_get_stubs"

  external pippenger_with_affine_array :
    jacobian ->
    affine_array ->
    Fr.t array ->
    Unsigned.Size_t.t ->
    Unsigned.Size_t.t ->
    int = "caml_blst_g1_pippenger_contiguous_affine_array_stubs"
end

module G1 = struct
  exception Not_on_curve of Bytes.t

  type t = Stubs.jacobian

  type affine = Stubs.affine

  type affine_array = Stubs.affine_array * int

  let size_in_bytes = 96

  let compressed_size_in_bytes = 48

  let memcpy dst src = ignore @@ Stubs.memcpy dst src

  let affine_of_jacobian j =
    let b = Stubs.allocate_g1_affine () in
    ignore @@ Stubs.to_affine b j ;
    b

  let jacobian_of_affine a =
    let b = Stubs.allocate_g1 () in
    ignore @@ Stubs.from_affine b a ;
    b

  let to_affine_array l =
    let length = Array.length l in
    let buffer = Stubs.allocate_g1_affine_contiguous_array length in
    ignore @@ Stubs.p1_affine_array_set_p1_points buffer l length ;
    (buffer, length)

  let of_affine_array (l, n) =
    Array.init n (fun i ->
        let p = Stubs.allocate_g1 () in
        ignore @@ Stubs.continuous_array_get p l i ;
        p)

  let affine_array_of_compressed_bytes_opt ~subgroup_check points_in_bytes =
    let npoints = Array.length points_in_bytes in
    let buffer = Stubs.allocate_g1_affine_contiguous_array npoints in
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
    let dst = Stubs.allocate_g1 () in
    memcpy dst src ;
    dst

  module Scalar = Fr

  let cofactor_fr = Scalar.of_string "76329603384216526031706109802092473003"

  let check_bytes bs =
    let buffer = Stubs.allocate_g1_affine () in
    Stubs.deserialize buffer bs = 0

  let of_bytes_opt bs =
    let buffer_affine = Stubs.allocate_g1_affine () in
    if Bytes.length bs <> size_in_bytes then None
    else
      let res = Stubs.deserialize buffer_affine bs in
      if res = 0 then (
        let buffer = Stubs.allocate_g1 () in
        ignore @@ Stubs.from_affine buffer buffer_affine ;
        let is_in_prime_subgroup = Stubs.in_g1 buffer in
        if is_in_prime_subgroup then Some buffer else None)
      else None

  let of_bytes_exn bs =
    match of_bytes_opt bs with None -> raise (Not_on_curve bs) | Some p -> p

  let zero =
    let bytes =
      Bytes.of_string
        "@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    in
    of_bytes_exn bytes

  let one =
    let bytes =
      Bytes.of_string
        "\023\241\211\1671\151\215\148&\149c\140O\169\172\015\195h\140O\151t\185\005\161N:?\023\027\172XlU\232?\249z\026\239\251:\240\n\
         \219\"\198\187\b\179\244\129\227\170\160\241\160\1580\237t\029\138\228\252\245\224\149\213\208\n\
         \246\000\219\024\203,\004\179\237\208<\199D\162\136\138\228\012\170#)F\197\231\225"
    in
    of_bytes_exn bytes

  let size_in_memory = Obj.reachable_words (Obj.repr one) * 8

  let of_compressed_bytes_opt bs =
    let buffer_affine = Stubs.allocate_g1_affine () in
    let res = Stubs.uncompress buffer_affine bs in
    if res = 0 then (
      let buffer = Stubs.allocate_g1 () in
      ignore @@ Stubs.from_affine buffer buffer_affine ;
      let is_in_prime_subgroup = Stubs.in_g1 buffer in
      if is_in_prime_subgroup then Some buffer else None)
    else None

  let of_compressed_bytes_exn bs =
    match of_compressed_bytes_opt bs with
    | None -> raise (Not_on_curve bs)
    | Some p -> p

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
    let buffer = Stubs.allocate_g1 () in
    ignore @@ Stubs.dadd buffer x y ;
    buffer

  let add_inplace res x y = ignore @@ Stubs.dadd res x y

  let add_bulk xs =
    let buffer = Stubs.allocate_g1 () in
    List.iter (fun x -> ignore @@ Stubs.dadd buffer buffer x) xs ;
    buffer

  let affine_add_bulk (affine_points, npoints) =
    let buffer = Stubs.allocate_g1 () in
    ignore @@ Stubs.affine_add_bulk buffer affine_points npoints ;
    buffer

  let double x =
    let buffer = Stubs.allocate_g1 () in
    ignore @@ Stubs.double buffer x ;
    buffer

  let mul g n =
    let buffer = Stubs.allocate_g1 () in
    let bytes = Fr.to_bytes n in
    ignore @@ Stubs.mult buffer g bytes (Unsigned.Size_t.of_int (32 * 8)) ;
    buffer

  let mul_inplace res g n =
    ignore @@ Stubs.mult res g (Fr.to_bytes n) (Unsigned.Size_t.of_int (32 * 8))

  let b = Fq.(one + one + one + one)

  let rec random ?state () =
    let x = Fq.random ?state () in
    let xx = Fq.(x * x) in
    let xxx = Fq.(x * xx) in
    let xxx_plus_b = Fq.(xxx + b) in
    let y_opt = Fq.sqrt_opt xxx_plus_b in
    match y_opt with
    | None -> random ?state ()
    | Some y ->
        let random_bool =
          match state with
          | None -> Random.bool ()
          | Some state -> Random.State.bool state
        in
        let y = if random_bool then y else Fq.negate y in
        let p_affine = Stubs.allocate_g1_affine () in
        ignore @@ Stubs.set_affine_coordinates p_affine x y ;
        let p = Stubs.allocate_g1 () in
        ignore @@ Stubs.from_affine p p_affine ;
        mul p cofactor_fr

  let eq g1 g2 = Stubs.equal g1 g2

  let is_zero x = eq x zero

  let order_minus_one = Scalar.(negate one)

  let negate g =
    let buffer = copy g in
    ignore @@ Stubs.cneg buffer true ;
    buffer

  let of_z_opt ~x ~y =
    let x = Fq.of_z x in
    let y = Fq.of_z y in
    let buffer_affine = Stubs.allocate_g1_affine () in
    ignore @@ Stubs.set_affine_coordinates buffer_affine x y ;
    let buffer = Stubs.allocate_g1 () in
    ignore @@ Stubs.from_affine buffer buffer_affine ;
    if Stubs.in_g1 buffer then Some buffer else None

  let hash_to_curve message dst =
    let message_length = Bytes.length message in
    let dst_length = Bytes.length dst in
    let buffer = Stubs.allocate_g1 () in
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
      let buffer = Stubs.allocate_g1 () in
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
    let buffer = Stubs.allocate_g1 () in
    let len = Option.value ~default:(l - start) len in
    if start < 0 || len < 1 || start + len > n then
      raise @@ Invalid_argument (Format.sprintf "start %i len %i" start len) ;
    (if len = 1 then (
       let tmp = Stubs.allocate_g1 () in
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

include G1
