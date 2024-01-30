(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Formats used for Srs:
    - the disk format uses compressed affine coordinates to save space
    - the memory format uses uncompressed affine coordiantes, because it's
      required by the pippenger API
    - the interface and memory representation of an Srs is as carray of affine
      elements (type G.affine) because they are however the interface that is
      exposed uses jacobian format (the type G.t). *)

(* The Elt signature is extended with pippenger. *)
module type GElt_sig = sig
  module G : Bls12_381.CURVE

  include Carray.Elt_sig with type t = G.affine

  (** [uncompress res bs n] reads [n] bytes from [bs] and uncompresses
      them into an affine point in [res]. Returns 0 if successful. *)
  val uncompress : G.affine -> bytes -> int
  (* This function is already bound in Bls12-381 but not exposed. *)

  (** [pippenger res srs poly offset len] writes in [res] the
      multiexponentiation of [srs] with the polynomial [poly] starting at
      [offset] and for [len] elements. Returns 0 if successful. *)
  val pippenger : G.t -> Bigstringaf.t -> Polynomial.t -> int -> int -> int
end

module type S = sig
  type polynomial

  type t [@@deriving repr]

  type elt

  val empty : t

  (** [get srs i] returns the [i]-th element of [srs] *)
  val get : t -> int -> elt

  (** Returns the pippenger ctxt size, i.e. the number of elements the context
      is supposed to be called with *)
  val size : t -> int

  val of_bigstring :
    ?len:int ->
    Bigstringaf.t ->
    (t, [> `End_of_file of string | `Invalid_point of int]) result

  (** [of_bigstring ~len bs] reads [len] points of G in affine
      compressed format from [bs] and returns a Srs. If [len] is omitted the
      whole bs is read.
      The bigstring can be loaded from a file with:
      {[
      let bigstring_of_file filename =
        let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o440 in
        Bigarray.array1_of_genarray
        @@ Unix.map_file fd Bigarray.char Bigarray.c_layout false
             [| (* [-1] means read the whole file *) -1 |]
      in
      Srs_g1.of_bigstring
            (bigstring_of_file ("srs_zcash_g1_21"))
            (1 lsl 5)
      ]}
      or with Lwt:
      {[
      let bigstring_of_file filename =
        let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o440 in
        Lwt_bytes.map_file ~fd ~shared:false ()
      in
      ]} *)

  val generate_insecure : int -> Bls12_381.Fr.t -> t

  (** [pippenger ctxt poly] computes the multiscalar exponentiation using the
      SRS saved in the context and the coefficients of the given polynomial *)
  val pippenger : ?offset:int -> ?len:int -> t -> polynomial -> elt

  (* TODO just for pack *)
  val to_array : ?len:int -> t -> elt array

  val of_array : elt array -> t
end

module Make (Elt : GElt_sig) = struct
  module G = Elt.G
  module Carray = Carray.Make (Elt)

  type polynomial = Polynomial.Polynomial_unsafe.t

  type t = Carray.t [@@deriving repr]

  type elt = G.t

  let size = Carray.length

  let get p i = Carray.get p i |> G.jacobian_of_affine

  let to_array ?len data =
    Carray.to_array ?len data |> Array.map G.jacobian_of_affine

  let of_array caml_array =
    Carray.of_array (Array.map G.affine_of_jacobian caml_array)

  let empty = Carray.empty

  let generate_insecure d x =
    let xi = ref G.one in
    Array.init d (fun _ ->
        let res = !xi in
        xi := G.mul !xi x ;
        res)
    |> of_array

  let of_bigstring ?len bs =
    let size_compressed = Elt.size / 2 in
    let n =
      Option.value ~default:(Bigstringaf.length bs / size_compressed) len
    in
    let data = Carray.allocate n in
    let buf = Bytes.create size_compressed in
    let point = Elt.allocate () in
    let ( let* ) = Stdlib.Result.bind in
    let rec loop i =
      if i = n then Ok ()
      else
        let* () =
          try
            Ok
              (Bigstringaf.blit_to_bytes
                 bs
                 ~src_off:(i * size_compressed)
                 buf
                 ~dst_off:0
                 ~len:size_compressed)
          with _ ->
            Error
              (`End_of_file
                (Format.sprintf "found only %d elements of the required %d" i n))
        in
        let code = Elt.uncompress point buf in
        let* () = if code <> 0 then Error (`Invalid_point i) else Ok () in
        Carray.set data point i ;
        loop (i + 1)
    in
    let* () = loop 0 in
    Ok data

  let pippenger ?(offset = 0) ?len srs poly =
    if Polynomial.is_zero poly then G.zero
    else
      let poly_length = Polynomial.degree poly + 1 in
      let len = Option.value ~default:(poly_length - offset) len in
      if
        len <= 0 || offset < 0
        || poly_length - offset < len
        || size srs - offset < len
      then
        raise
        @@ Invalid_argument
             (Format.sprintf
                "pippenger: invalid len %d or offset %d for size %d"
                len
                offset
                poly_length) ;
      let res = G.(copy zero) in
      let return_code =
        Elt.pippenger res (Carray.to_bigstring srs) poly offset len
      in
      assert (return_code = 0) ;
      res
end

module Elt_g1 = struct
  module G = Bls12_381.G1

  type t = G.affine

  let size = G.size_in_bytes

  let allocate () = G.(affine_of_jacobian zero)

  let zero = G.(affine_of_jacobian zero)

  let eq a b = G.eq (G.jacobian_of_affine a) (G.jacobian_of_affine b)

  external uncompress : t -> bytes -> int = "caml_blst_p1_uncompress_stubs"
    [@@noalloc]

  external pippenger : G.t -> Bigstringaf.t -> Polynomial.t -> int -> int -> int
    = "caml_bls12_381_polynomial_srs_g1_pippenger_stubs"
    [@@noalloc]
end

module Elt_g2 = struct
  module G = Bls12_381.G2

  type t = G.affine

  let size = G.size_in_bytes

  let allocate () = G.(affine_of_jacobian zero)

  let zero = G.(affine_of_jacobian zero)

  let eq a b = G.eq (G.jacobian_of_affine a) (G.jacobian_of_affine b)

  external uncompress : G.affine -> bytes -> int
    = "caml_blst_p2_uncompress_stubs"
    [@@noalloc]

  external pippenger : G.t -> Bigstringaf.t -> Polynomial.t -> int -> int -> int
    = "caml_bls12_381_polynomial_srs_g2_pippenger_stubs"
    [@@noalloc]
end

module Srs_g1 :
  S with type elt = Bls12_381.G1.t and type polynomial = Polynomial.t =
  Make (Elt_g1)

module Srs_g2 :
  S with type elt = Bls12_381.G2.t and type polynomial = Polynomial.t =
  Make (Elt_g2)

module Checks = struct
  open Bls12_381

  let equality g1s g2s =
    let g1 = Srs_g1.get g1s 0 in
    let g2 = Srs_g2.get g2s 0 in
    let size = min (Srs_g1.size g1s) (Srs_g2.size g2s) in
    for i = 0 to size - 1 do
      let g1i = Srs_g1.get g1s i in
      let g2i = Srs_g2.get g2s i in
      let gt1 = Pairing.pairing g1i g2 in
      let gt2 = Pairing.pairing g1 g2i in
      assert (GT.eq gt1 gt2)
    done

  let incrementation_g1 g1s g2s =
    let g2 = Srs_g2.get g2s 0 in
    let g2x = Srs_g2.get g2s 1 in
    for i = 0 to Srs_g1.size g1s - 2 do
      let g1i = Srs_g1.get g1s i in
      let g1iplus = Srs_g1.get g1s (i + 1) in
      let gt1 = Pairing.pairing g1i g2x in
      let gt2 = Pairing.pairing g1iplus g2 in
      assert (GT.eq gt1 gt2)
    done

  let incrementation_g2 g1s g2s =
    let g1 = Srs_g1.get g1s 0 in
    let g1x = Srs_g1.get g1s 1 in
    for i = 0 to Srs_g2.size g2s - 2 do
      let g2i = Srs_g2.get g2s i in
      let g2iplus = Srs_g2.get g2s (i + 1) in
      let gt1 = Pairing.pairing g1x g2i in
      let gt2 = Pairing.pairing g1 g2iplus in
      assert (GT.eq gt1 gt2)
    done

  let pairings (g1s, g2s) =
    equality g1s g2s ;
    incrementation_g1 g1s g2s ;
    incrementation_g2 g1s g2s
end

module Srs = struct
  module Srs_g1 = Srs_g1
  module Srs_g2 = Srs_g2

  type t = Srs_g1.t * Srs_g2.t

  let generate_insecure log_g1 log_g2 =
    let x = Bls12_381.Fr.random () in
    ( Srs_g1.generate_insecure (1 lsl log_g1) x,
      Srs_g2.generate_insecure (1 lsl log_g2) x )

  let check = Checks.pairings
end

(* testing only *)
module type S_unsafe = sig
  include S

  val of_array : elt array -> t
end

module Srs_g1_unsafe = Make (Elt_g1)
