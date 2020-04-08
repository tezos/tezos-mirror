open Ctypes

module MakeFieldBindings (S : sig
  val field_name : string
end)
(F : Cstubs.FOREIGN) =
struct
  open F

  let check_bytes =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_check_bytes" S.field_name)
      (ocaml_bytes @-> returning bool)

  let is_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_zero" S.field_name)
      (ocaml_bytes @-> returning bool)

  let is_one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_one" S.field_name)
      (ocaml_bytes @-> returning bool)

  let random =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_random" S.field_name)
      (ocaml_bytes @-> returning void)

  let zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_zero" S.field_name)
      (ocaml_bytes @-> returning void)

  let one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_one" S.field_name)
      (ocaml_bytes @-> returning void)

  let add =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_add" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let mul =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_mul" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let unsafe_inverse =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_unsafe_inverse" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let eq =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_eq" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let negate =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_negate" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let square =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_square" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let double =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_double" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let pow =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_pow" S.field_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)
end

module Fq12 = MakeFieldBindings (struct
  let field_name = "fq12"
end)

module Fr = MakeFieldBindings (struct
  let field_name = "fr"
end)

module MakeGroupBindings (S : sig
  val group_name : string
end)
(F : Cstubs.FOREIGN) =
struct
  open F

  let uncompressed_check_bytes =
    foreign
      (Printf.sprintf
         "rustc_bls12_381_%s_uncompressed_check_bytes"
         S.group_name)
      (ocaml_bytes @-> returning bool)

  let compressed_check_bytes =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_check_bytes" S.group_name)
      (ocaml_bytes @-> returning bool)

  let one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_one" S.group_name)
      (ocaml_bytes @-> returning void)

  let zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_zero" S.group_name)
      (ocaml_bytes @-> returning void)

  let random =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_random" S.group_name)
      (ocaml_bytes @-> returning void)

  let add =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_add" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let negate =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_negate" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let eq =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_eq" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let is_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_is_zero" S.group_name)
      (ocaml_bytes @-> returning bool)

  let mul =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_mul" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let compressed_one =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_one" S.group_name)
      (ocaml_bytes @-> returning void)

  let compressed_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_zero" S.group_name)
      (ocaml_bytes @-> returning void)

  let compressed_random =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_random" S.group_name)
      (ocaml_bytes @-> returning void)

  let compressed_add =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_add" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let compressed_negate =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_negate" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let compressed_eq =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_eq" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning bool)

  let compressed_is_zero =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_is_zero" S.group_name)
      (ocaml_bytes @-> returning bool)

  let compressed_mul =
    foreign
      (Printf.sprintf "rustc_bls12_381_%s_compressed_mul" S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let compressed_of_uncompressed =
    foreign
      (Printf.sprintf
         "rustc_bls12_381_%s_compressed_of_uncompressed"
         S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)

  let uncompressed_of_compressed =
    foreign
      (Printf.sprintf
         "rustc_bls12_381_%s_uncompressed_of_compressed"
         S.group_name)
      (ocaml_bytes @-> ocaml_bytes @-> returning void)
end

module G1 (F : Cstubs.FOREIGN) = struct
  open F

  include MakeGroupBindings
            (struct
              let group_name = "g1"
            end)
            (F)

  let build_from_components =
    foreign
      "rustc_bls12_381_g1_build_from_components"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning bool)
end

module G2 (F : Cstubs.FOREIGN) = struct
  open F

  include MakeGroupBindings
            (struct
              let group_name = "g2"
            end)
            (F)

  let build_from_components =
    foreign
      "rustc_bls12_381_g2_build_from_components"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> returning bool )
end

module Pairing (F : Cstubs.FOREIGN) = struct
  open F

  let miller_loop_simple =
    foreign
      "rustc_bls12_381_pairing_miller_loop_simple"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let miller_loop_2 =
    foreign
      "rustc_bls12_381_pairing_miller_loop_2"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> returning void )

  let miller_loop_3 =
    foreign
      "rustc_bls12_381_pairing_miller_loop_3"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void )

  let miller_loop_4 =
    foreign
      "rustc_bls12_381_pairing_miller_loop_4"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> returning void )

  let miller_loop_5 =
    foreign
      "rustc_bls12_381_pairing_miller_loop_5"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void )

  let miller_loop_6 =
    foreign
      "rustc_bls12_381_pairing_miller_loop_6"
      ( ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes
      @-> ocaml_bytes @-> returning void )

  let pairing =
    foreign
      "rustc_bls12_381_pairing"
      (ocaml_bytes @-> ocaml_bytes @-> ocaml_bytes @-> returning void)

  let final_exponentiation =
    foreign
      "rustc_bls12_381_unsafe_pairing_final_exponentiation"
      (ocaml_bytes @-> ocaml_bytes @-> returning void)
end
