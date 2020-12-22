val miller_loop_simple : G1.Uncompressed.t -> G2.Uncompressed.t -> Fq12.t

val pairing : G1.Uncompressed.t -> G2.Uncompressed.t -> Fq12.t

val final_exponentiation_exn : Fq12.t -> Fq12.t

val final_exponentiation_opt : Fq12.t -> Fq12.t option

val miller_loop : (G1.Uncompressed.t * G2.Uncompressed.t) list -> Fq12.t
