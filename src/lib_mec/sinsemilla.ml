open Pallas

exception Bottom

(* incomplete addition for Pallas *)
let ( ++ ) p1 p2 =
  if Affine.is_zero p1 then raise Bottom
  else if Affine.is_zero p2 then raise Bottom
  else
    let x1 = Affine.get_x_coordinate p1 in
    let x2 = Affine.get_x_coordinate p2 in
    if Affine.Base.eq x1 x2 then raise Bottom else Affine.add p1 p2

module MakeSinsemilla (Params : sig
  val iv : Affine.t

  val generators : Affine.t array

  val chunk_size : int
end) =
struct
  let hash_exn iterator =
    let rec aux acc =
      if Iterator.Bit.is_processed iterator then acc
      else
        let m_j_bits = Iterator.Bit.get_chunk iterator Params.chunk_size in
        let m_j, _ =
          List.fold_left
            (fun (acc, i) b -> (acc + ((1 lsl i) * b), i + 1))
            (0, 0)
            m_j_bits
        in
        let gen_j = Params.generators.(m_j) in
        aux (acc ++ (acc ++ gen_j))
    in
    let v = aux Params.iv in
    Affine.get_x_coordinate v

  let hash_opt iterator =
    try Some (hash_exn iterator) with Bottom -> None | e -> raise e
end

module Zcash = struct
  let hash_exn iv iterator =
    let module Sinsemilla = MakeSinsemilla (struct
      let iv = iv

      let generators = Sinsemilla_zcash_generators.generators_zcash

      let chunk_size = 10
    end) in
    Sinsemilla.hash_exn iterator

  let hash_opt iv iterator =
    let module Sinsemilla = MakeSinsemilla (struct
      let iv = iv

      let generators = Sinsemilla_zcash_generators.generators_zcash

      let chunk_size = 10
    end) in
    Sinsemilla.hash_opt iterator
end
