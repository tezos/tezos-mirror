(* Wrapper over Fq12, implementing the group in additive notation *)

module Stubs = struct
  type t = Fq12.Stubs.fp12

  external is_in_group : t -> int = "caml_blst_fp12_in_group_stubs"
end

module GT = struct
  type t = Stubs.t

  let check_bytes b =
    let x = Fq12.of_bytes_opt b in
    match x with
    | None -> false
    | Some x ->
        (not (Fq12.is_zero x))
        (* https://github.com/supranational/blst/issues/108 *)
        && Stubs.is_in_group x = 1

  exception Not_in_group of Bytes.t

  let order = Fr.order

  let of_bytes_opt b =
    let x = Fq12.of_bytes_opt b in
    match x with
    | None -> None
    | Some x ->
        if
          (not (Fq12.is_zero x))
          (* https://github.com/supranational/blst/issues/108 *)
          && Stubs.is_in_group x = 1
        then Some x
        else None

  let of_bytes_exn b =
    match of_bytes_opt b with None -> raise (Not_in_group b) | Some x -> x

  let zero = Fq12.one

  let one =
    of_bytes_exn
      (Hex.to_bytes
         (`Hex
            "b68917caaa0543a808c53908f694d1b6e7b38de90ce9d83d505ca1ef1b442d2727d7d06831d8b2a7920afc71d8eb50120f17a0ea982a88591d9f43503e94a8f1abaf2e4589f65aafb7923c484540a868883432a5c60e75860b11e5465b1c9a08873ec29e844c1c888cb396933057ffdd541b03a5220eda16b2b3a6728ea678034ce39c6839f20397202d7c5c44bb68134f93193cec215031b17399577a1de5ff1f5b0666bdd8907c61a7651e4e79e0372951505a07fa73c25788db6eb8023519a5aa97b51f1cad1d43d8aabbff4dc319c79a58cafc035218747c2f75daf8f2fb7c00c44da85b129113173d4722f5b201b6b4454062e9ea8ba78c5ca3cadaf7238b47bace5ce561804ae16b8f4b63da4645b8457a93793cbd64a7254f150781019de87ee42682940f3e70a88683d512bb2c3fb7b2434da5dedbb2d0b3fb8487c84da0d5c315bdd69c46fb05d23763f2191aabd5d5c2e12a10b8f002ff681bfd1b2ee0bf619d80d2a795eb22f2aa7b85d5ffb671a70c94809f0dafc5b73ea2fb0657bae23373b4931bc9fa321e8848ef78894e987bff150d7d671aee30b3931ac8c50e0b3b0868effc38bf48cd24b4b811a2995ac2a09122bed9fd9fa0c510a87b10290836ad06c8203397b56a78e9a0c61c77e56ccb4f1bc3d3fcaea7550f3503efe30f2d24f00891cb45620605fcfaa4292687b3a7db7c1c0554a93579e889a121fd8f72649b2402996a084d2381c5043166673b3849e4fd1e7ee4af24aa8ed443f56dfd6b68ffde4435a92cd7a4ac3bc77e1ad0cb728606cf08bf6386e5410f"))

  let size_in_memory = Obj.reachable_words (Obj.repr zero) * 8

  let size_in_bytes = Fq12.size_in_bytes

  let eq = Fq12.eq

  let is_zero x = eq x zero

  let is_one x = eq x one

  let add = Fq12.mul

  let negate = Fq12.inverse_exn

  let mul x n = Fq12.pow x (Fr.to_z n)

  let to_bytes = Fq12.to_bytes

  let random ?state () =
    let r = Fr.random ?state () in
    mul one r
end

include GT
