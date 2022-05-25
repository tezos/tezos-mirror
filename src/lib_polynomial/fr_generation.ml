module Make (Scalar : Ff_sig.PRIME) = struct
  let fr_of_int_safe n = Z.of_int n |> Scalar.of_z

  (* samples a 2^i-th root of unity, assuming that it exists *)
  let root_of_unity i =
    let two = Z.(one + one) in
    let two_i = Z.(one lsl i) in
    let exp, r = Z.div_rem Z.(Scalar.order - one) two_i in

    if i = 0 then Scalar.one
    else if not Z.(equal r zero) then
      let msg = Format.sprintf "There do not exist 2**%d-th roots of unity" i in
      raise (Invalid_argument msg)
    else
      (* x is a 2^i-th primitive root of unity if x^(2^i) = 1 for the first time.
         This is equivalent to x^(2^i / 2) being -1, since:
         - If x^(2^i) = 1, then x^(2^i / 2) must be in {+1,-1}, but if the root
           is primitive, it must be -1.
         - On the other hand, if x^(2^i / 2) = -1, then x must be a 2^i-th
           primitive root of unity. *)
      let is_primitive x = Scalar.(eq (negate one) @@ pow x Z.(two_i / two)) in

      let rec find_primitive_root seed =
        let root = Scalar.(pow seed exp) in
        if is_primitive root then root
        else find_primitive_root Scalar.(seed + one)
      in
      find_primitive_root (fr_of_int_safe 5330)

  let build_array init next len =
    let xi = ref init in
    Array.init len (fun _ ->
        let i = !xi in
        xi := next !xi ;
        i)

  (* computes [| 1; x; x²; x³; ...; xᵈ⁻¹ |] *)
  let powers d x = build_array Scalar.one Scalar.(mul x) d

  let build_domain i =
    let g = root_of_unity i in
    powers (1 lsl i) g
end
