module Utils = struct
  let bitreverse n' l =
    let r = ref 0 in
    let n = ref n' in
    for _i = 0 to l - 1 do
      r := (!r lsl 1) lor (!n land 1) ;
      n := !n lsr 1
    done ;
    !r

  let reorg_coefficients n logn values =
    for i = 0 to n - 1 do
      let reverse_i = bitreverse i logn in
      if i < reverse_i then (
        let a_i = values.(i) in
        let a_ri = values.(reverse_i) in
        values.(i) <- a_ri ;
        values.(reverse_i) <- a_i)
    done

  let next_power_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    if 1 lsl logx = x then x else 1 lsl (logx + 1)
end
