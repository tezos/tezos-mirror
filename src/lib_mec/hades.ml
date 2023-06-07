module type PARAMETERS = sig
  val width : int

  val full_rounds : int

  val partial_rounds : int

  val round_constants : string array

  val linear_transformation : string array array

  val partial_round_idx_to_permute : int

  val alpha : Z.t
end

module Make (Param : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) = struct
  open Param

  (* Verify: - the constants are consistent - the sbox is a permutation, i.e.
     pgcd(alpha, p - 1) = 1

     IMPROVEME: - Verify the linear layer is secure. It requiers to implement
     different algorithms in Caml, and it does require a bit of code about
     linear algebra not available in the ecosystem AFAIK. Use the sage scripts
     in the meantime *)
  let () =
    assert (Array.length linear_transformation = width) ;
    assert (
      Array.for_all
        (fun line -> Array.length line = width)
        linear_transformation) ;
    assert (Z.(equal (gcd (Z.pred Scalar.order) alpha) one))

  let linear_transformation =
    Array.map (Array.map Scalar.of_string) linear_transformation

  let round_constants = Array.map Scalar.of_string round_constants

  (* Initialize only once an array for the MDS matrix multiplication *)
  let res = Array.make width Scalar.zero

  type state = {mutable i_round_key : int; state : Scalar.t array}

  let init state =
    if Array.length state != width then
      failwith
        (Printf.sprintf
           "State length is %d, but the width of the strategy is %d"
           (Array.length state)
           width)
    else {i_round_key = 0; state = Array.copy state}

  let get_next_round_key s =
    let v = round_constants.(s.i_round_key) in
    s.i_round_key <- s.i_round_key + 1 ;
    v

  let substitution x = Scalar.(pow x alpha)

  (* Functions prefixed with apply_ are modifying the state given in
     parameters *)
  let apply_round_key s =
    let state = s.state in
    for i = 0 to Array.length state - 1 do
      state.(i) <- Scalar.(get_next_round_key s + state.(i))
    done

  let apply_substitution_last_elem s =
    let s = s.state in
    s.(partial_round_idx_to_permute) <-
      substitution s.(partial_round_idx_to_permute)

  let apply_substitution s =
    let s = s.state in
    for i = 0 to Array.length s - 1 do
      s.(i) <- substitution s.(i)
    done

  let apply_permutation m v =
    let v = v.state in
    for j = 0 to width - 1 do
      for k = 0 to width - 1 do
        res.(k) <- Scalar.(res.(k) + (m.(k).(j) * v.(j)))
      done
    done ;
    for j = 0 to width - 1 do
      v.(j) <- res.(j) ;
      res.(j) <- Scalar.zero
    done

  let apply_partial_round s =
    apply_round_key s ;
    apply_substitution_last_elem s ;
    apply_permutation linear_transformation s

  let apply_full_round s =
    apply_round_key s ;
    apply_substitution s ;
    apply_permutation linear_transformation s

  let apply s =
    s.i_round_key <- 0 ;
    for _i = 0 to (full_rounds / 2) - 1 do
      apply_full_round s
    done ;
    for _i = 0 to partial_rounds - 1 do
      apply_partial_round s
    done ;
    for _i = 0 to (full_rounds / 2) - 1 do
      apply_full_round s
    done

  let get s = Array.copy s.state
end
