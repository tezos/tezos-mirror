module MakePoly (R : Bls12_381.Ff_sig.PRIME) = struct
  (* We encode the polynomials as a list with decreasing degree.
     Invariants to respect for the type:
     - all coefficients are non null.
     - [a_n * X^n + ... a_1 X + a0] is encoded as [a_n ; ... ; a_1 ; a_0] with [a_i]
     non zero for all [i], i.e. the monomials are given in decreasing order.
     - the zero polynomial is represented as the empty list.
  *)
  type t = (R.t * int) list

  let zero = []

  let one = [(R.one, 0)]

  let of_coefficients l =
    (* check if the powers are all positive *)
    assert (List.for_all (fun (_e, power) -> power >= 0) l) ;
    (* Remove null coefficients *)
    let l = List.filter (fun (e, _power) -> not (R.is_zero e)) l in
    (* sort by the power, higher power first *)
    let l =
      List.fast_sort
        (fun (_e1, power1) (_e2, power2) -> Int.sub power2 power1)
        l
    in
    l

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | [] -> [R.zero]
    | l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

  let add p1 p2 =
    let rec inner acc l1 l2 =
      match (l1, l2) with
      | [], l | l, [] -> List.rev_append acc l
      | l1, l2 ->
          let e1, p1 = List.hd l1 in
          let e2, p2 = List.hd l2 in
          if p1 = p2 && R.is_zero (R.add e1 e2) then
            inner acc (List.tl l1) (List.tl l2)
          else if p1 = p2 then
            inner ((R.add e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
          else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
          else inner ((e2, p2) :: acc) l1 (List.tl l2)
    in
    let l = inner [] p1 p2 in
    of_coefficients l

  let mul p q =
    let mul_by_monom (scalar, int) p =
      List.map (fun (scalar_2, int_2) -> (R.mul scalar scalar_2, int + int_2)) p
    in
    List.fold_left (fun acc monom -> add acc (mul_by_monom monom q)) zero p

  let equal p1 p2 =
    if List.compare_lengths p1 p2 != 0 then false
    else List.for_all2 (fun (e1, n1) (e2, n2) -> n1 = n2 && R.eq e1 e2) p1 p2

  let opposite poly = List.(rev (rev_map (fun (a, i) -> (R.negate a, i)) poly))

  let sub p1 p2 =
    let rec inner acc l1 l2 =
      match (l1, l2) with
      | [], l2 -> List.rev_append acc (opposite l2)
      | l1, [] -> List.rev_append acc l1
      | l1, l2 ->
          let e1, p1 = List.hd l1 in
          let e2, p2 = List.hd l2 in
          if p1 = p2 && R.is_zero (R.sub e1 e2) then
            inner acc (List.tl l1) (List.tl l2)
          else if p1 = p2 then
            inner ((R.sub e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
          else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
          else inner ((R.negate e2, p2) :: acc) l1 (List.tl l2)
    in
    let l = inner [] p1 p2 in
    of_coefficients l

  let ( - ) = sub

  let ( * ) = mul
end

let compute_updated_constants_one_batch (type a)
    (module Fp : Bls12_381.Ff_sig.PRIME with type t = a) width batch_size mds
    k_cols =
  let var_name ?(s2 = "") s1 i = s1 ^ "_" ^ string_of_int i ^ s2 in
  (* We will represent the variables with monomials of increasing degree.
     For width = 3 and batch = 4, we would have the following matching:
       x_0, x_1, x_2^5, a,  a^5,  b,  b^5,  c,  c^5, y_0,  y_1,  y_2
        |    |    |     |    |    |    |    |    |    |     |    |
       x^1, x^2, x^3,  x^4, x^5, x^6, x^7, x^8, x^9, x^10, x^11, x^12 *)
  let module Poly = MakePoly (Fp) in
  let module Poly_Module = Linear_algebra.Make_Module (struct
    include Poly

    type t = Poly.t

    let eq = Poly.equal

    let negate p = Poly.(sub zero p)

    let mul = Poly.( * )
  end) in
  (* For convenience, we will store the variables and their degree in a
     StringMap: key = variable's name, value = degree. *)
  let module SMap = Map.Make (String) in
  let varsMap =
    let vars =
      (* The initial state of the batch: x_0, ..., x_{width-2}, x_{width-1}^alpha. *)
      let init_state =
        List.init width (fun i ->
            if i != width - 1 then var_name "x" i
            else var_name "x" i ~s2:"^alpha")
      in
      (* The temporary variables of the batch:
         tmp_0 (= a above), tmp_0^5, ..., tmp_{batch-2}, tmp_{batch-2}^5. *)
      let tmp =
        List.concat
          (List.init (batch_size - 1) (fun i ->
               [var_name "tmp" i; var_name "tmp" i ~s2:"^alpha"]))
      in
      (* The final state of the batch: y_0, ..., y_{width-1}. *)
      let final_state = List.init width (fun i -> var_name "y" i) in
      init_state @ tmp @ final_state
    in
    SMap.of_seq @@ List.(to_seq @@ mapi (fun i s -> (s, i + 1)) vars)
  in
  let get_var s = try SMap.find s varsMap with e -> raise e in
  let pvar s = Poly.of_coefficients [(Fp.one, get_var s)] in

  (* We store in variable "state", the state after the first SBox. *)
  let state =
    (* [| [| pvar "x0" |]; [| pvar "x1" |]; [| pvar "x2_5" |] |] *)
    Array.init width (fun i ->
        if i != width - 1 then [|pvar (var_name "x" i)|]
        else [|pvar (var_name "x" i ~s2:"^alpha")|])
  in

  (* We convert the MDS and round constants into matrices of constant polynomials. *)
  let to_poly = Array.(map (map (fun c -> Poly.of_coefficients [(c, 0)]))) in
  let matrix = to_poly mds in
  let k_cols = Array.map to_poly k_cols in

  (* Computing tmp state and saving corresponding polys for each of them.
     a = α_31 x_0 + α_32 x_1 + α_33 x_2^alpha + κ_0
     b = β_1  x_1 + β_2  x_1 + β_3  x_2^alpha + β_a a^5 + κ_b
     c = γ_1  x_1 + γ_2  x_1 + γ_3  x_2^alpha + γ_a a^5 + γ_b b^5 + κ_c
  *)
  let dummy_list = List.init (batch_size - 1) (fun i -> i) in
  let state, polys =
    List.fold_left
      (fun (acc_state, acc_poly) i ->
        (* Updating state by applying the MDS matrix and round constants. *)
        let state = Poly_Module.(add (mul matrix acc_state) @@ k_cols.(i)) in
        (* Updating state by applying SBox, defining the next tmp var
           and saving tmp state to retrieve coeffs later. *)
        let permuted_var = pvar (var_name "tmp" i) in
        let poly = Poly.(state.(Int.pred width).(0) - permuted_var) in
        state.(width - 1) <- [|pvar (var_name "tmp" i ~s2:"^alpha")|] ;
        (state, poly :: acc_poly))
      (state, [])
      dummy_list
  in
  let polys_tmp_var = List.rev polys in

  (* Computing last state.
      y_0 = δ_11 x_0 + δ_12 x_1 + δ_13 x_2^alpha + δ_1a a^alpha + δ_1b b^alpha + δ_1c c^alpha + κ_y1
      y_1 = δ_21 x_0 + δ_22 x_1 + δ_23 x_2^alpha + δ_2a a^alpha + δ_2b b^alpha + δ_2c c^alpha + κ_y2
      y_2 = δ_31 x_0 + δ_32 x_1 + δ_33 x_2^alpha + δ_3a a^alpha + δ_3b b^alpha + δ_3c c^alpha + κ_y3
  *)
  let state = Poly_Module.(add (mul matrix state) @@ k_cols.(batch_size - 1)) in
  (* Saving final polynomials to retrieve coeffs later. *)
  let polys_final =
    List.init width (fun i -> Poly.(state.(i).(0) - pvar (var_name "y" i)))
  in

  let polys =
    let nb_coefs = SMap.cardinal varsMap + 1 in
    let row_of_eq eq =
      (* This function gives coefficients in decending order of degree *)
      let coeffs = Poly.get_dense_polynomial_coefficients eq in
      let size = nb_coefs - List.length coeffs in
      List.(rev coeffs @ init size (fun _ -> Fp.zero)) |> Array.of_list
    in
    List.map row_of_eq (polys_tmp_var @ polys_final)
  in
  (* We retrieve the updated coefficients and round constants for each round/poly. *)
  let coeffs, _, _ =
    (* We list the name of the variables whose coeff need to be retrieved. *)
    let vars =
      List.init width (fun i ->
          if i != width - 1 then var_name "x" i else var_name "x" i ~s2:"^alpha")
    in
    (* We iterate over each equations/polys in the paper (c.f. page 8 Fig 5). *)
    List.fold_left
      (fun (coeffs, i, vars) poly ->
        (* For each batched round, we retrieve the constants: the coefficients
           first followed by the round constant. *)
        let coeffs =
          coeffs @ List.map (fun s -> poly.(get_var s)) vars @ [poly.(0)]
        in
        (* We update vars by appending the new tmp variable. *)
        let new_var = [var_name "tmp" i ~s2:"^alpha"] in
        let vars = if i < batch_size - 1 then vars @ new_var else vars in
        (* We retrieve the round constant, which is the polynomial constant. *)
        (coeffs, i + 1, vars))
      ([], 0, vars)
      polys
  in
  coeffs

let compute_updated_constants (type a)
    (module Fp : Bls12_381.Ff_sig.PRIME with type t = a) r_p r_f width
    batch_size arc mds =
  (* We retrieve the partial rounds' round constants by offsetting with the
     number of constants used in the first full rounds and the first shifted
     partial round. *)
  let arc_offset = (r_f * width / 2) + width in
  let nb_batch = r_p / batch_size in
  let arc_per_batch = batch_size * width in
  (* We retrieve the ARC constants for each batch. *)
  let batched_arc =
    Array.init nb_batch (fun i ->
        Array.sub arc (arc_offset + (i * arc_per_batch)) arc_per_batch)
  in
  (* We retrieve the remaining ARC constants. *)
  let unbatched_arc_offset = arc_offset + (arc_per_batch * nb_batch) in
  let unbatched_arc_size = r_p mod batch_size * width in
  let unbatched_arc = Array.sub arc unbatched_arc_offset unbatched_arc_size in
  (* The remainin constants are for the last set of full rounds. As a round ends
     with adding the round keys, there are (r_f / 2 - 1) * width constants left *)
  let arc_full_round_end =
    Array.sub
      arc
      (unbatched_arc_offset + unbatched_arc_size)
      (((r_f / 2) - 1) * width)
  in
  let constants =
    Array.fold_left
      (fun acc ks ->
        (* We split the ARC constants per batch in chunks of size width and
           format them in a matrix, each batch corresponding to the constants
           used in one partial round.*)
        let k_cols =
          Array.init batch_size (fun i ->
              Array.init width (fun j -> [|ks.((i * width) + j)|]))
        in
        let batch_constants =
          compute_updated_constants_one_batch
            (module Fp)
            width
            batch_size
            mds
            k_cols
        in
        acc @ batch_constants)
      []
      batched_arc
  in
  let arc_full_round_start = Array.sub arc 0 arc_offset in
  ( arc_full_round_start,
    Array.of_list constants,
    unbatched_arc,
    arc_full_round_end )

module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The total number of full rounds *)
  val full_rounds : int

  (** The number of partial rounds *)
  val partial_rounds : int

  (** The number of partial round to batch. It must be between 1 and the number
      of partial rounds *)
  val batch_size : int

  (** The round constants, given in decimal representation

      Secure round constants can be constructed using {{:
      https://gitlab.com/dannywillems/ocaml-ec/-/tree/master/utils/poseidon-hash
      } Sage scripts provided in this repository } *)
  val round_constants : string array

  (** The linear transformation, given in decimal representation.

      Secure linear transformations can be constructed using {{:
      https://gitlab.com/dannywillems/ocaml-ec/-/tree/master/utils/poseidon-hash
      } Sage scripts provided in this repository } *)
  val linear_transformation : string array array

  (** The index of the element of the state to permute during the partial
      rounds *)
  val partial_round_idx_to_permute : int

  (** The exponent to be used in the sbox *)
  val alpha : Z.t
end

module Make (Param : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) = struct
  open Param

  (* Verify:
     - the constants are consistent
     - the sbox is a permutation, i.e. pgcd(alpha, p - 1) = 1

     IMPROVEME:
     - Verify the linear layer is secure. It requiers to implement
       different algorithms in Caml, and it does require a bit of code about
       linear algebra not available in the ecosystem AFAIK. Use the sage scripts
       in the meantime
     - chechk the batch size consistency
  *)
  let () =
    assert (Array.length linear_transformation = width) ;
    assert (
      Array.for_all
        (fun line -> Array.length line = width)
        linear_transformation) ;
    assert (batch_size >= 1 && batch_size <= partial_rounds) ;
    assert (Z.(equal (gcd (Z.pred Scalar.order) alpha) one))

  let linear_transformation =
    Array.map (Array.map Scalar.of_string) linear_transformation

  let round_constants = Array.map Scalar.of_string round_constants

  let nb_batched_partial_rounds = Param.(partial_rounds / batch_size)

  let nb_unbatched_partial_rounds = Param.(partial_rounds mod batch_size)

  (* We compute the round constants on the fly, when initializing the module *)
  let ( arc_full_round_start_with_first_partial,
        arc_intermediate_state,
        arc_unbatched,
        arc_full_round_end ) =
    compute_updated_constants
      (module Scalar)
      Param.partial_rounds
      Param.full_rounds
      Param.width
      Param.batch_size
      round_constants
      linear_transformation

  let round_constants =
    Array.concat
      [
        arc_full_round_start_with_first_partial;
        arc_intermediate_state;
        arc_unbatched;
        arc_full_round_end;
        (* Adding for the last round as we apply the round key at the end of
           a full round *)
        Array.make width Scalar.zero;
      ]

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

  (* Functions prefixed with apply_ modify the state given in
     parameters *)
  let apply_round_key s =
    let state = s.state in
    for i = 0 to Array.length state - 1 do
      let r = get_next_round_key s in
      state.(i) <- Scalar.(r + state.(i))
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
    (* First, we apply the substitution *)
    apply_substitution_last_elem s ;
    (* And after that, we apply the linear transformation followed by the
       constant *)
    apply_permutation linear_transformation s ;
    apply_round_key s

  let apply_batched_partial_round s =
    let nb_tmp_vars = Param.batch_size - 1 in
    (* We want to compute
       [s0, s1, s2^5, var_1^5, ..., var_n^5] to use it when computing the final
       state.
       The final state is simply a linear combinaison of this list.
       var_1 := a0 * s0 + a1 * s1 + a2 * s2^5 + kappa
       We will compute
          acc = (a0 * s0) + zero
          acc = (a1 * s1) + acc
          acc = (a2 * s2^5) + acc
          acc = acc + kappa
       We can do the first 3 additions with a fold_right on [s2^5, s1, s0]
       starting with zero, and we add kappa at the end.
       For var_i, we will do the same on [var_(i - 1)^5, ..., var_1^5, s2^5,
       ..., s0]
    *)
    let rec aux i interm_state =
      if i = nb_tmp_vars then interm_state
      else
        let res =
          List.fold_right
            (fun interm_state_i acc ->
              Scalar.(acc + (interm_state_i * get_next_round_key s)))
            interm_state
            Scalar.zero
        in
        let res = Scalar.(res + get_next_round_key s) in
        let res = substitution res in
        aux (i + 1) (res :: interm_state)
    in
    (* get [s2, s1, s0] *)
    let interm_state = List.rev (Array.to_list s.state) in
    (* call aux with s2^5 :: [s1, s0] *)
    let interm_state =
      substitution (List.hd interm_state) :: List.tl interm_state
    in
    let interm_state = aux 0 interm_state in
    Array.iteri
      (fun i _ ->
        let res =
          List.fold_right
            (fun interm_state_i acc ->
              Scalar.(acc + (interm_state_i * get_next_round_key s)))
            interm_state
            Scalar.zero
        in
        let res = Scalar.(res + get_next_round_key s) in
        Array.set s.state i res)
      s.state

  let apply_full_round s =
    (* We apply the substitution, followed by the linear transformation and the
       round constant *)
    apply_substitution s ;
    apply_permutation linear_transformation s ;
    apply_round_key s

  let apply s =
    s.i_round_key <- 0 ;
    (* We apply the first round constants (as rounds are shifted) *)
    apply_round_key s ;
    (* We apply the first set of full rounds *)
    for _i = 0 to (full_rounds / 2) - 1 do
      apply_full_round s
    done ;
    (* We apply the optimized batched partial rounds *)
    for _i = 0 to nb_batched_partial_rounds - 1 do
      apply_batched_partial_round s
    done ;
    (* We apply the remaining unbatched partial rounds *)
    for _i = 0 to nb_unbatched_partial_rounds - 1 do
      apply_partial_round s
    done ;
    (* We apply the last set of full rounds *)
    for _i = 0 to (full_rounds / 2) - 1 do
      apply_full_round s
    done

  let get s = Array.copy s.state
end
