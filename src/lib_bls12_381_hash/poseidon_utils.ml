let array_for_all2 p l1 l2 =
  let n1 = Array.length l1 and n2 = Array.length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else
    let rec loop i =
      if i = n1 then true
      else if p (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then
        loop (succ i)
      else false
    in
    loop 0

(** Linear algebra module, copied from {{:
      https://gitlab.com/nomadic-labs/privacy-team/-/blob/9e4050cb4a304848901c3434d61a8d7f0c7107c4/nuplompiler/linear_algebra.ml
      } Nomadic Labs privacy team repository } *)
module type Ring_sig = sig
  type t

  val add : t -> t -> t

  val mul : t -> t -> t

  val negate : t -> t

  val zero : t

  val one : t

  val eq : t -> t -> bool
end

module type Field_sig = sig
  include Ring_sig

  val inverse_exn : t -> t
end

(** This refers to the mathematical generalization of vector space called
      "module", where the field of scalars is replaced by a ring *)
module type Module_sig = sig
  type t

  type matrix = t array array

  (** [zeros r c] is a matrix with [r] rows and [c] columns filled with zeros *)
  val zeros : int -> int -> matrix

  (** [identity n] is the identity matrix of dimension [n] *)
  val identity : int -> matrix

  (** matrix equality *)
  val equal : matrix -> matrix -> bool

  (** matrix addition *)
  val add : matrix -> matrix -> matrix

  (** matrix multiplication *)
  val mul : matrix -> matrix -> matrix

  (** matrix transposition *)
  val transpose : matrix -> matrix

  (** [row_add ~coeff i j m] adds to the i-th row, the j-th row times coeff in m *)
  val row_add : ?coeff:t -> int -> int -> matrix -> unit

  (** [row_swap i j m] swaps the i-th and j-th rows of m *)
  val row_swap : int -> int -> matrix -> unit

  (** [row_mul coeff i m] multiplies the i-th row by coeff in m *)
  val row_mul : t -> int -> matrix -> unit

  (** [filter_cols f m] removes the columns of [m] whose index does not satisfy [f] *)
  val filter_cols : (int -> bool) -> matrix -> matrix

  (** splits matrix [m] into the first n columns and the rest, producing two matrices *)
  val split_n : int -> matrix -> matrix * matrix
end

module type VectorSpace_sig = sig
  include Module_sig

  (** reduced row Echelon form of m *)
  val reduced_row_echelon_form : matrix -> matrix

  (** [inverse m] is the inverse matrix of m

        @raise [Invalid_argument] if [m] is not invertible *)
  val inverse : matrix -> matrix
end

module Make_Module (Ring : Ring_sig) : Module_sig with type t = Ring.t = struct
  type t = Ring.t

  type matrix = t array array

  let zeros r c = Array.make_matrix r c Ring.zero

  let identity n =
    Array.(init n (fun i -> init n Ring.(fun j -> if i = j then one else zero)))

  let equal = array_for_all2 (array_for_all2 Ring.eq)

  let add = Array.(map2 (map2 Ring.add))

  let mul m1 m2 =
    let nb_rows = Array.length m1 in
    let nb_cols = Array.length m2.(0) in
    let n = Array.length m1.(0) in
    assert (Array.length m2 = n) ;
    let p = zeros nb_rows nb_cols in
    for i = 0 to nb_rows - 1 do
      for j = 0 to nb_cols - 1 do
        for k = 0 to n - 1 do
          p.(i).(j) <- Ring.(add p.(i).(j) @@ mul m1.(i).(k) m2.(k).(j))
        done
      done
    done ;
    p

  let transpose m =
    let nb_rows = Array.length m in
    let nb_cols = Array.length m.(0) in
    Array.(init nb_cols (fun i -> init nb_rows (fun j -> m.(j).(i))))

  let row_add ?(coeff = Ring.one) i j m =
    m.(i) <- Array.map2 Ring.(fun a b -> add a (mul coeff b)) m.(i) m.(j)

  let row_swap i j m =
    let aux = m.(i) in
    m.(i) <- m.(j) ;
    m.(j) <- aux

  let row_mul coeff i m = m.(i) <- Array.map (Ring.mul coeff) m.(i)

  let list_filteri p l =
    let rec aux i acc = function
      | [] -> List.rev acc
      | x :: l -> aux (i + 1) (if p i x then x :: acc else acc) l
    in
    aux 0 [] l

  let filter_cols f =
    Array.map (fun row ->
        list_filteri (fun i _ -> f i) (Array.to_list row) |> Array.of_list)

  let split_n n m =
    (filter_cols (fun i -> i < n) m, filter_cols (fun i -> i >= n) m)
end

module Make_VectorSpace (Field : Field_sig) :
  VectorSpace_sig with type t = Field.t = struct
  include Make_Module (Field)

  let reduced_row_echelon_form m =
    let n = Array.length m in
    (* returns the first non-zero index in the row *)
    let find_pivot row =
      let rec aux cnt = function
        | [] -> None
        | x :: xs -> if Field.(eq zero x) then aux (cnt + 1) xs else Some cnt
      in
      aux 0 (Array.to_list row)
    in
    let move_zeros_to_bottom m =
      let is_non_zero_row = Array.exists (fun a -> not Field.(eq zero a)) in
      let rec aux nonzeros zeros = function
        | [] -> Array.of_list (List.rev nonzeros @ zeros)
        | r :: rs ->
            if is_non_zero_row r then aux (r :: nonzeros) zeros rs
            else aux nonzeros (r :: zeros) rs
      in
      aux [] [] (Array.to_list m)
    in
    let rec aux k =
      if k >= Array.length m then m
      else
        match find_pivot m.(k) with
        | Some j when j < n ->
            row_mul (Field.inverse_exn m.(k).(j)) k m ;
            Array.iteri
              (fun i _ ->
                if i <> k then row_add ~coeff:Field.(negate @@ m.(i).(j)) i k m)
              m ;
            row_swap k j m ;
            aux (k + 1)
        | _ -> aux (k + 1)
    in
    aux 0 |> move_zeros_to_bottom

  let inverse m =
    let n = Array.length m in
    let id_n = identity n in
    let augmented = Array.(map2 append m id_n) in
    let reduced = reduced_row_echelon_form augmented in
    let residue, inv = split_n n reduced in
    let is_zero_row = Array.for_all Field.(eq zero) in
    if Array.exists is_zero_row residue then
      raise @@ Invalid_argument "matrix [m] is not invertible"
    else inv
end

(* Partial copy of
   https://gitlab.com/dannywillems/ocaml-polynomial/-/blob/950dc70e8c3070918329d1e7e722f8361c25e182/src/polynomial.ml#L233 *)
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

let compute_updated_constants_one_batch width batch_size mds k_cols =
  let var_name ?(s2 = "") s1 i = s1 ^ "_" ^ string_of_int i ^ s2 in
  (* We will represent the variables with monomials of increasing degree.
     For width = 3 and batch = 4, we would have the following matching:
       x_0, x_1, x_2^5, a,  a^5,  b,  b^5,  c,  c^5, y_0,  y_1,  y_2
        |    |    |     |    |    |    |    |    |    |     |    |
       x^1, x^2, x^3,  x^4, x^5, x^6, x^7, x^8, x^9, x^10, x^11, x^12 *)
  let module Poly = MakePoly (Bls12_381.Fr) in
  let module Poly_Module = Make_Module (struct
    include Poly

    type t = Poly.t

    let eq = Poly.equal

    let negate p = Poly.(sub zero p)

    let mul = Poly.( * )
  end) in
  (* For convenience, we will store the variables and their degree in a
     StringMap: key = variable's name, value = degree. *)
  (* We start by create a map for each variables we need *)
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
  let pvar s = Poly.of_coefficients [(Bls12_381.Fr.one, get_var s)] in

  (* We convert the MDS and round constants into matrices of constant polynomials. *)
  let to_poly = Array.(map (map (fun c -> Poly.of_coefficients [(c, 0)]))) in
  let matrix = to_poly mds in
  let k_cols = Array.map to_poly k_cols in

  (* And now, we will compute the coefficients *)
  (* We store in variable "state", the state after the first SBox. *)
  let state =
    (* [| [| pvar "x0" |]; [| pvar "x1" |]; [| pvar "x2_5" |] |] *)
    Array.init width (fun i ->
        if i != width - 1 then [|pvar (var_name "x" i)|]
        else [|pvar (var_name "x" i ~s2:"^alpha")|])
  in

  (* We start by computing the temporary state and saving corresponding polys
     for each of them.
        a = α_31 x_0 + α_32 x_1 + α_33 x_2^alpha                     + κ_0
        b = β_1  x_1 + β_2  x_1 + β_3  x_2^alpha + β_a a^5           + κ_b
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
      List.(rev coeffs @ init size (fun _ -> Bls12_381.Fr.zero))
      |> Array.of_list
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

let compute_updated_constants r_p r_f width batch_size arc mds =
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
  (* The remaining constants are for the last set of full rounds. As a round ends
     with adding the round keys, there are (r_f / 2 - 1) * width constants left. *)
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
          compute_updated_constants_one_batch width batch_size mds k_cols
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
