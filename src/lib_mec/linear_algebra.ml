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

  let equal = Array.(for_all2 (for_all2 Ring.eq))

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

  let filter_cols f =
    Array.map (fun row ->
        List.filteri (fun i _ -> f i) (Array.to_list row) |> Array.of_list)

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
