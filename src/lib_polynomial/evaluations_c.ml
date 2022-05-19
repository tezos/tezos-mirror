module Fr = Bls12_381.Fr

module Stubs = struct
  type fr = Polynomial_c.Stubs.fr

  type fr_array = Polynomial_c.Stubs.fr_array

  external get : fr -> fr_array -> int -> unit = "caml_polynomial_get_stubs"

  (** [add res a b size_a size_b]

   requires:
   - [size_a <= size_b]
   - [size a = size_a]
   - [size b = size_b]
   - [size res = size_a] *)
  external add : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_evaluations_add_stubs"
    [@@noalloc]

  (** [rescale res a size_res size_a]

   requires:
   - [size res = size_res]
   - [size a = size_a]
   - [size_res <= size_a] *)
  external rescale : fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_evaluations_rescale_stubs"
    [@@noalloc]

  (** [mul_arrays res eval_evallen_comp_power_powlen size_res nb_evals]

   requires:
   - [size res = size_res]
   - [size eval_evallen_comp_power_powlen = nb_evals]
   - [size eval_evallen_comp_power_powlen.[i].[0] = eval_evallen_comp_power_powlen.[i].[1]]
   - [size_bits eval_evallen_comp_power_powlen.[i].[3] = eval_evallen_comp_power_powlen.[i].[4]] *)
  external mul_arrays :
    fr_array ->
    (fr_array * int * int * Bytes.t * int) array ->
    int ->
    int ->
    unit = "caml_polynomial_evaluations_mul_arrays_stubs"
    [@@noalloc]

  (** [linear_arrays res eval_evallen_coeff_comp add_constant size_res nb_evals]

   requires:
   - [size res = size_res]
   - [size eval_evallen_coeff_comp = nb_evals]
   - [size eval_evallen_coeff_comp.[i].[0] = eval_evallen_coeff_comp.[i].[1]] *)
  external linear_arrays :
    fr_array -> (fr_array * int * fr * int) array -> fr -> int -> int -> unit
    = "caml_polynomial_evaluations_linear_arrays_stubs"
    [@@noalloc]

  (** [fft_inplace p domain log] computes Fast Fourier Transform.
  It converts the coefficient representation of a polynomial [p] to
  the evaluation representation

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!compute_domain}) *)
  external fft_inplace : fr_array -> domain:fr_array -> log:int -> unit
    = "caml_polynomial_fft_inplace_on_stubs"
    [@@noalloc]

  (** [ifft_inplace p domain log] computes Inverse Fast Fourier Transform.
  It converts the evaluation representation of a polynomial [p] to
  the coefficient representation

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!compute_domain}) *)
  external ifft_inplace : fr_array -> domain:fr_array -> log:int -> unit
    = "caml_polynomial_ifft_inplace_on_stubs"
    [@@noalloc]
end

module type Evaluations_c_sig = sig
  type scalar

  type polynomial

  type t

  val make_evaluation : int * scalar array -> t

  val string_of_eval : t -> string

  type domain

  val of_domain : domain -> t

  val to_domain : t -> domain

  val zero : t

  val degree : t -> int

  val length : t -> int

  val get : t -> int -> scalar

  val mul_by_scalar : scalar -> t -> t

  val mul_c :
    evaluations:(string * t) list ->
    ?composition_gx:int list * int ->
    ?powers:int list ->
    unit ->
    t

  val linear_c :
    evaluations:(string * t) list ->
    ?linear_coeffs:scalar list ->
    ?composition_gx:int list * int ->
    ?add_constant:scalar ->
    unit ->
    t

  val add : t -> t -> t

  (*restrictive equality, the same polynomial evaluated
    on different domains are said to be different*)
  val equal : t -> t -> bool

  val evaluation_fft : domain -> polynomial -> t

  val interpolation_fft : domain -> t -> polynomial

  (* TODO DELETE *)
  val evaluation_fft2 : domain -> polynomial -> scalar array

  val interpolation_fft2 : domain -> scalar array -> polynomial
end

module Evaluations_c_impl = struct
  module Domain = Domain.Domain_unsafe
  module Polynomial = Polynomial_c.Polynomial_unsafe

  type scalar = Fr.t

  type polynomial = Polynomial.t

  (* degree & evaluations & length *)
  type t = int * Stubs.fr_array * int

  type domain = Domain.t

  (* TODO: define this function once*)
  let sort_by_size (eval_1, size_1) (eval_2, size_2) =
    if size_1 >= size_2 then (eval_1, eval_2, size_1, size_2)
    else (eval_2, eval_1, size_2, size_1)

  let make_evaluation (d, p) =
    if d < 0 then
      raise @@ Invalid_argument "make_evaluation: degree must be non negative" ;
    if Array.length p <= d then
      raise
      @@ Invalid_argument "make_evaluation: array must be longer than degree" ;
    let res, len = Carray.of_array p in
    (d, res, len)

  let string_of_eval (d, e, l) =
    Printf.sprintf "%d : [%s]" d (Carray.to_string (e, l))

  let of_domain domain =
    let d, l = Domain.to_carray domain in
    (1, d, l)

  let to_domain (_, eval, l) = Domain.of_carray (eval, l)

  let zero = (-1, Carray.allocate 1, 1)

  let degree (d, _, _) = d

  let length (_, _, l) = l

  let get (_, eval, l) i =
    if i < 0 || i >= l then raise @@ Invalid_argument "get: index out of bounds" ;
    let res = Fr.(copy zero) in
    Stubs.get res eval i ;
    res

  let mul_by_scalar lambda (deg, eval, len) =
    let res = Carray.allocate len in
    Polynomial_c.Stubs.mul_by_scalar res lambda eval len ;
    (deg, res, len)

  (* multiplies evaluations of all polynomials with name in poly_names,
     the resulting eval has the size of the smallest evaluation *)
  let mul_c ~evaluations
      ?(composition_gx = (List.init (List.length evaluations) (fun _ -> 0), 1))
      ?(powers = List.init (List.length evaluations) (fun _ -> 1)) () =
    let domain_len = snd composition_gx in
    assert (domain_len != 0) ;
    let deg_result, list_array, is_zero, (name_min, min_length_eval) =
      (* List.fold_left for 3 lists of same size *)
      let rec fold_left3 f acc l1 l2 l3 =
        match (l1, l2, l3) with
        | [], [], [] -> acc
        | a1 :: l1, a2 :: l2, a3 :: l3 -> fold_left3 f (f acc a1 a2 a3) l1 l2 l3
        | _ ->
            raise
              (Invalid_argument "fold_left3 : lists don’t have the same size.")
      in
      fold_left3
        (fun (acc_degree, list, is_zero, (min_name, min_length_eval))
             x
             composition
             pow ->
          let name, (d, eval, l) = x in
          let is_zero = if d = -1 then true else is_zero in
          let new_min_length_eval =
            if l < min_length_eval then (name, l)
            else (min_name, min_length_eval)
          in
          let deg_to_add = if d = -1 then 0 else d * pow in
          let pow = Z.of_int pow in
          let pow_bytes = Z.to_bits pow |> Bytes.unsafe_of_string in
          let pow_len = Z.numbits pow in
          let rescale_composition = composition * l / domain_len in
          ( acc_degree + deg_to_add,
            (* ! here we reverse but it's ok because mul is commutative*)
            (eval, l, rescale_composition, pow_bytes, pow_len) :: list,
            is_zero,
            new_min_length_eval ))
        (0, [], false, ("", Int.max_int))
        evaluations
        (fst composition_gx)
        powers
    in
    if is_zero then zero
    else
      let log = Z.(log2up (of_int deg_result)) in
      let length_degree = Z.(to_int (one lsl log)) in
      if length_degree > min_length_eval then
        raise
          (Invalid_argument
             (Printf.sprintf
                "Utils.mul_evaluations : %s's evaluation is too short \
                 (length=%d) for expected result size %d"
                name_min
                min_length_eval
                length_degree))
      else
        let nb_evals = List.length evaluations in
        let res = Carray.allocate min_length_eval in
        Stubs.mul_arrays res (Array.of_list list_array) min_length_eval nb_evals ;
        (deg_result, res, min_length_eval)

  (* Adds evaluation of a1 × p1 + a2 × p2 in evaluations
     /!\ the degree may not be always accurate,
         the resulting degree may not be the max of the 2 polynomials degrees *)
  let linear_c ~evaluations
      ?(linear_coeffs = List.init (List.length evaluations) (fun _ -> Fr.one))
      ?(composition_gx = (List.init (List.length evaluations) (fun _ -> 0), 1))
      ?(add_constant = Fr.zero) () =
    let domain_len = snd composition_gx in
    assert (domain_len != 0) ;
    let list_eval_coeff_composition =
      List.map2
        (fun ((_, eval), coeff) composition ->
          let rescale_composition = composition * length eval / domain_len in
          (eval, coeff, rescale_composition))
        (List.combine evaluations linear_coeffs)
        (fst composition_gx)
      |> List.filter (fun ((d, _eval, _length), _, _) -> d != -1)
    in
    let l =
      List.sort
        (fun ((_, _, length_1), _, _) ((_, _, length_2), _, _) ->
          Int.compare length_1 length_2)
        list_eval_coeff_composition
    in
    let length_result =
      if List.compare_length_with l 0 = 0 then 0
      else
        let (_, _, res), _, _ = List.hd l in
        res
    in
    if length_result = 0 then zero
    else
      let l =
        List.sort
          (fun ((d1, _, _), _, _) ((d2, _, _), _, _) -> Int.compare d2 d1)
          list_eval_coeff_composition
      in
      let degree_result =
        if List.compare_length_with l 0 = 0 then -1
        else
          let (res, _, _), _, _ = List.hd l in
          res
      in
      let nb_evals = List.length list_eval_coeff_composition in
      let res = Carray.allocate length_result in
      let list_eval_coeff_composition =
        List.map
          (fun ((_, eval, eval_len), linear_coeff, composition) ->
            (eval, eval_len, linear_coeff, composition))
          list_eval_coeff_composition
      in
      Stubs.linear_arrays
        res
        (Array.of_list list_eval_coeff_composition)
        add_constant
        length_result
        nb_evals ;
      (degree_result, res, length_result)

  (* Adds 2 evaluations *)
  let add (d1, eval1, l1) (d2, eval2, l2) =
    if d1 = -1 then (d2, eval2, l2)
    else if d2 = -1 then (d1, eval1, l1)
    else
      let deg_result = max d1 d2 in
      let arg_bigger, arg_smaller, size_bigger, size_smaller =
        sort_by_size (eval1, l1) (eval2, l2)
      in
      let length_result = size_smaller in
      let res = Carray.allocate length_result in
      Stubs.add res arg_smaller arg_bigger size_smaller size_bigger ;
      (deg_result, res, length_result)

  (*restrictive equality, the same polynomial evaluated
    on different domains are said to be different*)
  let equal (deg1, eval1, l1) (deg2, eval2, l2) =
    if deg1 <> deg2 || l1 <> l2 then false
    else Carray.Stubs.eq eval1 eval2 l1 l2

  let evaluation_fft_internal : Domain.t -> polynomial -> Carray.t =
   fun domain poly ->
    let domain, n_domain = Domain.to_carray domain in
    let poly, size_poly = Polynomial.to_carray poly in
    let log = Z.(log2up @@ of_int n_domain) in
    if not Z.(log = log2 @@ of_int n_domain) then
      raise @@ Invalid_argument "Size of domain should be a power of 2." ;
    (* See MR from Marc for padding *)
    if not (size_poly <= n_domain) then
      raise @@ Invalid_argument "Size of poly should be smaller than domain." ;
    let dense_evaluation = Carray.allocate n_domain in
    Carray.Stubs.copy dense_evaluation poly size_poly ;
    Stubs.fft_inplace dense_evaluation ~domain ~log ;
    (dense_evaluation, n_domain)

  let evaluation_fft : domain -> polynomial -> t =
   fun domain poly ->
    let res, len = evaluation_fft_internal domain poly in
    let d = Polynomial.degree poly in
    (d, res, len)

  let evaluation_fft2 : Domain.t -> polynomial -> scalar array =
   fun domain p -> Carray.to_array (evaluation_fft_internal domain p)

  let interpolation_fft_internal : Domain.t -> Carray.t -> polynomial =
   fun domain (coefficients, n_coefficients) ->
    let domain, n_domain = Domain.to_carray domain in
    let log = Z.(log2up @@ of_int n_domain) in
    if not Z.(log = log2 @@ of_int n_domain) then
      raise @@ Invalid_argument "Size of domain should be a power of 2." ;
    if not (n_coefficients = n_domain) then
      raise @@ Invalid_argument "Size of coefficients should be same as domain." ;
    Stubs.ifft_inplace coefficients ~domain ~log ;
    Polynomial.of_carray (coefficients, n_coefficients)

  let interpolation_fft : domain -> t -> polynomial =
   fun domain (d, evaluation, l) ->
    if d = -1 then Polynomial.zero
    else
      let length_res = Domain.length domain in
      let rescaled_eval = Carray.allocate length_res in
      Stubs.rescale rescaled_eval evaluation length_res l ;
      interpolation_fft_internal domain (rescaled_eval, length_res)

  let interpolation_fft2 : Domain.t -> scalar array -> polynomial =
   fun domain coefficients ->
    interpolation_fft_internal domain (Carray.of_array coefficients)
end

include (
  Evaluations_c_impl :
    Evaluations_c_sig
      with type scalar = Bls12_381.Fr.t
       and type domain = Domain.t
       and type polynomial = Polynomial_c.t)
