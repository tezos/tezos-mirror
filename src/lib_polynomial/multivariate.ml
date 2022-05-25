(** Module for multivariate rational functions, vectors and jacobian matrices on
    an arbitrary scalar field.
 *)

(* TODO: exp and log
 *)

module StringMap = struct
  include Map.Make (String)

  let of_list l = of_seq (List.to_seq l)

  let encoding : 'a Data_encoding.t -> 'a t Data_encoding.t =
   fun inner_enc ->
    let to_list m = List.of_seq @@ to_seq m in
    Data_encoding.(conv to_list of_list (list (tup2 string inner_enc)))

  let prefix_map ~prefix str_map =
    if prefix = "" then str_map
    else fold (fun k v acc -> add (prefix ^ k) v acc) str_map empty

  let monomial_of_list l =
    let l_with_degree = List.map (fun p -> (p, 1)) l in
    of_list l_with_degree
end

exception Monomial_non_positive_exponent of (string * int)

exception Polynomial_division_by_zero

exception Substitution_with_not_a_number

(* TYPE DEFINITIONS *)

(* Variables are represented as strings *)
type variable = string

module type MultiPoly_sig = sig
  type scalar

  module UPoly : Univariate.UNIVARIATE with type scalar := scalar

  type spec = scalar StringMap.t

  module Monomial : sig
    (** Monomials are represented as mapping from variables to their exponent
      All monomials created here have non-negative exponents
      This is to ensure the unicity of the writting of polynomials.
   *)
    type t = int StringMap.t

    (* Note: There is no zero monomial *)

    val encoding : t Data_encoding.t

    (** The unit monomial *)
    val one : t

    (** The "variable" monomial *)
    val singleton : variable -> t

    (** Returns the sorted list of variables occuring in the object *)
    val get_support : t -> variable list

    (** Comparison bewteen monomials
      Uses the lexicographical ordering on monomials, with lexicographical ordering on the variables.
      The ordering on the variables should not matter.
      What matters is that the constant monomial is the minimum for this order.
   *)
    val compare : t -> t -> int

    (** Equality, which assumes no zero exponents *)
    val equal : t -> t -> bool

    (** Returns the degree of the given variable in the given monomial *)
    val deg : t -> variable -> int

    (** Multiplies two monomials *)
    val mul : t -> t -> t

    (** Partial application in a monomial.
      Returns [None] if the result of the application would be 0
      Otherwise, returns a non-zero coefficient and the rest of the monomial.
   *)
    val apply : t -> spec -> (scalar * t) option

    (** Conversion to printable strings. [None] is for [one] *)
    val to_ascii : t -> string option

    val pp : Format.formatter -> t -> unit

    val add_prefix : string -> t -> t

    (** Infix operators *)
    module MonomialOperators : sig
      val ( * ) : t -> t -> t

      val ( = ) : t -> t -> bool

      val ( <> ) : t -> t -> bool
    end
  end

  module type MONOMIAL_MAP = sig
    include Map.S

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module MonomialMap : MONOMIAL_MAP with type key = Monomial.t

  module Polynomial : sig
    (** Polynomials are maps from monomials to their coefficient.
      These coefficients should be non-zero. An empty map is the zero polynomial.
   *)
    type t = scalar MonomialMap.t

    (** The zero polynomial *)
    val zero : t

    (** The unit polynomial *)
    val one : t

    (** The "variable" polynomial *)
    val singleton : variable -> t

    (** Builds a polynomial with a single monomial *)
    val of_monomial : Monomial.t -> t

    (** Builds a polynomial from a list of monomials *)
    val of_list : (Monomial.t * scalar) list -> t

    (** Returns the sorted list of variables occuring in the object *)
    val get_support : t -> variable list

    (* Returns the sorted list of variables occuring in the objects of the map *)
    val get_support_map : t StringMap.t -> variable list

    (** Equality between polynomials.
     Assuming monomials do not contain zero exponents,
     and the polynomials do not contain zero coefficients,
     the polynomial representation is unique,
     so equality is the equality of the coefficients for each of their monomial.
   *)
    val equal : t -> t -> bool

    (** Returns the maximum degree of the given variable in the given polynomial *)
    val deg : t -> variable -> int

    (** Adds two polynomials *)
    val add : t -> t -> t

    (** Negation of a polynomial *)
    val neg : t -> t

    (** Substraction of two polynomials *)
    val sub : t -> t -> t

    (** Multiplies a polynomial by a scalar *)
    val mul_scalar : scalar -> t -> t

    (** [normalize_poly p = (k,q)] verifies [scalar_poly k q = p]
   and [q] is normalized such that its smallest monomial has coef 1.
   The monomial ordering makes so that the constant term is always the smallest (if it exists).
   Also, the scalar [K.zero] is returned only if [p] is [zero], in which case
   [q] will also be equal to [zero].
 *)
    val normalize : t -> scalar * t

    (** Polynomial multiplication *)
    val mul : t -> t -> t

    (** [group_by p var] returns a list [p_0,p_1,...,p_k] such that
      [p = \sum var^i * p_i]
      [p = p_0 + var * (p_1 + var * ( ... + var * ( p_{k-1} + var * p_k ) ... ) )]
   *)
    val group_by : t -> variable -> t list

    (** [substitution p x q] replaces the variable [x] in [p] by [q] *)
    val substitution : t -> variable -> t -> t

    (** [substitution_comp p x a b] replaces the variables [x] and [y] (a fresh variable)
      in [y^n.p(x/y)] (where n = deg(x)) by [a] and [b] respectively. *)
    val substitution_comp : t -> variable -> t -> t -> t

    (** Returns the leading coefficient of [p] in the variable [x], with the degree of [x] *)
    val leading_coef : t -> variable -> t * int

    (** [partial_div_euclid p1 p2 y] returns [(q,r,d,n)] st:
      - p1 = (q/d^n) * p2 + (r/d^n)
      - if p1 and p2 are polynomials in Q[x_i,y], where the x_i are other variables, then q and r are polynomials in y, with coefficients in Q[x_i], and d is a polynomial in Q[x_i].
      - deg_y(r) < deg_y(p2)
   *)
    val partial_div_euclid : t -> t -> variable -> t * t * t * int

    (** [partial_gcd p1 p2 y] returns the gcd of p1 and p2 wrt the variable y *)
    val partial_gcd : t -> t -> variable -> t

    (** Partial application for polynomials *)
    val apply : t -> spec -> scalar

    val fast_apply : t -> spec -> scalar

    val partial_apply : t -> spec -> t

    (** Extract the constant value of the given polynomial *)
    val apply_poly : t -> UPoly.polynomial StringMap.t -> UPoly.polynomial

    (** Conversion to printable strings *)
    val to_ascii : t -> string

    val add_prefix : variable -> t -> t

    (** Prefix and infix operators *)
    module PolynomialOperators : sig
      val ( ~- ) : t -> t

      val ( + ) : t -> t -> t

      val ( - ) : t -> t -> t

      val ( * ) : t -> t -> t

      val ( *. ) : scalar -> t -> t

      val ( = ) : t -> t -> bool

      val ( <> ) : t -> t -> bool
    end
  end
end

(* module type MultiPoly_sig = sig *)
(*   type scalar *)

(*   module UPoly : Polynomial.UNIVARIATE with type scalar = scalar *)

module MultiPoly
    (K : Ff_sig.PRIME)
    (UPoly : Univariate.UNIVARIATE with type scalar = K.t) :
  MultiPoly_sig with type scalar = K.t and module UPoly = UPoly = struct
  type scalar = K.t

  module UPoly = UPoly

  type spec = scalar StringMap.t

  module Monomial = struct
    type t = int StringMap.t

    let encoding = StringMap.encoding Data_encoding.int31

    let one = StringMap.empty

    let singleton var = StringMap.singleton var 1

    let get_support m = List.map fst (StringMap.bindings m)

    let compare m1 m2 =
      let rec aux lb1 lb2 =
        match (lb1, lb2) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | (k1, e1) :: t1, (k2, e2) :: t2 -> (
            match Stdlib.compare k1 k2 with
            | 0 -> if e1 = e2 then aux t1 t2 else e1 - e2
            | c -> c)
      in
      aux (StringMap.bindings m1) (StringMap.bindings m2)

    let equal = StringMap.equal Int.equal

    let neq a b = not (equal a b)

    let deg m var = Option.value (StringMap.find_opt var m) ~default:0

    let mul : t -> t -> t =
      let aux_union _ e1 e2 = Some (e1 + e2) in
      StringMap.union aux_union

    let apply m spec : (K.t * t) option =
      let resulting_scalar, string_removed =
        StringMap.fold
          (fun string int (acc_res, acc_removed) ->
            let value_opt = StringMap.find_opt string spec in
            match value_opt with
            | Some value ->
                let scalar =
                  if int = 1 then value else K.pow value (Z.of_int int)
                in
                (K.mul acc_res scalar, string :: acc_removed)
            | None -> (acc_res, acc_removed))
          m
          (K.one, [])
      in
      if K.is_zero resulting_scalar then None
      else
        let resulting_monom =
          List.fold_left
            (fun monom_res string -> StringMap.remove string monom_res)
            m
            string_removed
        in
        Some (resulting_scalar, resulting_monom)

    (* let apply_single m var value =
         *   match StringMap.find_opt var m with
         *   | None -> Some (K.one, m)
         *   | Some exp ->
         *       if exp < 1 then raise (Monomial_non_positive_exponent (var, exp))
         *       else if K.is_zero value then None
         *       else Some (K.pow value (Z.of_int exp), StringMap.remove var m)
         * in
         * StringMap.fold
         *   (fun var value -> function
         *     | None -> None
         *     | Some (coef, m_acc) -> (
         *         match apply_single m_acc var value with
         *         | None -> None
         *         | Some (a_coef, m_acc) -> Some (K.mul coef a_coef, m_acc)))
         *   spec
         *   (Some (K.one, m)) *)

    let rec exp_poly p n =
      (* computes p^n for p polynomial and n ≥ 0 *)
      match Z.to_int n with
      | 0 -> UPoly.constants K.one
      | 1 -> p
      | 2 -> UPoly.polynomial_multiplication p p
      | _ ->
          if Z.(rem n (of_int 2) = zero) then
            (* (p²)^(n/2) *)
            exp_poly (exp_poly p (Z.of_int 2)) (Z.shift_right n 1)
          else
            (* p×(p²)^((n-1)/2) *)
            UPoly.polynomial_multiplication
              p
              (exp_poly
                 (exp_poly p (Z.of_int 2))
                 Z.(shift_right (n - of_int 1) 1))

    let apply_poly m spec =
      let apply_single m var value =
        match StringMap.find_opt var m with
        | None -> Some (UPoly.one, m)
        | Some exp ->
            if exp < 1 then raise (Monomial_non_positive_exponent (var, exp))
            else if UPoly.is_null value then None
            else Some (exp_poly value (Z.of_int exp), StringMap.remove var m)
      in
      StringMap.fold
        (fun var value -> function
          | None -> None
          | Some (coef, m_acc) -> (
              match apply_single m_acc var value with
              | None -> None
              | Some (a_coef, m_acc) ->
                  Some (UPoly.polynomial_multiplication coef a_coef, m_acc)))
        spec
        (Some (UPoly.one, m))

    let to_ascii m : string option =
      if StringMap.is_empty m then None
      else
        let aux_map (var, exp) =
          if exp = 1 then var else Printf.sprintf "%s ^ %d" var exp
        in
        Some (String.concat "  *  " (List.map aux_map (StringMap.bindings m)))

    let pp fmt t =
      Format.pp_print_string fmt (Option.value (to_ascii t) ~default:"")

    let fast_apply m spec =
      StringMap.fold
        (fun var int acc ->
          match StringMap.find_opt var spec with
          | None ->
              failwith
                (Printf.sprintf
                   "MPoly.fast_apply: '%s' not found in monomial %s"
                   var
                   (Option.get (to_ascii m)))
          | Some x ->
              let value = if int = 1 then x else K.pow x (Z.of_int int) in
              K.(acc * value))
        m
        K.one

    let add_prefix prefix monomial = StringMap.prefix_map ~prefix monomial

    module MonomialOperators = struct
      let ( * ) = mul

      let ( = ) = equal

      let ( <> ) = neq
    end
  end

  module type MONOMIAL_MAP = sig
    include Map.S

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module MonomialMap = struct
    include Map.Make (Monomial)

    let encoding inner_enc =
      let to_list m = List.of_seq @@ to_seq m in
      let of_list m = of_seq @@ List.to_seq m in
      Data_encoding.(
        conv to_list of_list (list (tup2 Monomial.encoding inner_enc)))
  end

  module Polynomial = struct
    type t = K.t MonomialMap.t

    let zero = MonomialMap.empty

    let one = MonomialMap.singleton Monomial.one K.one

    let singleton var = MonomialMap.singleton (Monomial.singleton var) K.one

    let of_monomial m = MonomialMap.singleton m K.one

    let get_support p =
      let ls =
        MonomialMap.fold (fun m _ ac -> Monomial.get_support m :: ac) p []
      in
      List.sort_uniq Stdlib.compare (List.flatten ls)

    let get_support_map p_map =
      let support =
        StringMap.fold
          (fun _ poly acc ->
            MonomialMap.fold
              (fun m _ ac -> Monomial.get_support m :: ac)
              poly
              []
            @ acc)
          p_map
          []
      in
      List.sort_uniq Stdlib.compare (List.flatten support)

    let equal = MonomialMap.equal K.eq

    let neq a b = not (equal a b)

    let deg p var =
      if equal p zero then -1
      else MonomialMap.fold (fun m _ acc -> max acc (Monomial.deg m var)) p 0

    let add : t -> t -> t =
      let aux_union _ sc1 sc2 =
        let sc = K.add sc1 sc2 in
        if K.is_zero sc then None else Some sc
      in
      MonomialMap.union aux_union

    let neg = MonomialMap.map (fun x -> K.negate x)

    let sub a b = add a (neg b)

    let mul_scalar : K.t -> t -> t =
     fun k p ->
      if K.is_zero k then zero
      else if K.is_one k then p
      else MonomialMap.map (fun x -> K.mul k x) p

    let normalize p : K.t * t =
      match MonomialMap.min_binding_opt p with
      | None ->
          (* p is the empty polynomial zero *)
          (K.zero, p)
      | Some (_, s) ->
          (* s should not be zero *)
          (s, mul_scalar (K.inverse_exn s) p)

    let mul : t -> t -> t =
     fun p1 p2 ->
      let fold_aux m1 sc1 p_acc =
        add
          (MonomialMap.fold
             (fun m2 sc2 acc ->
               MonomialMap.add (Monomial.mul m1 m2) (K.mul sc1 sc2) acc)
             p2
             zero)
          p_acc
      in
      MonomialMap.fold fold_aux p1 zero

    let rec group_by p var : t list =
      if p = zero then []
      else
        let rest, no_var_p =
          MonomialMap.partition (fun m _ -> StringMap.mem var m) p
        in
        let downgrade =
          StringMap.update var (function
              | None ->
                  assert false
                  (* The partition ensures this never happens in [rest] *)
              | Some i -> if i = 1 then None else Some (i - 1))
        in
        let rest =
          MonomialMap.fold
            (fun m q acc -> MonomialMap.add (downgrade m) q acc)
            rest
            zero
        in
        no_var_p :: group_by rest var

    let substitution p var p_rep : t =
      List.fold_left
        (fun p_acc el -> add el (mul p_acc p_rep))
        zero
        (List.rev (group_by p var))

    let substitution_comp p var x_rep y_rep : t =
      let g = group_by p var in
      let g =
        List.mapi
          (fun i s ->
            if i = 0 then s
            else mul (of_monomial (StringMap.singleton "__tmp__" i)) s)
          g
      in
      let r_x =
        List.fold_left (fun p_acc el -> add el (mul p_acc y_rep)) zero g
      in
      substitution r_x "__tmp__" x_rep

    let leading_coef p var =
      MonomialMap.fold
        (fun m c (acc, d) ->
          let dm = Monomial.deg m var in
          if dm < d then (acc, d)
          else if dm = d then
            (add acc (mul_scalar c (of_monomial (StringMap.remove var m))), d)
          else (mul_scalar c (of_monomial (StringMap.remove var m)), dm))
        p
        (zero, -1)

    let partial_div_euclid : t -> t -> variable -> t * t * t * int =
     fun p1 p2 y ->
      let l2, n2 = leading_coef p2 y in
      if n2 < 0 then (zero, p1, one, 1)
      else if n2 = 0 then (p1, zero, l2, 1)
      else
        let rec aux q r d_n =
          let _l1, n1 = leading_coef r y in
          if n1 < n2 then (q, r, d_n)
          else
            let tmp =
              MonomialMap.fold
                (fun m c acc ->
                  let nm = Monomial.deg m y in
                  if nm > n2 then
                    MonomialMap.add (StringMap.add y (nm - n2) m) c acc
                  else if nm = n2 then
                    MonomialMap.add (StringMap.remove y m) c acc
                  else acc)
                r
                zero
            in
            let q = add (mul l2 q) tmp in
            let r = sub (mul l2 r) (mul tmp p2) in
            aux q r (Int.succ d_n)
        in
        let a, b, c = aux zero p1 0 in
        if Int.equal c 0 then (a, b, one, 1) else (a, b, l2, c)

    let rec partial_gcd : t -> t -> variable -> t =
     fun p1 p2 y ->
      let d = deg p2 y in
      if d < 0 then p1
      else if d = 0 then one
      else
        let _, r, _, _ = partial_div_euclid p1 p2 y in
        partial_gcd p2 r y

    let partial_apply : t -> spec -> t =
     fun p x ->
      let fold_aux m sc p_acc =
        match Monomial.apply m x with
        | None -> p_acc
        | Some (coef, m_app) ->
            add (mul_scalar (K.mul sc coef) (of_monomial m_app)) p_acc
      in
      MonomialMap.fold fold_aux p zero

    let mul_poly k p =
      if UPoly.is_null k then zero
      else if UPoly.equal k UPoly.one then p
      else MonomialMap.map (fun x -> UPoly.polynomial_multiplication k x) p

    let of_monomial_poly m = MonomialMap.singleton m UPoly.one

    let of_list l =
      List.fold_left
        (fun acc (monom, coef) ->
          let poly = mul_scalar coef (of_monomial monom) in
          add acc poly)
        zero
        l

    let add_poly :
        UPoly.polynomial MonomialMap.t ->
        UPoly.polynomial MonomialMap.t ->
        UPoly.polynomial MonomialMap.t =
      let aux_union _ sc1 sc2 =
        let sc = UPoly.add sc1 sc2 in
        if UPoly.is_null sc then None else Some sc
      in
      MonomialMap.union aux_union

    let partial_apply_poly p x =
      (* sc = coeff of monomial m *)
      let fold_aux m sc p_acc =
        match Monomial.apply_poly m x with
        | None -> p_acc
        | Some (coef, m_app) ->
            add_poly
              (mul_poly (UPoly.mult_by_scalar sc coef) (of_monomial_poly m_app))
              p_acc
      in
      MonomialMap.fold fold_aux p zero

    let apply_at_0 p =
      let support = get_support p in
      if (not (MonomialMap.is_empty p)) && support != [] then
        let msg =
          let support = String.concat "', '" (get_support p) in
          String.concat
            ""
            [
              "Polynomial's variable(s) '";
              support;
              "' not found in evaluation map.";
            ]
        in
        failwith msg
      else
        match MonomialMap.find_opt Monomial.one p with
        | None -> K.zero
        | Some x -> x

    let apply_at_0_poly p =
      let support = get_support p in
      if (not (MonomialMap.is_empty p)) && support != [] then
        let msg =
          let support_list = String.concat "', '" support in
          String.concat
            ""
            [
              "Polynomial's variable(s) '";
              support_list;
              "' not found in evaluation map.";
            ]
        in
        failwith msg
      else
        match MonomialMap.find_opt Monomial.one p with
        | None -> UPoly.zero (* failwith "0" *)
        | Some x -> x

    let apply_poly p sp = apply_at_0_poly (partial_apply_poly p sp)

    let apply p sp = apply_at_0 (partial_apply p sp)

    let fast_apply p sp =
      MonomialMap.fold
        (fun monomial coef acc ->
          K.(acc + (coef * Monomial.fast_apply monomial sp)))
        p
        K.zero

    let to_ascii p : string =
      let sprint () = K.to_string in
      if equal p zero then "0"
      else
        let aux_map (m, coef) =
          match Monomial.to_ascii m with
          | None ->
              Printf.sprintf "%s" (Z.to_string (K.to_z coef)) (* TODO remove *)
          | Some ms ->
              if K.is_one coef then Printf.sprintf "%s" ms
              else Printf.sprintf "%a.(%s)" sprint coef ms
        in
        String.concat "   +   " (List.map aux_map (MonomialMap.bindings p))

    let add_prefix prefix poly =
      MonomialMap.fold
        (fun m x acc -> MonomialMap.add (Monomial.add_prefix prefix m) x acc)
        poly
        MonomialMap.empty

    module PolynomialOperators = struct
      let ( ~- ) = neg

      let ( + ) = add

      let ( - ) = sub

      let ( * ) = mul

      let ( *. ) = mul_scalar

      let ( = ) = equal

      let ( <> ) = neq
    end
  end
end
