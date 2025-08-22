(*
    For each curve form and each coordinates, an external link MUST be given
    pointing to the addition and doubling formulas. The addition MUST support
    doubling. It is verified by the ECProperties functor.

    When adding a new curve form/coordinates, it is REQUIRED to add
    - the number of additions (A),
    - the number of additions with a constant (AC),
    - the number of multiplications (M),
    - the number of multiplications with a constant (MC),
    - the number of negations (N),
    - the number of substractions (SU),
    - the number of divisions (DI),
    - the number of doublings (DO),
    - the number of squarings (SQ).

    If there is a requirement for the parameters, it MUST be verified at the
    module top level. It is RECOMMENDED, but not required, to use a specific
    exception and raise it with a meaningful message.

    When implementing a new form or new coordinates, it MUST be accompanied by
    the instantiation of a standard/popular curve and test vectors in addition
    to the generic PBT on EC properties. The origin of the test vectors MUST be
    documented.

    If any constant are defined at the top level, it MUST be accompanied by an
    explanation on where it is used, and why.

    In the case of an optimisation not listed in the associated paper/link
    describing the formulas, the author MUST add an explanation. If it reduces
    the number of field operations, the "original" and the "optimised" versions
    MUST be compared in comments.

    IMPROVEME:
    - For the moment, there is no ec.mli. When adding a new form or new
      coordinates, the functor signature MUST be added in the interface. If you
      read these lines and there is a file ec.mli, please open an issue.
    - For the moment, it is only RECOMMENDED to use a particular exception when
      verifying the parameters. It must be changed to a REQUIREMENT.
    - For the moment, there is no distinction between `not being on the curve`
      and `not being in the prime subgroup`. This MUST change.
*)

open Bls12_381

module MakeJacobianWeierstrass
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME)
    (Params : sig
      val a : Fq.t

      val b : Fq.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.JacobianWeierstrassT with type Scalar.t = Fp.t and type Base.t = Fq.t =
struct
  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module Base = Fq
  module Scalar = Fp

  let a = Params.a

  let b = Params.b

  (* checking the curve is non-singular *)
  let () =
    if Base.is_zero Base.((a * square a) + (of_string "27" * square b)) then
      failwith "a^3 + 27 * b^2 must be different than zero"

  let cofactor = Params.cofactor

  type t = {x : Fq.t; y : Fq.t; z : Fq.t}

  let size_in_bytes = Fq.size_in_bytes * 3

  let zero = {x = Fq.zero; y = Fq.one; z = Fq.zero}

  let is_zero t = Fq.(t.x = zero) && Fq.(t.z = zero)

  let eq t1 t2 =
    if Fq.(is_zero t1.z) && Fq.(is_zero t2.z) then true
    else if Fq.is_zero t1.z || Fq.is_zero t2.z then false
    else
      let t1z2 = Fq.(square t1.z) in
      let t1z3 = Fq.(t1z2 * t1.z) in
      let t2z2 = Fq.(square t2.z) in
      let t2z3 = Fq.(t2z2 * t2.z) in
      let x1 = Fq.(t1.x / t1z2) in
      let x2 = Fq.(t2.x / t2z2) in
      let y1 = Fq.(t1.y / t1z3) in
      let y2 = Fq.(t2.y / t2z3) in
      Fq.(x1 = x2 && y1 = y2)

  (* https://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-2007-bl *)
  let double t =
    if is_zero t then zero
    else
      let {x; y; z} = t in
      let xx = Fq.(square x) in
      let yy = Fq.(square y) in
      let yyyy = Fq.(square yy) in
      let zz = Fq.(square z) in
      let s = Fq.(double (square (x + yy) + negate xx + negate yyyy)) in
      let m = Fq.(xx + xx + xx + (a * square zz)) in
      let t = Fq.(square m + negate (double s)) in
      let x3 = t in
      let y3 =
        Fq.((m * (s + negate t)) + negate (double (double (double yyyy))))
      in
      let z3 = Fq.(square (y + z) + negate yy + negate zz) in
      {x = x3; y = y3; z = z3}

  (* https://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl *)
  let add t1 t2 =
    if is_zero t1 then t2
    else if is_zero t2 then t1
    else if eq t1 t2 then double t1
    else
      let {x = x1; y = y1; z = z1} = t1 in
      let {x = x2; y = y2; z = z2} = t2 in
      let z1z1 = Fq.(z1 * z1) in
      let z2z2 = Fq.(z2 * z2) in
      let u1 = Fq.(x1 * z2z2) in
      let u2 = Fq.(x2 * z1z1) in
      let s1 = Fq.(y1 * z2 * z2z2) in
      let s2 = Fq.(y2 * z1 * z1z1) in
      let h = Fq.(u2 + negate u1) in
      let i = Fq.(square (double h)) in
      let j = Fq.(h * i) in
      let r = Fq.(double (s2 + negate s1)) in
      let v = Fq.(u1 * i) in
      let x3 = Fq.(square r + negate j + negate (double v)) in
      let y3 = Fq.((r * (v + negate x3)) + negate (double (s1 * j))) in
      let z3 = Fq.((square (z1 + z2) + negate z1z1 + negate z2z2) * h) in
      {x = x3; y = y3; z = z3}

  let negate {x; y; z} = {x; y = Fq.negate y; z}

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let a, r = Z.ediv_rem n two_z in
        if Z.equal r Z.zero then aux (double x) a else add x (aux x (Z.pred n))
    in
    aux x (Scalar.to_z n)

  let is_on_curve ~x ~y ~z =
    if Fq.is_zero x && Fq.is_zero z then true
    else if Fq.is_zero z then false
    else
      let z2 = Fq.(square z) in
      let z3 = Fq.(z * z2) in
      let x' = Fq.(x / z2) in
      let y' = Fq.(y / z3) in
      Fq.((x' * x' * x') + (a * x') + b = y' * y')

  let is_in_prime_subgroup ~x ~y ~z =
    let p = {x; y; z} in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let of_bytes_opt bytes =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length bytes <> size_in_bytes then None
    else
      let x_bytes = Bytes.sub bytes 0 Fq.size_in_bytes in
      let y_bytes = Bytes.sub bytes Fq.size_in_bytes Fq.size_in_bytes in
      let z_bytes = Bytes.sub bytes (2 * Fq.size_in_bytes) Fq.size_in_bytes in
      let x = Fq.of_bytes_opt x_bytes in
      let y = Fq.of_bytes_opt y_bytes in
      let z = Fq.of_bytes_opt z_bytes in
      match (x, y, z) with
      | None, _, _ | _, None, _ | _, _, None -> None
      (* Verify it is on the curve *)
      | Some x, Some y, Some z ->
          if Fq.is_zero x && Fq.is_zero z then Some zero
          else if Fq.is_zero z then None
          else if is_on_curve ~x ~y ~z && is_in_prime_subgroup ~x ~y ~z then
            Some {x; y; z}
          else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    match of_bytes_opt b with
    | Some g -> g
    | None -> raise (Not_on_curve b)

  let to_bytes g =
    let buffer = Bytes.make size_in_bytes '\000' in
    Bytes.blit (Fq.to_bytes g.x) 0 buffer 0 Fq.size_in_bytes ;
    Bytes.blit (Fq.to_bytes g.y) 0 buffer Fq.size_in_bytes Fq.size_in_bytes ;
    Bytes.blit
      (Fq.to_bytes g.z)
      0
      buffer
      (2 * Fq.size_in_bytes)
      Fq.size_in_bytes ;
    buffer

  let one = of_bytes_exn Params.bytes_generator

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let x = Fq.random () in
      let y_square = Fq.((x * x * x) + (a * x) + b) in
      let y_opt = Fq.sqrt_opt y_square in
      match y_opt with
      | None -> aux ()
      | Some y -> mul {x; y; z = Fq.one} (Scalar.of_z cofactor)
    in
    aux ()

  let get_x_coordinate t = t.x

  let get_y_coordinate t = t.y

  let get_z_coordinate t = t.z

  let from_coordinates_exn ~x ~y ~z =
    if is_on_curve ~x ~y ~z then {x; y; z}
    else
      raise
        (Not_on_curve
           (Bytes.concat
              Bytes.empty
              [Fq.to_bytes x; Fq.to_bytes y; Fq.to_bytes z]))

  let from_coordinates_opt ~x ~y ~z =
    if is_on_curve ~x ~y ~z then Some {x; y; z} else None

  let get_affine_x_coordinate t =
    if is_zero t then failwith "Zero"
    else
      let z2 = Fq.(square t.z) in
      Fq.(t.x / z2)

  let get_affine_y_coordinate t =
    if is_zero t then failwith "Zero"
    else
      let z3 = Fq.(square t.z * t.z) in
      Fq.(t.y / z3)

  let from_affine_coordinates_exn ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one

  let from_affine_coordinates_opt ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one
end

module MakeAffineWeierstrass
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME)
    (Params : sig
      val a : Fq.t

      val b : Fq.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.AffineWeierstrassT with type Scalar.t = Fp.t and type Base.t = Fq.t =
struct
  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module Base = Fq
  module Scalar = Fp

  let a = Params.a

  let b = Params.b

  (* checking the curve is non-singular *)
  let () =
    if Base.is_zero Base.((a * square a) + (of_string "27" * square b)) then
      failwith "a^3 + 27 * b^2 must be different than zero"

  let cofactor = Params.cofactor

  type t = Infinity | P of (Fq.t * Fq.t)

  let size_in_bytes = Fq.size_in_bytes * 2

  let zero = Infinity

  let buffer_zero = Bytes.make size_in_bytes '\000'

  let is_zero t = match t with Infinity -> true | _ -> false

  let is_on_curve ~x ~y = Fq.((x * x * x) + (a * x) + b = y * y)

  let to_bytes g =
    let buffer = Bytes.make size_in_bytes '\000' in
    match g with
    | Infinity -> buffer
    | P (x, y) ->
        Bytes.blit (Fq.to_bytes x) 0 buffer 0 Fq.size_in_bytes ;
        Bytes.blit (Fq.to_bytes y) 0 buffer Fq.size_in_bytes Fq.size_in_bytes ;
        buffer

  let eq t1 t2 =
    match (t1, t2) with
    | Infinity, Infinity -> true
    | Infinity, _ | _, Infinity -> false
    | P (x1, y1), P (x2, y2) -> Fq.(x1 = x2 && y1 = y2)

  let double t =
    match t with
    | Infinity -> Infinity
    | P (x, y) ->
        let xx = Fq.(square x) in
        let xx_3_plus_a = Fq.(double xx + xx + a) in
        let double_x = Fq.(double x) in
        let double_y = Fq.(double y) in
        let square_double_y = Fq.(square double_y) in
        let x3 =
          Fq.((square xx_3_plus_a / square_double_y) + negate double_x)
        in
        let triple_x = Fq.(x + double_x) in
        let y3 =
          Fq.(
            (triple_x * xx_3_plus_a / double_y)
            + (negate (square xx_3_plus_a * xx_3_plus_a)
               / (square_double_y * double_y)
              + negate y))
        in
        P (x3, y3)

  (* https://hyperelliptic.org/EFD/g1p/auto-shortw.html *)
  let add t1 t2 =
    match (t1, t2) with
    | Infinity, t2 -> t2
    | t1, Infinity -> t1
    | t1, t2 when eq t1 t2 -> double t1
    | P (x1, y1), P (x2, y2) ->
        if Fq.(x1 = x2 && y1 = negate y2) then Infinity
        else
          let y2_min_y1 = Fq.(y2 + negate y1) in
          let x2_min_x1 = Fq.(x2 + negate x1) in
          let slope = Fq.(y2_min_y1 / x2_min_x1) in
          let square_slope = Fq.(square slope) in
          let x3 = Fq.(square_slope + negate x1 + negate x2) in
          let double_x1 = Fq.(double x1) in
          let double_x1_plus_x2 = Fq.(double_x1 + x2) in
          let y3 =
            Fq.(
              (double_x1_plus_x2 * slope)
              + negate (square_slope * slope)
              + negate y1)
          in
          P (x3, y3)

  let negate p =
    match p with Infinity -> Infinity | P (x, y) -> P (x, Fq.negate y)

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let a, r = Z.ediv_rem n two_z in
        if Z.equal r Z.zero then aux (double x) a else add x (aux x (Z.pred n))
    in
    aux x (Scalar.to_z n)

  let is_in_prime_subgroup ~x ~y =
    let p = P (x, y) in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let of_bytes_opt bytes =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length bytes <> size_in_bytes then None
    else
      let x_bytes = Bytes.sub bytes 0 Fq.size_in_bytes in
      let y_bytes = Bytes.sub bytes Fq.size_in_bytes Fq.size_in_bytes in
      if Bytes.equal buffer_zero bytes then Some Infinity
      else
        let x = Fq.of_bytes_opt x_bytes in
        let y = Fq.of_bytes_opt y_bytes in
        match (x, y) with
        | None, _ | _, None -> None
        (* Verify it is on the curve *)
        | Some x, Some y ->
            if is_on_curve ~x ~y && is_in_prime_subgroup ~x ~y then
              Some (P (x, y))
            else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    match of_bytes_opt b with
    | Some g -> g
    | None -> raise (Not_on_curve b)

  let one = of_bytes_exn Params.bytes_generator

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let x = Fq.random () in
      let y_square = Fq.((x * x * x) + (a * x) + b) in
      let y_opt = Fq.sqrt_opt y_square in
      match y_opt with
      | None -> aux ()
      | Some y -> mul (P (x, y)) (Scalar.of_z Params.cofactor)
    in
    aux ()

  let get_x_coordinate t =
    match t with Infinity -> raise (Invalid_argument "Zero") | P (x, _y) -> x

  let get_y_coordinate t =
    match t with Infinity -> raise (Invalid_argument "Zero") | P (_x, y) -> y

  (* FIXME:
     To have functions to Montgomery, we need to be able to compute alpha which
     is a root of the equation: z^3 + a*z + b = 0 which requires the cubic root
     operation in F_q.
     There is an open issue in ocaml-ff:
     https://gitlab.com/dannywillems/ocaml-ff/-/issues/21

     let alpha =
       let two = Fq.of_string "2" in
       let four = Fq.of_string "4" in
       let twenty_seven = Fq.of_string "27" in
       let a_cube = Fq.(mul a (square a)) in
       let b_square = Fq.square b in
       let delta = Fq.( b_square + negate ( (4 * a_cube ) / twenty_seven) ) in
       let sqrt_delta = Fq.sqrt_opt delta in
       if Option.is_none sqr_delta then assert (1=0);
       let sqrt_delta = Option.get sqr_delta in
       let u = Fq.(negate ((sqrt_delta + b) / two) ) in
       let v = Fq.( (sqrt_delta + negate b) / two) in
       let u_cbrt = Fq.cbrt_opt u in
       let v_cbrt = Fq.cbrt_opt v in
       if Option.(is_none u_cbrt || is_none v_cbrt) then assert (1=0);
       Fq.(u_cbrt + v_cbrt)

     let s =
       let three = Fq.of_string "3" in
       let aux = Fq.(three * (square alpha) + a)
       let sqrt_aux = Fq.sqrt_opt aux in
       if Option.is_none sqrt_aux then assert (1=0);
       Fq.( 1 / (Option.get sqrt_aux) )

     (* https://en.wikipedia.org/wiki/Montgomery_curve *)
     let to_montgomery t =
       match t with
         | Infinity -> raise (Invalid_argument "Zero")
         | P (x, y) ->
           Some (Fq.(s * (x - alpha)), Fq.(s*y))

     let to_montgomery_curve_parameters () =
       let three = Fr.of_string "3" in
       let cond = Fq.(three * (square alpha) * a) in
       if not (Fq.is_quadratic_residue cond) then None
       else
         let gen = Option.get (to_montgomery one) in
         let a = Fq.( three * alpha * s) in
         let b = s in
         Some (a, b, Params.cofactor, gen)
  *)

  let from_coordinates_exn ~x ~y =
    if is_on_curve ~x ~y then P (x, y)
    else
      raise
        (Not_on_curve (Bytes.concat Bytes.empty [Fq.to_bytes x; Fq.to_bytes y]))

  let from_coordinates_opt ~x ~y =
    if is_on_curve ~x ~y then Some (P (x, y)) else None

  let of_compressed_bytes_opt bs =
    (* required to avoid side effect! *)
    let bs = Bytes.copy bs in
    let length = Bytes.length bs in
    if length <> Base.size_in_bytes then None
    else if bs = Bytes.make Base.size_in_bytes '\000' then Some Infinity
    else
      (* We get the last bit of the input, representing the bit of u. We also
         remove the last bit from the bytes we received
      *)
      let last_byte = int_of_char @@ Bytes.get bs (length - 1) in
      let sign = last_byte lsr 7 in
      let last_byte_without_sign = last_byte land 0b01111111 in
      Bytes.set bs (length - 1) (char_of_int last_byte_without_sign) ;
      (* We compute u *)
      let x = Base.of_bytes_opt bs in
      match x with
      | None -> None
      | Some x -> (
          let yy = Base.((x * x * x) + (a * x) + b) in
          let y_opt = Base.sqrt_opt yy in
          let y =
            match y_opt with
            | None -> None
            | Some y ->
                (* computed before for constant time *)
                let negated_y = Base.negate y in
                let y_first_byte = Bytes.get (Base.to_bytes y) 0 in
                let is_sign_flipped =
                  int_of_char y_first_byte lxor sign land 1
                in
                Some (if is_sign_flipped = 0 then y else negated_y)
          in
          match y with
          | Some y ->
              if is_in_prime_subgroup ~x ~y then Some (P (x, y)) else None
          | None -> None)

  let of_compressed_bytes_exn b =
    match of_compressed_bytes_opt b with
    | None -> raise (Not_on_curve b)
    | Some p -> p

  let to_compressed_bytes p =
    match p with
    | Infinity -> Bytes.make Base.size_in_bytes '\000'
    | P (x, y) ->
        let x_bytes = Base.to_bytes x in
        let y_bytes = Base.to_bytes y in
        let y_first_byte = int_of_char (Bytes.get y_bytes 0) in
        let x_last_byte =
          int_of_char (Bytes.get x_bytes (Base.size_in_bytes - 1))
        in
        (* Get the first bit of y, i.e. the sign of y *)
        let sign_of_y = y_first_byte land 0b00000001 in
        (* Set the last bit of the last byte of x to the sign of y *)
        let x_last_byte_with_y = x_last_byte lor (sign_of_y lsl 7) in
        Bytes.set
          x_bytes
          (Base.size_in_bytes - 1)
          (char_of_int x_last_byte_with_y) ;
        x_bytes
end

module MakeProjectiveWeierstrass
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME)
    (Params : sig
      val a : Fq.t

      val b : Fq.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.ProjectiveWeierstrassT with type Scalar.t = Fp.t and type Base.t = Fq.t =
struct
  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module Base = Fq
  module Scalar = Fp

  let a = Params.a

  let b = Params.b

  (* checking the curve is non-singular *)
  let () =
    if Base.is_zero Base.((a * square a) + (of_string "27" * square b)) then
      failwith "a^3 + 27 * b^2 must be different than zero"

  let cofactor = Params.cofactor

  type t = {x : Fq.t; y : Fq.t; z : Fq.t}

  let size_in_bytes = Fq.size_in_bytes * 3

  let zero = {x = Fq.zero; y = Fq.one; z = Fq.zero}

  let is_zero t = Fq.(t.x = zero) && Fq.(t.z = zero)

  let is_on_curve ~x ~y ~z =
    if Fq.is_zero x && Fq.is_zero z then true
    else if Fq.is_zero z then false
    else
      let x' = Fq.(x / z) in
      let y' = Fq.(y / z) in
      Fq.((x' * x' * x') + (a * x') + b = y' * y')

  let add t1 t2 =
    (* See https://github.com/o1-labs/snarky/blob/master/snarkette/elliptic_curve.ml *)
    if is_zero t1 then t2
    else if is_zero t2 then t1
    else
      let open Fq in
      let x1z2 = t1.x * t2.z in
      let x2z1 = t1.z * t2.x in
      let y1z2 = t1.y * t2.z in
      let y2z1 = t1.z * t2.y in
      if x1z2 = x2z1 && y1z2 = y2z1 then
        (* Double case *)
        let xx = square t1.x in
        let zz = square t1.z in
        let w = (a * zz) + (xx + xx + xx) in
        let y1z1 = t1.y * t1.z in
        let s = y1z1 + y1z1 in
        let ss = square s in
        let sss = s * ss in
        let r = t1.y * s in
        let rr = square r in
        let b = square (t1.x + r) + negate xx + negate rr in
        let h = square w + negate (b + b) in
        let x3 = h * s in
        let y3 = (w * (b + negate h)) + negate (rr + rr) in
        let z3 = sss in
        {x = x3; y = y3; z = z3}
      else
        (* Generic case *)
        let z1z2 = t1.z * t2.z in
        let u = y2z1 + negate y1z2 in
        let uu = square u in
        let v = x2z1 + negate x1z2 in
        let vv = square v in
        let vvv = v * vv in
        let r = vv * x1z2 in
        let a = (uu * z1z2) + negate (vvv + r + r) in
        let x3 = v * a in
        let y3 = (u * (r + negate a)) + negate (vvv * y1z2) in
        let z3 = vvv * z1z2 in
        {x = x3; y = y3; z = z3}

  let double t = add t t

  let negate {x; y; z} = {x; y = Fq.negate y; z}

  let eq t1 t2 =
    if Fq.(is_zero t1.z) && Fq.(is_zero t2.z) then true
    else if Fq.is_zero t1.z || Fq.is_zero t2.z then false
    else
      let x1 = Fq.(t1.x / t1.z) in
      let x2 = Fq.(t2.x / t2.z) in
      let y1 = Fq.(t1.y / t1.z) in
      let y2 = Fq.(t2.y / t2.z) in
      Fq.(x1 = x2 && y1 = y2)

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let a, r = Z.ediv_rem n two_z in
        if Z.equal r Z.zero then aux (double x) a else add x (aux x (Z.pred n))
    in
    aux x (Scalar.to_z n)

  let is_in_prime_subgroup ~x ~y ~z =
    let p = {x; y; z} in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let of_bytes_opt bytes =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length bytes <> size_in_bytes then None
    else
      let x_bytes = Bytes.sub bytes 0 Fq.size_in_bytes in
      let y_bytes = Bytes.sub bytes Fq.size_in_bytes Fq.size_in_bytes in
      let z_bytes = Bytes.sub bytes (2 * Fq.size_in_bytes) Fq.size_in_bytes in
      let x = Fq.of_bytes_opt x_bytes in
      let y = Fq.of_bytes_opt y_bytes in
      let z = Fq.of_bytes_opt z_bytes in
      match (x, y, z) with
      | None, _, _ | _, None, _ | _, _, None -> None
      (* Verify it is on the curve *)
      | Some x, Some y, Some z ->
          if Fq.is_zero x && Fq.is_zero z then Some zero
          else if Fq.is_zero z then None
          else if is_on_curve ~x ~y ~z && is_in_prime_subgroup ~x ~y ~z then
            Some {x; y; z}
          else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    match of_bytes_opt b with
    | Some g -> g
    | None -> raise (Not_on_curve b)

  let to_bytes g =
    let buffer = Bytes.make size_in_bytes '\000' in
    Bytes.blit (Fq.to_bytes g.x) 0 buffer 0 Fq.size_in_bytes ;
    Bytes.blit (Fq.to_bytes g.y) 0 buffer Fq.size_in_bytes Fq.size_in_bytes ;
    Bytes.blit
      (Fq.to_bytes g.z)
      0
      buffer
      (2 * Fq.size_in_bytes)
      Fq.size_in_bytes ;
    buffer

  let one = of_bytes_exn Params.bytes_generator

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let x = Fq.random () in
      let y_square = Fq.((x * x * x) + (a * x) + b) in
      let y_opt = Fq.sqrt_opt y_square in
      match y_opt with
      | None -> aux ()
      | Some y -> mul {x; y; z = Fq.one} (Scalar.of_z cofactor)
    in
    aux ()

  let get_x_coordinate t = t.x

  let get_y_coordinate t = t.y

  let get_z_coordinate t = t.z

  let from_coordinates_exn ~x ~y ~z =
    if is_on_curve ~x ~y ~z && is_in_prime_subgroup ~x ~y ~z then {x; y; z}
    else
      raise
        (Not_on_curve
           (Bytes.concat
              Bytes.empty
              [Fq.to_bytes x; Fq.to_bytes y; Fq.to_bytes z]))

  let from_coordinates_opt ~x ~y ~z =
    if is_on_curve ~x ~y ~z && is_in_prime_subgroup ~x ~y ~z then Some {x; y; z}
    else None

  let get_affine_x_coordinate t =
    if is_zero t then failwith "Zero" else Fq.(t.x / t.z)

  let get_affine_y_coordinate t =
    if is_zero t then failwith "Zero" else Fq.(t.y / t.z)

  let from_affine_coordinates_exn ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one

  let from_affine_coordinates_opt ~x ~y = from_coordinates_exn ~x ~y ~z:Fq.one
end

module MakeAffineMontgomery
    (Fq : Ff_sig.PRIME)
    (Fp : Ff_sig.PRIME)
    (Params : sig
      val a : Fq.t

      val b : Fq.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.AffineMontgomeryT with type Scalar.t = Fp.t and type Base.t = Fq.t =
struct
  (* Costello, Craig, and Benjamin Smith. "Montgomery curves and their
     arithmetic."
     Journal of Cryptographic Engineering 8.3 (2018): 227-240.
     (https://arxiv.org/pdf/1703.01863.pdf) *)

  (* Summary for the complexity:
     Addition: 6A + 1AC + 1M + 1MC + 4N + 1DI
     Doubling: 4A + 2AC + 1M + 4MC + 3N + 1DI + 1DO + 2SQ
  *)

  (* Checking non-singularity:
     - if b=0 then the Curve is a union of 3 lines;
       if a^2=4 then the curve is a nodal cubic. *)
  let () = assert (not Fq.(eq (square Params.a) (Fq.of_string "4")))

  let () = assert (not (Fq.is_zero Params.b))

  exception Not_on_curve of Bytes.t

  module Base = Fq
  module Scalar = Fp

  (* used later in addition and doubling formulas *)
  let two = Fq.(one + one)

  let three = Fq.(one + two)

  let a = Params.a

  let two_a = Fq.mul two a

  let b = Params.b

  let two_b = Fq.mul two b

  let three_b = Fq.mul three b

  let cofactor = Params.cofactor

  type t = Infinity | P of (Fq.t * Fq.t)

  let size_in_bytes = Fq.size_in_bytes * 2

  let zero = Infinity

  let buffer_zero = Bytes.make size_in_bytes '\000'

  let is_zero t = match t with Infinity -> true | _ -> false

  let eq t1 t2 =
    match (t1, t2) with
    | Infinity, Infinity -> true
    | Infinity, _ | _, Infinity -> false
    | P (x1, y1), P (x2, y2) -> Fq.(x1 = x2 && y1 = y2)

  (* The operation for adding two points are the same whether we add the same or
     distinct points
     except for the calculus of the slope.
     (https://arxiv.org/pdf/1703.01863.pdf)

     Let P(x_p, y_p) + P(x_q, y_q) = P(x_r, y_r), then
       x_r = Params.b * slope^2 - (x_p + x_q) - Params.a
       y_r = (2 * x_p + x_ q + Params.a) * slope - Params.b * slope^3 - y_p
           = slope * (x_p - x_q) - y_p
  *)
  (* Complexity (update above summary if any change):
         1SQ
       + 1MC + 1MC + 1A + 1AC + 1MC + 1DI   -> 3MC + 1A + 1AC + 1DI (SLOPE COMPUTATION)
       + 1SQ + 1DO + 1MC + 1AC + 1N + 1A                            (X COMPUTATION)
       + 1N + 1N + 1A + 1M + 1A             -> 2N + 2A + 1M         (Y COMPUTATION USING X)
       Total:
         4A + 2AC + 1M + 4MC + 3N + 1DI + 1DO + 2SQ
  *)
  let double t =
    match t with
    | Infinity -> Infinity
    | P (x, y) ->
        (* Slope computation *)
        let xx = Fq.(square x) in
        (* slope = (3 * x^2 + 2 * Params.a * x + 1) / (2 * Params.b * y) *)
        let three_xx = Fq.(three * xx) in
        let two_a_x = Fq.(two_a * x) in
        let l_num = Fq.(three_xx + two_a_x) in
        let l_num = Fq.(l_num + Fq.one) in
        let l_div = Fq.(two_b * y) in
        let l = Fq.(l_num / l_div) in
        (* x computation *)
        let ll = Fq.square l in
        let two_x = Fq.double x in
        let b_ll = Fq.(b * ll) in
        let a_two_x = Fq.(a + two_x) in
        let neg_a_two_x = Fq.(negate a_two_x) in
        let x3 = Fq.(b_ll + neg_a_two_x) in
        (* computing y3 by using x3, see 2.2. There is a typo when giving a
           shorter formula for y. It must be x_plus instead of x_q
        *)
        let neg_x3 = Fq.negate x3 in
        let neg_y = Fq.negate y in
        let x_plus_neg_x3 = Fq.(x + neg_x3) in
        let l_x_plus_neg_x3 = Fq.(l * x_plus_neg_x3) in
        let y3 = Fq.(l_x_plus_neg_x3 + neg_y) in
        P (x3, y3)

  (* Complexity (update above summary if any change):
       (SLOPE COMPUTATION)
       1N + 1N + 1A + 1A + 1DI
       -> 2A + 2N + 1DI
       (X COMPUTATION)
     + 1SQ + 1A + 1MC + 1AC + 1N + 1A
       -> 2A + 1AC + 1MC + 1N + 1SQ
       (Y COMPUTATION)
     + 1N + 1A + 1M + 1A
       -> 2A + 1M + 1N
     Total:
       6A + 1AC + 1M + 1MC + 4N + 1DI
  *)
  let add t1 t2 =
    match (t1, t2) with
    | Infinity, t2 -> t2
    | t1, Infinity -> t1
    | t1, t2 when eq t1 t2 -> double t1
    | P (x1, y1), P (x2, y2) ->
        if Fq.(x1 = x2 && y1 = negate y2) then Infinity
        else
          (* slope = (y2 - y1) / (x2 - x1) *)
          let neg_y1 = Fq.(negate y1) in
          let neg_x1 = Fq.(negate x1) in
          let y2_min_y1 = Fq.(y2 + neg_y1) in
          let x2_min_x1 = Fq.(x2 + neg_x1) in
          let l = Fq.(y2_min_y1 / x2_min_x1) in
          (* x computation *)
          let ll = Fq.(square l) in
          let x2_plus_x1 = Fq.(x1 + x2) in
          let b_ll = Fq.(b * ll) in
          let a_plus_x2_plus_x1 = Fq.(a + x2_plus_x1) in
          let neg_a_plus_x2_plus_x1 = Fq.(negate a_plus_x2_plus_x1) in
          let x3 = Fq.(b_ll + neg_a_plus_x2_plus_x1) in
          (* y computation
             Cost improvement by using x3 directly:
             3A + 1AC + 2M + 1MC -> 2A + 1M + 1N
             Saving 1A, 1AC, 1M, 1MC and costs 1 extra N
          *)
          let neg_x3 = Fq.(negate x3) in
          let x1_min_x3 = Fq.(x1 + neg_x3) in
          let l_x1_min_x3 = Fq.(l * x1_min_x3) in
          let y3 = Fq.(l_x1_min_x3 + neg_y1) in
          P (x3, y3)

  let negate p =
    match p with Infinity -> Infinity | P (x, y) -> P (x, Fq.negate y)

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let a, r = Z.ediv_rem n two_z in
        if Z.equal r Z.zero then aux (double x) a else add x (aux x (Z.pred n))
    in
    aux x (Scalar.to_z n)

  let is_on_curve ~x ~y =
    let xx = Fq.square x in
    let yy = Fq.square y in
    Fq.((x * xx) + (a * xx) + x = b * yy)

  let is_in_prime_subgroup ~x ~y =
    let p = P (x, y) in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let of_bytes_opt bytes =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length bytes <> size_in_bytes then None
    else
      let x_bytes = Bytes.sub bytes 0 Fq.size_in_bytes in
      let y_bytes = Bytes.sub bytes Fq.size_in_bytes Fq.size_in_bytes in
      if Bytes.equal buffer_zero bytes then Some Infinity
      else
        let x = Fq.of_bytes_opt x_bytes in
        let y = Fq.of_bytes_opt y_bytes in
        match (x, y) with
        | None, _ | _, None -> None
        (* Verify it is on the curve *)
        | Some x, Some y ->
            if is_on_curve ~x ~y && is_in_prime_subgroup ~x ~y then
              Some (P (x, y))
            else None

  let check_bytes bytes =
    match of_bytes_opt bytes with Some _ -> true | None -> false

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    match of_bytes_opt b with
    | Some g -> g
    | None -> raise (Not_on_curve b)

  let to_bytes g =
    let buffer = Bytes.make size_in_bytes '\000' in
    match g with
    | Infinity -> buffer
    | P (x, y) ->
        Bytes.blit (Fq.to_bytes x) 0 buffer 0 Fq.size_in_bytes ;
        Bytes.blit (Fq.to_bytes y) 0 buffer Fq.size_in_bytes Fq.size_in_bytes ;
        buffer

  let one = of_bytes_exn Params.bytes_generator

  let is_in_prime_subgroup ~x ~y =
    let p = P (x, y) in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let x = Fq.random () in
      let xx = Fq.mul x x in
      let y_square = Fq.(((x * xx) + (a * xx) + x) / b) in
      let y_opt = Fq.sqrt_opt y_square in
      match y_opt with
      | None -> aux ()
      | Some y -> mul (P (x, y)) (Scalar.of_z Params.cofactor)
    in
    aux ()

  let get_x_coordinate t =
    match t with Infinity -> raise (Invalid_argument "Zero") | P (x, _y) -> x

  let get_y_coordinate t =
    match t with Infinity -> raise (Invalid_argument "Zero") | P (_x, y) -> y

  (* https://en.wikipedia.org/wiki/Montgomery_curve *)
  let to_twisted t =
    match t with
    | Infinity -> raise (Invalid_argument "Zero")
    | P (x, y) ->
        if Fq.is_zero y || Fq.(is_zero (one + x)) then None
        else Some Fq.(x / y, (x + negate one) / (x + one))

  let to_twisted_curve_parameters () =
    let gen = to_twisted one in
    if Option.is_none gen then None
    else
      let a = Fq.((Params.a + two) / Params.b) in
      let d = Fq.((Params.a + negate two) / Params.b) in
      Some (a, d, Params.cofactor, Option.get gen)

  (* https://en.wikipedia.org/wiki/Montgomery_curve *)
  let to_weierstrass t =
    match t with
    | Infinity -> raise (Invalid_argument "Zero")
    | P (x, y) ->
        let x = Fq.((x / b) + (a / three_b)) in
        let y = Fq.(y / b) in
        Some (x, y)

  let to_weierstrass_curve_parameters () =
    let gen = to_weierstrass one in
    if Option.is_none gen then None
    else
      let nine = Fq.of_string "9" in
      let twenty_seven = Fq.of_string "27" in
      let a_square = Fq.square Params.a in
      let a_cube = Fq.mul a a_square in
      let b_square = Fq.square Params.b in
      let b_cube = Fq.mul b b_square in
      let d =
        Fq.(((two * a_cube) + (negate nine * a)) / (twenty_seven * b_cube))
      in
      let a = Fq.((three + negate a_square) / (three * b_square)) in
      Some (a, d, Params.cofactor, Option.get gen)

  let from_coordinates_exn ~x ~y =
    if is_on_curve ~x ~y && is_in_prime_subgroup ~x ~y then P (x, y)
    else
      raise
        (Not_on_curve (Bytes.concat Bytes.empty [Fq.to_bytes x; Fq.to_bytes y]))

  let from_coordinates_opt ~x ~y =
    if is_on_curve ~x ~y && is_in_prime_subgroup ~x ~y then Some (P (x, y))
    else None

  let of_compressed_bytes_opt bs =
    (* required to avoid side effect! *)
    let bs = Bytes.copy bs in
    let length = Bytes.length bs in
    if length <> Base.size_in_bytes then None
    else if bs = Bytes.make Base.size_in_bytes '\000' then Some Infinity
    else
      (* We get the last bit of the input, representing the bit of u. We also
         remove the last bit from the bytes we received
      *)
      let last_byte = int_of_char @@ Bytes.get bs (length - 1) in
      let sign = last_byte lsr 7 in
      let last_byte_without_sign = last_byte land 0b01111111 in
      Bytes.set bs (length - 1) (char_of_int last_byte_without_sign) ;
      (* We compute u *)
      let x = Base.of_bytes_opt bs in
      match x with
      | None -> None
      | Some x -> (
          let xx = Base.mul x x in
          let yy = Base.(((x * xx) + (a * xx) + x) / b) in
          let y_opt = Base.sqrt_opt yy in
          let y =
            match y_opt with
            | None -> None
            | Some y ->
                (* computed before for constant time *)
                let negated_y = Base.negate y in
                let y_first_byte = Bytes.get (Base.to_bytes y) 0 in
                let is_sign_flipped =
                  int_of_char y_first_byte lxor sign land 1
                in
                Some (if is_sign_flipped = 0 then y else negated_y)
          in
          match y with
          | Some y ->
              let p = P (x, y) in
              if is_in_prime_subgroup ~x ~y then Some p else None
          | None -> None)

  let of_compressed_bytes_exn b =
    match of_compressed_bytes_opt b with
    | None -> raise (Not_on_curve b)
    | Some p -> p

  let to_compressed_bytes p =
    match p with
    | Infinity -> Bytes.make Base.size_in_bytes '\000'
    | P (x, y) ->
        let x_bytes = Base.to_bytes x in
        let y_bytes = Base.to_bytes y in
        let y_first_byte = int_of_char (Bytes.get y_bytes 0) in
        let x_last_byte =
          int_of_char (Bytes.get x_bytes (Base.size_in_bytes - 1))
        in
        (* Get the first bit of y, i.e. the sign of y *)
        let sign_of_y = y_first_byte land 0b00000001 in
        (* Set the last bit of the last byte of x to the sign of y *)
        let x_last_byte_with_y = x_last_byte lor (sign_of_y lsl 7) in
        Bytes.set
          x_bytes
          (Base.size_in_bytes - 1)
          (char_of_int x_last_byte_with_y) ;
        x_bytes
end

module MakeAffineEdwards
    (Base : Ff_sig.PRIME)
    (Scalar : Ff_sig.PRIME)
    (Params : sig
      val a : Base.t

      val d : Base.t

      val cofactor : Z.t

      val bytes_generator : Bytes.t
    end) :
  Ec_sig.AffineEdwardsT with type Base.t = Base.t and type Scalar.t = Scalar.t =
struct
  (* https://www.hyperelliptic.org/EFD/g1p/auto-twisted.html *)
  (* https://en.wikipedia.org/wiki/Twisted_Edwards_curve *)

  (* Summary for the complexity:
     Addition: 2A + 2AC + 5M + 2MC + 2N + 2DI
     Doubling: 3A + 1AC + 1M + 1MC + 2N + 2DI + 1DO + 2SQ
  *)
  exception Not_on_curve of Bytes.t

  module Scalar = Scalar
  module Base = Base
  include Params

  let () =
    (* Addition formula is complete if d is a not a square *)
    assert (Option.is_none (Base.sqrt_opt d))

  let size_in_bytes = Base.size_in_bytes * 2

  type t = {u : Base.t; v : Base.t}

  let zero = {u = Base.zero; v = Base.one}

  let is_zero {u; v} = Base.(u = zero) && Base.(v = one)

  let to_bytes {u; v} =
    Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]

  (* Complexity (update above summary if any change):
     2A + 2AC + 5M + 2MC + 2N + 2DI
  *)
  let add {u = u1; v = v1} {u = u2; v = v2} =
    let u1v2 = Base.(u1 * v2) in
    let v1u2 = Base.(v1 * u2) in
    let u1u2v1v2 = Base.(u1v2 * v1u2) in
    let v1v2 = Base.(v1 * v2) in
    let u1u2 = Base.(u1 * u2) in
    let du1u2v1v2 = Base.(d * u1u2v1v2) in
    let u = Base.((u1v2 + v1u2) / (Base.one + du1u2v1v2)) in
    let v =
      Base.(
        (v1v2 + Base.negate (a * u1u2)) / (Base.one + Base.negate du1u2v1v2))
    in
    {u; v}

  (* used in doubling *)
  let two = Base.(one + one)

  (* Complexity (update above summary if any change)
     3A + 1AC + 1M + 1MC + 2N + 2DI + 1DO + 2SQ
  *)
  let double {u; v} =
    let uv = Base.(u * v) in
    let uu = Base.square u in
    let vv = Base.square v in
    let neg_uu = Base.negate uu in
    let neg_vv = Base.negate vv in
    let a_uu = Base.(a * uu) in
    let a_neguu = Base.(a * neg_uu) in
    (* a u^2 v^2 = 1 + d u^2 v^2 --> we can skip one multiplication *)
    let u' = Base.(double uv / (a_uu + vv)) in
    let v' = Base.((vv + a_neguu) / (two + a_neguu + neg_vv)) in
    {u = u'; v = v'}

  let negate {u; v} = {u = Base.negate u; v}

  let eq {u = u1; v = v1} {u = u2; v = v2} = Base.(u1 = u2 && v1 = v2)

  let mul x n =
    let rec aux x n =
      let two_z = Z.succ Z.one in
      if Z.equal n Z.zero then zero
      else if Z.equal n Z.one then x
      else
        let q, r = Z.ediv_rem n two_z in
        let x_plus_x = double x in
        if Z.equal r Z.zero then aux x_plus_x q else add x (aux x_plus_x q)
    in
    aux x (Scalar.to_z n)

  let is_on_curve ~u ~v =
    (* a * u^2 + v^2 = 1 + d u^2 v^2 *)
    let uu = Base.square u in
    let vv = Base.square v in
    let uuvv = Base.(uu * vv) in
    Base.((a * uu) + vv = one + (d * uuvv))

  let is_in_prime_subgroup ~u ~v =
    let p = {u; v} in
    if is_zero p then true else not (is_zero (mul p (Scalar.of_z cofactor)))

  let of_bytes_opt b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used and [Bytes.sub]
       creates a new buffer *)
    if Bytes.length b <> size_in_bytes then None
    else
      let u_opt = Base.of_bytes_opt (Bytes.sub b 0 Base.size_in_bytes) in
      let v_opt =
        Base.of_bytes_opt (Bytes.sub b Base.size_in_bytes Base.size_in_bytes)
      in
      match (u_opt, v_opt) with
      | Some u, Some v ->
          if is_on_curve ~u ~v && is_in_prime_subgroup ~u ~v then Some {u; v}
          else None
      | _ -> None

  let of_bytes_exn b =
    (* no need to copy the bytes [p] because [Bytes.sub] is used in
       [of_bytes_opt] and [Bytes.sub] creates a new buffer *)
    match of_bytes_opt b with
    | None -> raise (Not_on_curve b)
    | Some p -> p

  let check_bytes b = match of_bytes_opt b with None -> false | Some _ -> true

  let one = of_bytes_exn bytes_generator

  let rec random ?state () =
    let () = match state with Some s -> Random.set_state s | None -> () in
    let u = Base.random ?state:None () in
    let uu = Base.(square u) in
    let auu = Base.(a * uu) in
    let duu = Base.(d * uu) in
    if Base.(is_one duu) then random ?state:None ()
    else
      (*      a u^2 + v^2 = 1 + d u^2 v^2 *)
      (* <==> a u^2 + v^2 - d u^2 v^2 = 1 *)
      (* <==> v^2 - d u^2 v^2 = 1 - a u^2 *)
      (* <==> v^2 * (1 - d u^2) = 1 - a u^2 *)
      (* <==> v^2 = (1 - a * u^2) / (1 - d * u^2) *)
      let tmp = Base.((one + negate auu) / (one + negate duu)) in
      let v_sqrt = Base.(sqrt_opt tmp) in
      match v_sqrt with
      | None -> random ?state:None ()
      | Some v ->
          let p = mul {u; v} (Scalar.of_z cofactor) in
          if eq p zero then random ?state:None () else p

  let get_u_coordinate p = p.u

  let get_v_coordinate p = p.v

  (* https://en.wikipedia.org/wiki/Montgomery_curve *)
  let to_montgomery p =
    match (p.u, p.v) with
    | u, v when Base.(is_zero u && is_one v) -> raise (Invalid_argument "Zero")
    | u, v ->
        assert (not Base.(eq a d)) ;
        if Base.is_zero u || Base.(is_zero (one + v)) then None
        else
          let one_plus_v = Base.(one + v) in
          let one_minus_v = Base.(one + negate v) in
          let x = Base.(one_plus_v / one_minus_v) in
          let y = Base.(x / u) in
          Some (x, y)

  let to_montgomery_curve_parameters () =
    let gen = to_montgomery one in
    if Option.is_none gen then None
    else
      let gen = Option.get gen in
      let two = Base.of_string "2" in
      let four = Base.of_string "4" in
      let a_min_d = Base.(a + negate d) in
      let b = Base.(four / a_min_d) in
      let a = Base.(two * (a + d) / a_min_d) in
      Some (a, b, Params.cofactor, gen)

  let from_coordinates_opt ~u ~v =
    let p = {u; v} in
    if is_on_curve ~u ~v && is_in_prime_subgroup ~u ~v then Some p else None

  let from_coordinates_exn ~u ~v =
    match from_coordinates_opt ~u ~v with
    | None ->
        raise
          (Not_on_curve
             (Bytes.concat Bytes.empty [Base.to_bytes u; Base.to_bytes v]))
    | Some p -> p

  let unsafe_from_coordinates ~u ~v = {u; v}
end

let from_affine_weierstrass_to_jacobian_weierstrass
    (type affine jacobian base scalar)
    (module Affine : Ec_sig.AffineWeierstrassT
      with type t = affine
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Jacobian : Ec_sig.JacobianWeierstrassT
      with type t = jacobian
       and type Base.t = base
       and type Scalar.t = scalar) (p_affine : affine) : jacobian =
  if Affine.is_zero p_affine then Jacobian.zero
  else
    let x = Affine.get_x_coordinate p_affine in
    let y = Affine.get_y_coordinate p_affine in
    Jacobian.from_affine_coordinates_exn ~x ~y

let from_jacobian_weierstrass_to_affine_weierstrass
    (type affine jacobian base scalar)
    (module Jacobian : Ec_sig.JacobianWeierstrassT
      with type t = jacobian
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine : Ec_sig.AffineWeierstrassT
      with type t = affine
       and type Base.t = base
       and type Scalar.t = scalar) (p_jacobian : jacobian) : affine =
  if Jacobian.is_zero p_jacobian then Affine.zero
  else
    let x = Jacobian.get_x_coordinate p_jacobian in
    let y = Jacobian.get_y_coordinate p_jacobian in
    let z = Jacobian.get_z_coordinate p_jacobian in
    let zz = Jacobian.Base.square z in
    let zzz = Jacobian.Base.(z * zz) in
    let x' = Jacobian.Base.(x / zz) in
    let y' = Jacobian.Base.(y / zzz) in
    Affine.from_coordinates_exn ~x:x' ~y:y'

let from_affine_weierstrass_to_projective_weierstrass
    (type affine projective base scalar)
    (module Affine : Ec_sig.AffineWeierstrassT
      with type t = affine
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Projective : Ec_sig.ProjectiveWeierstrassT
      with type t = projective
       and type Base.t = base
       and type Scalar.t = scalar) (p_affine : affine) : projective =
  if Affine.is_zero p_affine then Projective.zero
  else
    let x = Affine.get_x_coordinate p_affine in
    let y = Affine.get_y_coordinate p_affine in
    Projective.from_affine_coordinates_exn ~x ~y

let from_projective_weierstrass_to_affine_weierstrass
    (type affine projective base scalar)
    (module Projective : Ec_sig.ProjectiveWeierstrassT
      with type t = projective
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine : Ec_sig.AffineWeierstrassT
      with type t = affine
       and type Base.t = base
       and type Scalar.t = scalar) (p_projective : projective) : affine =
  if Projective.is_zero p_projective then Affine.zero
  else
    let x = Projective.get_x_coordinate p_projective in
    let y = Projective.get_y_coordinate p_projective in
    let z = Projective.get_z_coordinate p_projective in
    let x' = Projective.Base.(x / z) in
    let y' = Projective.Base.(y / z) in
    Affine.from_coordinates_exn ~x:x' ~y:y'

let from_affine_montgomery_to_affine_weierstrass
    (type affine_mt affine_wt base scalar)
    (module Affine_mt : Ec_sig.AffineMontgomeryT
      with type t = affine_mt
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine_wt : Ec_sig.AffineWeierstrassT
      with type t = affine_wt
       and type Base.t = base
       and type Scalar.t = scalar) (p_mt : affine_mt) : affine_wt option =
  let coords_opt = Affine_mt.to_weierstrass p_mt in
  Option.bind coords_opt (fun (x, y) -> Affine_wt.from_coordinates_opt ~x ~y)

(* let from_weierstrass_montgomery (type affine_wt affine_mt base scalar)
    (module Affine_wt : Ec_sig.AffineWeierstrassT
      with type t = affine_wt
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine_mt : Ec_sig.AffineMontgomeryT
      with type t = affine_mt
       and type Base.t = base
       and type Scalar.t = scalar) (p_wt : affine_wt) : affine_mt option =
       let coords_opt = Affine_wt.to_montgomery p_wt in
       Option.bind coords_opt (fun (x, y) -> Affine_mt.from_coordinates_opt ~x ~y) *)

let from_affine_montgomery_to_affine_edwards
    (type affine_mt affine_tw base scalar)
    (module Affine_mt : Ec_sig.AffineMontgomeryT
      with type t = affine_mt
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine_tw : Ec_sig.AffineEdwardsT
      with type t = affine_tw
       and type Base.t = base
       and type Scalar.t = scalar) (p_mt : affine_mt) : affine_tw option =
  let coords_opt = Affine_mt.to_twisted p_mt in
  Option.bind coords_opt (fun (u, v) -> Affine_tw.from_coordinates_opt ~u ~v)

let from_affine_edwards_to_affine_montgomery
    (type affine_tw affine_mt base scalar)
    (module Affine_tw : Ec_sig.AffineEdwardsT
      with type t = affine_tw
       and type Base.t = base
       and type Scalar.t = scalar)
    (module Affine_mt : Ec_sig.AffineMontgomeryT
      with type t = affine_mt
       and type Base.t = base
       and type Scalar.t = scalar) (p_tw : affine_tw) : affine_mt option =
  let coords_opt = Affine_tw.to_montgomery p_tw in
  Option.bind coords_opt (fun (x, y) -> Affine_mt.from_coordinates_opt ~x ~y)

module MakeAffineEdwardsToAffineMontgomery (E : Ec_sig.AffineEdwardsT) :
  Ec_sig.AffineMontgomeryT
    with module Base = E.Base
     and module Scalar = E.Scalar =
  MakeAffineMontgomery (E.Base) (E.Scalar)
    (struct
      let two = E.Base.(double one)

      let four = E.Base.(double two)

      let a_neg_d = E.Base.(E.a + negate E.d)

      let a = E.Base.(two * (E.a + E.d) / a_neg_d)

      let b = E.Base.(four / a_neg_d)

      let cofactor = E.cofactor

      let bytes_generator =
        let u = E.get_u_coordinate E.one in
        let v = E.get_v_coordinate E.one in
        let one_plus_v = E.Base.(one + v) in
        let one_minus_v = E.Base.(one + negate v) in
        let x = E.Base.(one_plus_v / one_minus_v) in
        let y = E.Base.(x / u) in
        Bytes.concat Bytes.empty [E.Base.to_bytes x; E.Base.to_bytes y]
    end)
