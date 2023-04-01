module Stubs = struct
  type ctxt

  external get_state : Bls12_381.Fr.t array -> ctxt -> unit
    = "caml_bls12_381_hash_anemoi_get_state_stubs"

  external get_state_size : ctxt -> int
    = "caml_bls12_381_hash_anemoi_get_state_size_stubs"

  external set_state : ctxt -> Bls12_381.Fr.t array -> unit
    = "caml_bls12_381_hash_anemoi_set_state_stubs"

  external anemoi_apply_flystel : ctxt -> unit
    = "caml_bls12_381_hash_anemoi_apply_flystel_stubs"

  external anemoi_apply_linear_layer : ctxt -> unit
    = "caml_bls12_381_hash_anemoi_apply_linear_layer_stubs"

  external anemoi_apply_constants_addition : ctxt -> int -> unit
    = "caml_bls12_381_hash_anemoi_apply_constants_addition_stubs"

  external anemoi_apply_one_round : ctxt -> int -> unit
    = "caml_bls12_381_hash_anemoi_apply_one_round_stubs"

  external anemoi_apply_permutation : ctxt -> unit
    = "caml_bls12_381_hash_anemoi_apply_permutation_stubs"

  external anemoi_allocate_ctxt :
    mds:Bls12_381.Fr.t array array ->
    constants:Bls12_381.Fr.t array ->
    beta:Bls12_381.Fr.t ->
    delta:Bls12_381.Fr.t ->
    int ->
    int ->
    ctxt
    = "caml_bls12_381_hash_anemoi_allocate_ctxt_stubs_bytecode" "caml_bls12_381_hash_anemoi_allocate_ctxt_stubs"
end

module Parameters = struct
  type t = {
    security : int;
    state_size : int;
    nb_rounds : int;
    linear_layer : Bls12_381.Fr.t array array;
    round_constants : Bls12_381.Fr.t array;
  }

  let g = Bls12_381.Fr.of_string "7"

  let beta = Bls12_381.Fr.of_string "7"

  let delta =
    Bls12_381.Fr.of_string
      "14981678621464625851270783002338847382197300714436467949315331057125308909861"

  let alpha = Bls12_381.Fr.of_string "5"

  let alpha_inv =
    Bls12_381.Fr.of_string
      "20974350070050476191779096203274386335076221000211055129041463479975432473805"

  let gamma = Bls12_381.Fr.zero

  let pi_0 =
    Bls12_381.Fr.of_string
      "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

  let pi_1 =
    Bls12_381.Fr.of_string
      "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"

  let generate_constants nb_rounds l =
    let csts_c =
      Array.init (l * nb_rounds) (fun _ -> Bls12_381.Fr.(copy zero))
    in
    let csts_d =
      Array.init (l * nb_rounds) (fun _ -> Bls12_381.Fr.(copy zero))
    in
    for i = 0 to nb_rounds - 1 do
      let pi_0_i = Bls12_381.Fr.pow pi_0 (Z.of_int i) in
      let pi_0_i_square = Bls12_381.Fr.(square pi_0_i) in
      for j = 0 to l - 1 do
        let pi_1_j = Bls12_381.Fr.pow pi_1 (Z.of_int j) in
        let pi_1_j_square = Bls12_381.Fr.square pi_1_j in
        csts_c.((i * l) + j) <-
          Bls12_381.Fr.(
            (g * pi_0_i_square)
            + pow (pi_0_i + pi_1_j) (Bls12_381.Fr.to_z alpha)) ;
        csts_d.((i * l) + j) <-
          Bls12_381.Fr.(
            (g * pi_1_j_square)
            + pow (pi_0_i + pi_1_j) (Bls12_381.Fr.to_z alpha)
            + delta)
      done
    done ;
    Array.concat [csts_c; csts_d]

  let compute_number_of_rounds state_size security =
    let l = state_size / 2 in
    let security_margin = 1 + l in
    let alpha = 5 in
    let pow_security_2 = Z.pow (Z.of_int 2) security in
    let rec aux r =
      let num = (2 * l * r) + alpha + 1 + (2 * ((l * r) - 2)) in
      let den = 2 * l * r in
      let bin = Z.bin (Z.of_int num) den in
      let bin_square = Z.mul bin bin in
      if Z.gt bin_square pow_security_2 then r else aux (r + 1)
    in
    max 10 (security_margin + aux 0)

  let get_number_of_rounds p = p.nb_rounds

  let get_round_constants p = p.round_constants

  let get_matrix p = p.linear_layer

  let get_state_size p = p.state_size

  let create security state_size linear_layer =
    if state_size mod 2 = 1 then failwith "State size must be a multiple of 2" ;
    if state_size = 2 || state_size = 4 || state_size = 6 || state_size = 8 then
      failwith
        "Use the value given above. The library enforces users to use the \
         linear layer recommended in the paper" ;
    let nb_rounds = compute_number_of_rounds state_size security in
    let round_constants = generate_constants nb_rounds (state_size / 2) in
    {security; state_size; nb_rounds; round_constants; linear_layer}

  let security_128_state_size_2 =
    {
      security = 128;
      nb_rounds = 19;
      state_size = 2;
      linear_layer = Bls12_381.Fr.[|[|one; g|]; [|g; square g + one|]|];
      round_constants = generate_constants 19 1;
    }

  let security_141_state_size_2 =
    {
      security = 141;
      nb_rounds = 20;
      state_size = 2;
      linear_layer = Bls12_381.Fr.[|[|one; g|]; [|g; square g + one|]|];
      round_constants = generate_constants 20 1;
    }

  let security_128_state_size_4 =
    {
      security = 128;
      nb_rounds = 12;
      state_size = 4;
      linear_layer = Bls12_381.Fr.[|[|one; g|]; [|g; square g + one|]|];
      round_constants = generate_constants 12 2;
    }

  let security_128_state_size_6 =
    {
      security = 128;
      nb_rounds = 10;
      state_size = 6;
      linear_layer =
        Bls12_381.Fr.
          [|[|g + one; one; g + one|]; [|one; one; g|]; [|g; one; one|]|];
      round_constants = generate_constants 10 3;
    }

  let security_128_state_size_8 =
    {
      security = 128;
      nb_rounds = 10;
      state_size = 8;
      round_constants = generate_constants 10 4;
      linear_layer =
        Bls12_381.Fr.
          [|
            [|one; one + g; g; g|];
            [|square g; g + square g; one + g; one + double g|];
            [|square g; square g; one; one + g|];
            [|one + g; one + double g; g; one + g|];
          |];
    }
end

type parameters = Parameters.t

type ctxt = Stubs.ctxt

let allocate_ctxt parameters =
  let state_size = parameters.Parameters.state_size in
  if state_size mod 2 = 1 then
    failwith "The state size given in parameters must be a multiple of 2" ;
  let l = state_size / 2 in
  if state_size <= 0 then failwith "State size must be at least 2" ;
  let mds = parameters.Parameters.linear_layer in
  let constants = parameters.Parameters.round_constants in
  let beta = Parameters.beta in
  let delta = Parameters.delta in
  let ctxt =
    Stubs.anemoi_allocate_ctxt
      ~mds
      ~constants
      ~beta
      ~delta
      l
      parameters.nb_rounds
  in
  ctxt

let set_state ctxt state =
  let exp_state_size = Stubs.get_state_size ctxt in
  let state_size = Array.length state in
  if state_size <> exp_state_size then
    failwith
      (Printf.sprintf
         "The given array contains %d elements but the expected state size is \
          %d"
         state_size
         exp_state_size) ;
  Stubs.set_state ctxt state

let get_state_size ctxt = Stubs.get_state_size ctxt

let get_state ctxt =
  let state_size = Stubs.get_state_size ctxt in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.(copy zero)) in
  Stubs.get_state state ctxt ;
  state

let apply_constants_addition ctxt round =
  Stubs.anemoi_apply_constants_addition ctxt round

let apply_linear_layer ctxt = Stubs.anemoi_apply_linear_layer ctxt

let apply_one_round ctxt idx = Stubs.anemoi_apply_one_round ctxt idx

let apply_flystel ctxt = Stubs.anemoi_apply_flystel ctxt

let apply_permutation ctxt = Stubs.anemoi_apply_permutation ctxt

let jive128_1 x y =
  let state = [|x; y|] in
  let ctxt = allocate_ctxt Parameters.security_128_state_size_2 in
  let () = set_state ctxt state in
  let () = apply_permutation ctxt in
  let state = get_state ctxt in
  Bls12_381.Fr.(x + y + state.(0) + state.(1))

let jive141_1 x y =
  let state = [|x; y|] in
  let ctxt = allocate_ctxt Parameters.security_141_state_size_2 in
  let () = set_state ctxt state in
  let () = apply_permutation ctxt in
  let state = get_state ctxt in
  Bls12_381.Fr.(x + y + state.(0) + state.(1))
