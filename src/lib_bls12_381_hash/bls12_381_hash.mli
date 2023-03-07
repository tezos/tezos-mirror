module type PERMUTATION = S.PERMUTATION

module Permutation : sig
  (** Implementation of
    {{:https://eprint.iacr.org/2019/458.pdf} Poseidon} over the scalar field of
    BLS12-381 for a security with the permutation [x^5].
*)
  module Poseidon : sig
    module Parameters : sig
      type t = {
        state_size : int;
        nb_of_partial_rounds : int;
        nb_of_full_rounds : int;
        batch_size : int;
        round_constants : Bls12_381.Fr.t array;
        linear_layer : Bls12_381.Fr.t array array;
      }

      (** Parameters for Poseidon with a state size of [3] for a security of
        128bits.
        FIXME: The linear layer and the round constants are not standard
      *)
      val security_128_state_size_3 : t

      (** Parameters for Poseidon with a state size of [5] for a security of
        256bits.
        FIXME: The linear layer and the round constants are not standard
    *)
      val security_256_state_size_5 : t
    end

    include S.PERMUTATION with type parameters = Parameters.t
  end

  (** Implementation of an instantiation of {{: https://eprint.iacr.org/2019/426 }
    Rescue } over the scalar field of BLS12-381 for a security of 128 bits and
    with the permutation [x^5]. The parameters of the instantiation are:
    - state size = 3
    - number of rounds = 14

    These parameters have been generated using {{:
    https://github.com/KULeuven-COSIC/Marvellous/blob/0969ce8a5ebaa0bf45696b44e276d3dd81d2e455/rescue_prime.sage}
    this script}.
*)
  module Rescue : sig
    (** Set of parameters for BLS12-381, and parameters for specific
      instantiations given in the reference paper *)
    module Parameters : sig
      type t = {
        linear_layer : Bls12_381.Fr.t array array;
        round_constants : Bls12_381.Fr.t array;
        state_size : int;
        nb_of_rounds : int;
      }

      (** Parameters for Rescue with [state_size = 3] and 128 bits of security.
        FIXME: The linear layer and the round constants are not standard
    *)
      val security_128_state_size_3 : t
    end

    include S.PERMUTATION with type parameters = Parameters.t
  end

  (** Implementation of {{: https://eprint.iacr.org/2022/840}
    the permutation Anemoi and the mode of operation Jive} over the scalar field
    of BLS12-381.

    The state of the permutation Anemoi is [m], where [m] is a multiple of [2].
    It is commonly refered by [l] such that [m = 2l].
*)
  module Anemoi : sig
    (** Set of parameters for BLS12-381, and parameters for specific
     instantiations given in the reference paper *)
    module Parameters : sig
      (** The type representing the set of parameters for a given instance *)
      type t

      (** [create security state_size linear_layer] creates a
        value of type {!t}. If the [state_size] is [2], [4], [6] or [8], an
        exception is raised. The library enforces the user to use the default
        security parameters and an optimised implementation is provided in these
        cases. Also, an exception is raised if the state size is not a multiple
        of [2].

        @deprecated It is highly recommended to follow the recommandation in the
        paper for the choice of security parameters. Please open an issue if you
        need support for other instances than the default parameters provided by
        the library.
    *)
      val create : int -> int -> Bls12_381.Fr.t array array -> t
        [@@deprecated
          "It is highly recommended to follow the recommandation in the paper \
           for the choice of security parameters. Please open an issue if you \
           need support for other instances than the default parameters \
           provided by the library."]

      (** Exponent for the substitution box. For BLS12-381, it is [5] *)
      val alpha : Bls12_381.Fr.t

      (** Inverse of the exponent for the substitution box. For BLS12-381, it is
        [20974350070050476191779096203274386335076221000211055129041463479975432473805] *)
      val alpha_inv : Bls12_381.Fr.t

      (** For BLS12-381, it is
        [14981678621464625851270783002338847382197300714436467949315331057125308909861]
    *)
      val delta : Bls12_381.Fr.t

      (** First generator of the scalar field of BLS12-381, i.e. [7] *)
      val g : Bls12_381.Fr.t

      (** Same than {!g} *)
      val beta : Bls12_381.Fr.t

      (** Set to [0] for BLS12-381 *)
      val gamma : Bls12_381.Fr.t

      (** [compute_number_of_rounds state_size security] computes the minimal
        number of rounds for an instance of Anemoi with a state size of
        [m = state_size] to reach a security level of [security] bits. The
        computation follows the formula given in section 5.2 *)
      val compute_number_of_rounds : int -> int -> int

      (** [generate_constants nb_rounds l] generates the constants for the
        instance of Anemoi for a state size of [m = 2 * l]. The output contains
        the C's followed by the D's as described in the paper in section 5.1 *)
      val generate_constants : int -> int -> Bls12_381.Fr.t array

      val get_round_constants : t -> Bls12_381.Fr.t array

      val get_matrix : t -> Bls12_381.Fr.t array array

      val get_number_of_rounds : t -> int

      val get_state_size : t -> int

      (** Parameters for the permutation Anemoi for a state size of [m = 2] (i.e.
        [l = 1]) and 128 bits of security given in the paper
    *)
      val security_128_state_size_2 : t

      (** Parameters for the permutation Anemoi for a state size of [m = 2] (i.e.
        [l = 1]) and 141 bits of security
    *)
      val security_141_state_size_2 : t

      (** Parameters for the permutation Anemoi for a state size of [m = 4] (i.e.
        [l = 2]) and 128 bits of security given in the paper
    *)
      val security_128_state_size_4 : t

      (** Parameters for the permutation Anemoi for a state size of [m = 6] (i.e.
        [l = 3]) and 128 bits of security given in the paper
    *)
      val security_128_state_size_6 : t

      (** Parameters for the permutation Anemoi for a state size of [m = 8] (i.e.
        [l = 4]) and 128 bits of security given in the paper
    *)
      val security_128_state_size_8 : t
    end

    include S.PERMUTATION with type parameters = Parameters.t

    (** [apply_linear_layer ctxt] applies the linear layer on the state. The
      context is modified *)
    val apply_linear_layer : ctxt -> unit

    (** [apply_flystel ctxt] applies the Flystel construction on the context.
      The context is modified *)
    val apply_flystel : ctxt -> unit

    (** [apply_constants_addition ctxt round] applies the constant addition for the
      round [round]. The context is modified *)
    val apply_constants_addition : ctxt -> int -> unit

    (** [apply_one_round ctxt round] applies the round [round] on the state. The
      context is modified *)
    val apply_one_round : ctxt -> int -> unit

    (** [jive128_1 x y] calls the permutation Anemoi for [l = 1] with the state
      [S = (x, y)] and apply Jive on the output. Expected security is 128
      bits *)
    val jive128_1 : Bls12_381.Fr.t -> Bls12_381.Fr.t -> Bls12_381.Fr.t

    (** [jive141_1 x y] calls the permutation Anemoi for [l = 1] with the state
      [S = (x, y)] and apply Jive on the output. Expected security is 141
      bits *)
    val jive141_1 : Bls12_381.Fr.t -> Bls12_381.Fr.t -> Bls12_381.Fr.t
  end

  (** {{: https://eprint.iacr.org/2022/403.pdf } Griffin } over the scalar field
    of BLS12-381 for a security of 128 bits and with the permutation [x^5].
*)
  module Griffin : sig
    module Parameters : sig
      type t = {
        nb_of_rounds : int;
        state_size : int;
        round_constants : Bls12_381.Fr.t array;
        alpha_beta_s : Bls12_381.Fr.t array;
      }

      (** Exponent for the substitution box. For BLS12-381, it is [5] *)
      val d : Bls12_381.Fr.t

      (** Inverse of the exponent for the substitution box. For BLS12-381, it is
        [20974350070050476191779096203274386335076221000211055129041463479975432473805] *)
      val d_inv : Bls12_381.Fr.t

      (** Parameters for Griffin with a state size of [3] for a security of
        128bits
    *)
      val security_128_state_size_3 : t

      (** Parameters for Griffin with a state size of [4] for a security of 128
        bits
    *)
      val security_128_state_size_4 : t
    end

    include S.PERMUTATION with type parameters = Parameters.t
  end
end

module Mode : sig
  module Jive : sig
    include S.MODE

    val digest_b :
      (module S.PERMUTATION with type parameters = 'p) ->
      'p ->
      Bls12_381.Fr.t array ->
      int ->
      Bls12_381.Fr.t array
  end
end
