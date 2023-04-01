(** Follow {{:https://tools.ietf.org/pdf/draft-irtf-cfrg-bls-signature-04.pdf}
    the BLS signature draft of CFRG, version 4} *)

(** Type of the secret keys. *)
type sk

(** The size of a serialized value [sk] *)
val sk_size_in_bytes : int

(** [sk_of_bytes_exn bs] attempts to deserialize [bs] into a secret key. [bs]
    must be the little endian representation of the secret key. In this case,
    secret keys are scalars of BLS12-381 and are encoded on 32 bytes. The
    bytes sequence might be less of 32 bytes and in this case, the bytes
    sequence is padded on the right by 0's.

    @raise Invalid_argument if the bytes sequence is longer than 32 bytes *)
val sk_of_bytes_exn : Bytes.t -> sk

(** [sk_of_bytes_opt bs] is the same than {!sk_of_bytes_exn} but returns an
    option instead of an exception. *)
val sk_of_bytes_opt : Bytes.t -> sk option

(** [sk_to_bytes sk] serialises the secret key into the little endian
    representation. *)
val sk_to_bytes : sk -> Bytes.t

(** [generate_sk ?key_info ikm] generates a new (random) secret key. [ikm]
    must be at least 32 bytes (otherwise, raise [Invalid_argument]). The
    default value of [key_info] is the empty bytes sequence. *)
val generate_sk : ?key_info:Bytes.t -> Bytes.t -> sk

(** BLS signatures instantiation minimizing the size of the public keys (48
    bytes) but use longer signatures (96 bytes). *)
module MinPk : sig
  (** Type of the public keys *)
  type pk

  (** The size of a serialized value [pk] *)
  val pk_size_in_bytes : int

  (** The size of a serialized value [signature] *)
  val signature_size_in_bytes : int

  (** Build a value of type {!pk} without performing any check on the input
      (hence the unsafe prefix because it might not give a correct
      inhabitant of the type [pk]).
      It is safe to use this function when verifying a signature as the
      signature function verifies if the point is in the prime subgroup. Using
      {!unsafe_pk_of_bytes} removes a verification performed twice when used
      {!pk_of_bytes_exn} or {!pk_of_bytes_opt}.

      The expected bytes format are the compressed form of a point on G1. *)
  val unsafe_pk_of_bytes : Bytes.t -> pk

  (** Build a value of type [pk] safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime
      subgroup. Raise [Invalid_argument] if the bytes are not in the correct
      format or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1. *)
  val pk_of_bytes_exn : Bytes.t -> pk

  (** Build a value of type {!pk} safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime
      subgroup. Return [None] if the bytes are not in the correct format or
      does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1. *)
  val pk_of_bytes_opt : Bytes.t -> pk option

  (** Returns a bytes representation of a value of type {!pk}. The output is
      the compressed form of the point [Bls12_381.G1.t] the [pk] represents. *)
  val pk_to_bytes : pk -> Bytes.t

  (** [derive_pk sk] derives the corresponding public key of [sk]. *)
  val derive_pk : sk -> pk

  (** Type of the signatures *)
  type signature

  (** Build a value of type {!signature} without performing any check on the
      input (hence the unsafe prefix because it might not give a correct
      inhabitant of the type [signature]).
      It is safe to use this function when verifying a signature as the
      signature function verifies if the point is in the prime subgroup. Using
      {!unsafe_signature_of_bytes} removes a verification performed twice when
      used {!signature_of_bytes_exn} or {!signature_of_bytes_opt}.

      The expected bytes format are the compressed form of a point on G2. *)
  val unsafe_signature_of_bytes : Bytes.t -> signature

  (** Build a value of type {!signature} safely, i.e. the function checks the
      bytes given in parameters represents a point on the curve and in the
      prime subgroup. Raise [Invalid_argument] if the bytes are not in the
      correct format or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G2. *)
  val signature_of_bytes_exn : Bytes.t -> signature

  (** Build a value of type {!signature} safely, i.e. the function checks the
      bytes given in parameters represents a point on the curve and in the
      prime subgroup. Return [None] if the bytes are not in the correct format
      or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G2. *)
  val signature_of_bytes_opt : Bytes.t -> signature option

  (** Returns a bytes representation of a value of type [signature]. The
      output is the compressed form of a point {!Bls12_381.G2.t} the signature
      represents. *)
  val signature_to_bytes : signature -> Bytes.t

  (** [aggregate_signature_opt signatures] aggregates the signatures
      [signatures], following {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.8
      } section 2.8}.
      Return [None] if [INVALID] is expected in the specification *)
  val aggregate_signature_opt : signature list -> signature option

  (** Basic scheme described in
      {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1}
      section 3.1}

      In a basic scheme, rogue key attacks are handled by requiring all
      messages signed by an aggregate signature to be distinct. This
      requirement is enforced in the definition of AggregateVerify.

      {!Basic.sign} and {!Basic.verify} implements the algorithms {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.6
      } CoreSign} and {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.7}
      CoreVerify}, respectively. *)
  module Basic : sig
    val sign : sk -> Bytes.t -> signature

    val verify : pk -> Bytes.t -> signature -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification.
        It implements the AggregateVerify algorithm specified in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1.1
        } section 3.1.1 }. Raise [Invalid_argument] if the messages are not
        distinct. *)
    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (** Augmentation scheme described in
      {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2}
      section 3.2}

      In a message augmentation scheme, signatures are generated over the
      concatenation of the public key and the message, ensuring that messages
      signed by different public keys are distinct. *)
  module Aug : sig
    (** [sign sk msg] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.1
        } section 3.2.1 } *)
    val sign : sk -> Bytes.t -> signature

    (** [verify pk msg signature] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.2
        } section 3.2.2 } *)
    val verify : pk -> Bytes.t -> signature -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification.
        It implements the AggregateVerify algorithm specified in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.3
        } section 3.2.3 }*)
    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (** Proof of possession scheme described in
      {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3}
      section 3.3}

      A proof of possession scheme uses a separate public key validation step,
      called a proof of possession, to defend against rogue key attacks. This
      enables an optimization to aggregate signature verification for the case
      that all signatures are on the same message. *)
  module Pop : sig
    type proof = Bytes.t

    (** Equivalent to [core_sign] with the DST given in the specification
        {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3}
        in section 4.2.3}. *)
    val sign : sk -> Bytes.t -> signature

    (** Equivalent to [core_verify] with the DST given in the specification
        {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3}
        in section 4.2.3}. *)
    val verify : pk -> Bytes.t -> signature -> bool

    (** [pop_proof sk] implements
        {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.2}
        section 3.3.2}. *)
    val pop_prove : sk -> proof

    (** [pop_verify pk signature] implements
        {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.3}
        section 3.3.3}. *)
    val pop_verify : pk -> proof -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification. It supposes the same message [msg] has been
        signed. It implements the FastAggregateVerify algorithm specified in
        {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.4}
        section 3.3.4}. *)
    val aggregate_verify : (pk * proof) list -> Bytes.t -> signature -> bool
  end
end

(** BLS signatures instantiation minimizing the size of the signatures (48
    bytes) but use longer public keys (96 bytes). *)
module MinSig : sig
  (** Type of the public keys *)
  type pk

  (** The size of a serialized value [pk] *)
  val pk_size_in_bytes : int

  (** Build a value of type [pk] without performing any check on the input
      (hence the unsafe prefix because it might not give a correct inhabitant
      of the type [pk]).
      It is safe to use this function when verifying a signature as the
      signature function verifies if the point is in the prime subgroup. Using
      {!unsafe_pk_of_bytes} removes a verification performed twice when used
      {!pk_of_bytes_exn} or {!pk_of_bytes_opt}.

      The expected bytes format are the compressed form of a point on G2. *)
  val unsafe_pk_of_bytes : Bytes.t -> pk

  (** Build a value of type [pk] safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime
      subgroup. Raise [Invalid_argument] if the bytes are not in the correct
      format or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G2. *)
  val pk_of_bytes_exn : Bytes.t -> pk

  (** Build a value of type [pk] safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime
      subgroup. Return [None] if the bytes are not in the correct format or
      does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G2. *)
  val pk_of_bytes_opt : Bytes.t -> pk option

  (** Returns a bytes representation of a value of type [pk]. The output is
      the compressed form of the point [Bls12_381.Bls12_381.G2.t] the [pk] represents. *)
  val pk_to_bytes : pk -> Bytes.t

  (** [derive_pk sk] derives the corresponding public key of [sk]. *)
  val derive_pk : sk -> pk

  (** Type of the signatures *)
  type signature

  (** The size of a serialized value [signature] *)
  val signature_size_in_bytes : int

  (** Build a value of type [signature] without performing any check on the
      input (hence the unsafe prefix because it might not give a correct
      inhabitant of the type [signature]).
      It is safe to use this function when verifying a signature as the
      signature function verifies if the point is
      in the prime subgroup. Using {!unsafe_signature_of_bytes} removes a
      verification performed twice when
      used {!signature_of_bytes_exn} or {!signature_of_bytes_opt}.

      The expected bytes format are the compressed form of a point on G1. *)
  val unsafe_signature_of_bytes : Bytes.t -> signature

  (** Build a value of type [signature] safely, i.e. the function checks the
      bytes given in parameters represents a point on the curve and in the
      prime subgroup. Raise [Invalid_argument] if the bytes are not in the
      correct format or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1. *)
  val signature_of_bytes_exn : Bytes.t -> signature

  (** Build a value of type [signature] safely, i.e. the function checks the
      bytes given in parameters represents a point on the curve and in the
      prime subgroup. Return [None] if the bytes are not in the correct format
      or does not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1. *)
  val signature_of_bytes_opt : Bytes.t -> signature option

  (** Returns a bytes representation of a value of type [signature]. The
      output is the compressed form a the point [Bls12_381.G1.t] the [signature]
      represents. *)
  val signature_to_bytes : signature -> Bytes.t

  (** [aggregate_signature_opt signatures] aggregates the signatures
      [signatures], following {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.8
      } section 2.8 }.
      Return [None] if [INVALID] is expected in the specification *)
  val aggregate_signature_opt : signature list -> signature option

  (** Basic scheme described in
      {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1}
      section 3.1}

      In a basic scheme, rogue key attacks are handled by requiring all
      messages signed by an aggregate signature to be distinct. This
      requirement is enforced in the definition of AggregateVerify.

      {!Basic.sign} and {!Basic.verify} implements the algorithms {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.6
      } CoreSign} and {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.7}
      CoreVerify}, respectively. *)
  module Basic : sig
    val sign : sk -> Bytes.t -> signature

    val verify : pk -> Bytes.t -> signature -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification.
        It implements the AggregateVerify algorithm specified in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1.1
        } section 3.1.1 }. Raise [Invalid_argument] if the messages are not
        distinct. *)
    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (** Augmentation scheme described in
      {{:https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2}
      section 3.2}

      In a message augmentation scheme, signatures are generated over the
      concatenation of the public key and the message, ensuring that messages
      signed by different public keys are distinct. *)
  module Aug : sig
    (** [sign sk msg] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.1
        } section 3.2.1 } *)
    val sign : sk -> Bytes.t -> signature

    (** [verify pk msg signature] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.2
        } section 3.2.2 } *)
    val verify : pk -> Bytes.t -> signature -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification.
        It implements the FastAggregateVerify algorithm specified in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2.3
        } section 3.2.3 }*)
    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (** Follow {{:
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3
      } section 3.3 }.

      A proof of possession scheme uses a separate public key validation step,
      called a proof of possession, to defend against rogue key attacks. This
      enables an optimization to aggregate signature verification for the case
      that all signatures are on the same message. *)
  module Pop : sig
    type proof = Bytes.t

    (** Equivalent to [core_sign] with the DST given in the specification, {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3}
        section 4.2.3 } *)
    val sign : sk -> Bytes.t -> signature

    (** Equivalent to [core_verify] with the DST given in the specification
        {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3}
        section 4.2.3 } *)
    val verify : pk -> Bytes.t -> signature -> bool

    (** [pop_proof sk] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.2
        } section 3.3.2 } *)
    val pop_prove : sk -> proof

    (** [pop_verify pk proof] implements the algorithm described in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.3
        } section 3.3.3 } *)
    val pop_verify : pk -> proof -> bool

    (** [aggregate_verify pks msg aggregated_signature] performs a aggregate
        signature verification. It supposes the same message [msg] has been
        signed. It implements the FastAggregateVerify algorithm specified in {{:
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.4
        } section 3.3.4 }*)
    val aggregate_verify : (pk * proof) list -> Bytes.t -> signature -> bool
  end
end
