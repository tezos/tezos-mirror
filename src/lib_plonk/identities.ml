open Kzg.Bls
module SMap = Kzg.SMap

(* Module to operate with polynomials in FFT evaluations form. *)
module Evaluations = Evaluations_map.Make (Evals)

module Identities : sig
  (** The type for prover identities: functions from a (string) map of
      polynomials in FFT evaluations form to a (string) map of evaluated
      identities (also polynomials in FFT evaluations form). *)
  type prover_identities = Evaluations.t SMap.t -> Evaluations.t SMap.t

  (** The type for verifier identities: functions which map an evaluation point
      ξ an a [PC.answer] into a (string) map of evaluated identities. *)
  type verifier_identities =
    Scalar.t -> Scalar.t SMap.t SMap.t -> Scalar.t SMap.t

  (** The type for evaluation points. Either [X], [GX], or a custom point,
        which must be specified by an evaluation point name paired with a
        scalar that will multiply ξ. For example:
          - [X] could be implemented as [Custom ("x", Scalar.one)]
          - [GX] could be implemented as
            [Custom ("gx", generator)]. *)
  type eval_point = X | GX | Custom of string * Scalar.t [@@deriving repr]

  val string_of_eval_point : eval_point -> string

  (** [convert_eval_points gen x points] maps the polynomial protocol
               [points : eval_point list] into scalars, by evaluating the underlying
               "composition" polynomial at [x].
               The generator [gen] is used in case the [eval_point] equals [GX], in
               which case the resulting scalar is [x * gen]. *)
  val convert_eval_points :
    generator:Scalar.t -> x:Scalar.t -> eval_point list -> Scalar.t SMap.t

  (** [get_answer answers p name] extracts the evaluation of polynomial [name]
                at point [p] from the given [answers]. *)
  val get_answer : Scalar.t SMap.t SMap.t -> eval_point -> string -> Scalar.t

  (** A function to merge a list of prover identities into one. *)
  val merge_prover_identities : prover_identities list -> prover_identities

  (** A function to merge a list of verifier identities into one. *)
  val merge_verifier_identities :
    verifier_identities list -> verifier_identities
end = struct
  type prover_identities = Evaluations.t SMap.t -> Evaluations.t SMap.t

  type verifier_identities =
    Scalar.t -> Scalar.t SMap.t SMap.t -> Scalar.t SMap.t

  type eval_point = X | GX | Custom of string * Scalar.t [@@deriving repr]

  let string_of_eval_point = function
    | X -> "x"
    | GX -> "gx"
    | Custom (s, _) -> s

  let convert_eval_points ~generator ~x l =
    let eval = function
      | X -> x
      | GX -> Scalar.mul generator x
      | Custom (_, f) -> Scalar.mul f x
    in
    SMap.of_list @@ List.map (fun p -> (string_of_eval_point p, eval p)) l

  let get_answer answers x n =
    let x_map = SMap.find (string_of_eval_point x) answers in
    match SMap.find_opt n x_map with
    | Some x -> x
    | None ->
        raise
          (Invalid_argument
             (Printf.sprintf
                "Identities.get_answers : name '%s' not found in answers."
                n))

  let merge_prover_identities identities_list : prover_identities =
   fun evaluations ->
    List.fold_left
      (fun acc_map ids -> SMap.union_disjoint acc_map (ids evaluations))
      SMap.empty
      identities_list

  let merge_verifier_identities identities_list : verifier_identities =
   fun x answers ->
    List.fold_left
      (fun acc_map ids -> SMap.union_disjoint acc_map (ids x answers))
      SMap.empty
      identities_list
end

include Identities
