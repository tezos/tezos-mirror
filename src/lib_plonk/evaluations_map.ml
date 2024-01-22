(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Alternative representation of polynomials containing
   the evaluations of a polynomial on a certain set and its
   degree *)

module SMap = Kzg.SMap

module type Evaluations_sig = sig
  include Octez_bls12_381_polynomial.Evaluations_sig

  (** [size_evaluations] returns the maximum size of elements in evaluations *)
  val size_evaluations : t SMap.t -> int

  (** [find_evaluation m name] returns the evaluation for a given name [name] *)
  val find_evaluation : t SMap.t -> string -> t

  (** [print_evaluations_name] prints [(name, degree, length)] for each evaluation *)
  val print_evaluations_name : t SMap.t -> unit

  (** [get_domain] returns the evaluation for ["X"] *)
  val get_domain : t SMap.t -> domain

  (** [compute_evaluations] converts the coefficient representation of each
  polynomial [pᵢ] to the evaluation representation.

  Note:
  - size of domain must be a power of two
  - size of a polynomial [pᵢ] must be less than or equal to size of domain *)
  val compute_evaluations : domain:domain -> polynomial SMap.t -> t SMap.t

  (** [compute_evaluations_update_map] writes the result of {!compute_evaluations} in
  [evaluations]. If [domain] is not provided, {!get_domain} is called *)
  val compute_evaluations_update_map :
    ?domain:domain -> evaluations:t SMap.t -> polynomial SMap.t -> t SMap.t

  (** [mul] invokes {!mul_c} with the evaluations for given names [poly_names] *)
  val mul :
    ?res:t ->
    evaluations:t SMap.t ->
    poly_names:string list ->
    ?composition_gx:int list * int ->
    ?powers:int list ->
    unit ->
    t

  (** [linear] invokes {!linear_c} with the evaluations for given names [poly_names] *)
  val linear :
    ?res:t ->
    evaluations:t SMap.t ->
    poly_names:SMap.key list ->
    ?linear_coeffs:scalar list ->
    ?composition_gx:int list * int ->
    ?add_constant:scalar ->
    unit ->
    t
end

module Make (E : Octez_bls12_381_polynomial.Evaluations_sig) :
  Evaluations_sig
    with type scalar = E.scalar
     and type domain = E.domain
     and type polynomial = E.polynomial
     and type t = E.t = struct
  include E

  let print_evaluations_name map =
    let s_eval =
      "{"
      ^ SMap.fold
          (fun k eval acc ->
            acc
            ^ Printf.sprintf "\n  %s -> (%d, %d)" k (degree eval) (length eval))
          map
          ""
      ^ "\n}"
    in
    Printf.printf "\nevaluations : %s" s_eval

  (* Returns the maximum size of elements in evaluations ;
     raise failure if max_size equals zero *)
  let size_evaluations evaluations =
    let evals = SMap.values evaluations in
    let size_max = List.fold_left max 0 @@ List.map length evals in
    if size_max = 0 then
      failwith "size_evaluations: a maximum size of evaluations can't be zero!"
    else size_max

  let not_found x =
    raise
      (Invalid_argument
         (Printf.sprintf "Evaluations : %s not found in evaluations map." x))

  let find_evaluation evaluations name =
    match SMap.find_opt name evaluations with
    | None -> not_found name
    | Some x -> x

  (* Returns the evals of "X" as a domain
     @raise Invalid_argument if "X" is not in evaluations
  *)
  let get_domain evaluations = to_domain (find_evaluation evaluations "X")

  let compute_evaluations ~domain poly_map =
    SMap.map (evaluation_fft domain) poly_map

  (* Adds evaluation of poly_map’s polynomials on the domain given by the evaluation of "X"
     @raise Invalid_argument if "X" is not in evaluations
  *)
  let compute_evaluations_update_map ?domain ~evaluations poly_map =
    let domain =
      match domain with Some domain -> domain | None -> get_domain evaluations
    in
    SMap.union_disjoint evaluations (compute_evaluations ~domain poly_map)

  let mul ?res ~evaluations ~poly_names ?composition_gx ?powers () =
    let list_eval = List.map (find_evaluation evaluations) poly_names in
    mul_c ?res ~evaluations:list_eval ?composition_gx ?powers ()

  let linear ?res ~evaluations ~poly_names ?linear_coeffs ?composition_gx
      ?add_constant () =
    let list_eval = List.map (find_evaluation evaluations) poly_names in
    linear_c
      ?res
      ~evaluations:list_eval
      ?linear_coeffs
      ?composition_gx
      ?add_constant
      ()
end
