module Main_Pack = Aggregation.Main_protocol
module L = Plompiler.LibCircuit

(** Tells the meta-verification proof what to do with the public inputs
    of the inner proofs *)
module type CircuitPI = sig
  (** number of public inputs in an individual inner proof *)
  val nb_inner : int

  (** total number of public inputs that are public for aPlonK *)
  val nb_outer : int

  (** predicate (in circuit form) on all of PI of the inner proofs and outer PI.
      giving the argument [[x_1;x_2];[y_1;y_2]] means that x_1 ; x_2 are resp.
      the fst and second PI of the first inner proof and y_1;y_2 the fst and snd
      PI of the snd inner proof *)
  val check :
    switches:bool L.repr list ->
    outer:L.scalar L.repr list ->
    inner:L.scalar L.repr list list ->
    bool L.repr L.t

  (** create outer from inner PI; note that the existence of this function
     restricts the outer PI to be computable from the inner PI. *)
  val outer_of_inner : Main_Pack.scalar list list -> Main_Pack.scalar list
end

module type S = sig
  val get_pi_module : string -> (module CircuitPI)
end

module No_public_input : CircuitPI = struct
  let nb_inner = 0

  let nb_outer = 0

  let check ~switches:_ ~outer:_ ~inner:_ = L.Bool.constant_bool true

  let outer_of_inner _ = []
end

module One_public_input : CircuitPI = struct
  let nb_inner = 1

  let nb_outer = 0

  let check ~switches:_ ~outer:_ ~inner:_ = L.Bool.constant_bool true

  let outer_of_inner _ = []
end

(* PI_parameters module designed for a rollup. There are 2 public inputs
   per proof, for the initial and final state respectively;
   everything is hidden expect the initial state of the very first proof &
   the final state of the very last proof. *)
module Rollup_example : CircuitPI = struct
  let nb_inner = 2

  let nb_outer = 2

  (* /!\ Note that this function assumes that the first proof is not turned
         off by the switches (ie the first switch is true);
         If the first proof is turned off, this function will NOT return the
         expected result. *)
  let check ~switches ~outer ~inner =
    let open L in
    (* we unroll the first iteration of the fold to start with a prec_out *)
    let* last_out, rel_inner_pi =
      fold2M
        (fun (prec_out, res) inner_pi switch ->
          match inner_pi with
          | [current_in; current_out] ->
              (* if switch is true then we compare the roots and replace the
                 old root by the new one; else we fill [ok] with true and keep
                 the old root for next comparison. *)
              let* ok =
                let* n_switch = Bool.bnot switch in
                let* compare = equal prec_out current_in in
                Bool.bor n_switch compare
              in
              let* out = Bool.ifthenelse switch current_out prec_out in
              let res = ok :: res in
              ret (out, res)
          | _ -> failwith "Check_pi : invalid format for inner_pi.")
        (List.nth (List.hd inner) 1, [])
        (List.tl inner)
        (List.tl switches)
    in
    match outer with
    | [first_root; last_root] ->
        let* first_root = equal first_root List.(hd (hd inner)) in
        let* last_root = equal last_root last_out in
        Bool.band_list ([first_root; last_root] @ rel_inner_pi)
    | _ -> failwith "Check_pi : invalid format for outer_pi."

  let outer_of_inner inner_pi =
    let rec get_last = function
      | [[_; x]] -> x
      | _ :: l -> get_last l
      | _ -> failwith "outer_of_inner : wrong inner_pi format"
    in
    [List.(hd (hd inner_pi)); get_last inner_pi]
end
