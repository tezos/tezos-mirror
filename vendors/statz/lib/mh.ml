open Structures
open Stats

module type MH_parameters = sig
  include Ordered

  val pp : Format.formatter -> t -> unit

  val proposal : t -> t fin_prb

  val log_weight : t -> float
end

module Make (X : MH_parameters) = struct
  (* exception Null_backward_flow of X.t * X.t *)

  let sample_step (current_state : X.t) : X.t =
    let forward_prob = X.proposal current_state in
    let proposal_state = sample_prb forward_prob in
    let backward_prob = X.proposal proposal_state in
    let p_forward = eval_prb forward_prob proposal_state in
    let p_backward = eval_prb backward_prob current_state in
    if p_backward <= 0.0 then proposal_state
    else
      (* if p_backward <= 0.0 then
       *   raise (Null_backward_flow (current_state, proposal_state)) ; *)
      let log_forward_flow = X.log_weight current_state +. log p_forward in
      let log_backward_flow = X.log_weight proposal_state +. log p_backward in
      let log_acceptance = min 0.0 (log_backward_flow -. log_forward_flow) in
      let flip = Random.float 1.0 in
      if flip <= exp log_acceptance then proposal_state else current_state

  let silent () = ()

  let mcmc ~verbosity ~(initial : X.t) ~(burn_in : int) : X.t gen =
    let (burn_in_progress, sample_progress, trace) =
      match verbosity with
      | `Silent -> (silent, silent, false)
      | `Progress ->
          let burn_in = Tools.make_progress_printer burn_in "burn-in" in
          let sample = Tools.make_progress_printer 0 "sampling" in
          (burn_in, sample, false)
      | `Trace -> (silent, silent, true)
    in
    let rec sample_loop (index : int) (bound : int) (current_state : X.t) =
      if trace then
        Format.eprintf "burn-in %d/%d: %a\n%!" index bound X.pp current_state ;
      if index > bound then current_state
      else (
        burn_in_progress () ;
        sample_loop (index + 1) bound (sample_step current_state) )
    in
    let after_burn_in = sample_loop 1 burn_in initial in
    let state_ref = ref after_burn_in in
    generative ~sampler:(fun () ->
        let _ = sample_progress () in
        let new_state = sample_step !state_ref in
        state_ref := new_state ;
        new_state)
end
