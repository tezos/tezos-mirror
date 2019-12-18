open Structures

(* ------------------------------------------------------------------------- *)
(* Metropolis-Hastings MCMC *)

module type MH_parameters =
sig
  include Ordered

  val pp : Format.formatter -> t -> unit
  (** State pretty-printer. *)

  val proposal : t -> t Stats.fin_prb
  (** Proposal Markov kernel. *)

  val log_weight : t -> float
  (** Logarithm of unormalized target density. *)
end

(** Metropolis-Hastings functor. *)
module Make(X : MH_parameters) :
sig

  val mcmc :
    verbosity: [`Silent|`Progress|`Trace] ->
    initial: X.t ->
    burn_in: int ->
    X.t Stats.gen
    (** The [mcmc] function produces a generative probability. Be warned that
        the samples obtained from [mcmc] are _not_ independent. The guarantee
        is that (if the proposal and weight are reasonable) the empirical
        measure will converge in law to the target measure. *)
end
