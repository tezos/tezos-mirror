open Structures

(* ------------------------------------------------------------------------- *)
(* Manipulating generative, empirical & finitely supported functions. *)

(** Generative probability (i.e. sampler) *)
type 'a gen

(** Empirical probability *)
type 'a emp

(** Finitely supported function *)
type 'a fin_fun

(** Finitely supported density *)
type 'a fin_den = private 'a fin_fun

(** Finitely supported probability *)
type 'a fin_prb = private 'a fin_fun

(** Total order on finitely supported probabilities, inherited from
    underlying total order. *)
val compare_prb : 'a fin_prb -> 'a fin_prb -> int

(** Sample from generative probability *)
val sample_gen : 'a gen -> 'a

(** Sample from empirical probability *)
val sample_emp : 'a emp -> 'a

(** Sample from finitely supported probability *)
val sample_prb : 'a fin_prb -> 'a

(** Functorial action on generative probabilities *)
val map_gen : ('a -> 'b) -> 'a gen -> 'b gen

(** Functorial action on empirical probabilities *)
val map_emp : ('a -> 'b) -> 'a emp -> 'b emp

(** Creates a generative probability distribution from a sampler. *)
val generative : sampler:(unit -> 'a) -> 'a gen

(** Creates a finitely supported _density_ from weighted points.
    A density is not necessarily normalized.
    The underlying set needs to be totally ordered. *)
val density :
  (module Ordered with type t = 'elt) -> ('elt * float) list -> 'elt fin_den

(** Returns the total mass associated to a finitely supported density. *)
val total_mass : 'a fin_den -> float

(** Normalize a density to obtain a probability measure.
    This amounts to integrate the density wrt the uniform measure
    to obtain (via Riesz) a probability. *)
val normalize : 'a fin_den -> 'a fin_prb

(** Samples (under iid assumptions) [nsamples] times from [sampler], producing
    an empirical distribution. *)
val empirical_of_generative : nsamples:int -> 'a gen -> 'a emp

(** Returns the empirical probability distribution associated to a raw array of samples. *)
val empirical_of_raw_data : 'a array -> 'a emp

(** Subsamples the given empirical generative distribution by sampling one out of [n]
    samples. *)
val subsample : n:int -> 'a gen -> 'a gen

(** Computes density from empirical distribution. *)
val fin_prb_of_empirical :
  (module Ordered with type t = 'elt) -> 'elt emp -> 'elt fin_prb

(** Finitely supported uniform distribution. *)
val uniform : (module Ordered with type t = 'elt) -> 'elt array -> 'elt fin_prb

(** Evaluates finitely supported probability on argument. Returns 0 if
    the argument is out of the support. *)
val eval_prb : 'elt fin_prb -> 'elt -> float

(** Samples (under iid assumptions) [nsamples] times from each element
    of [samplers], producing an array of empirical distributions.
    The sequence of calls to each sampler is random, so as to avoid
    background noise. *)
val empirical_float_random :
  nsamples:int -> samplers:float gen array -> float emp array

(** [truncate (module O) dist p] conditions an empirical distribution
    on a totally ordered space on the lower subset containing [p] mass. *)
val truncate :
  (module Ordered with type t = 'elt) -> 'elt emp -> float -> 'elt emp

(** [quantile (module O) dist p] returns the value x that splits the support of
    the distribution into a lower subset of mass [p] and an upper subset of
    mass [1-p]. *)
val quantile : (module Ordered with type t = 'elt) -> 'elt emp -> float -> 'elt

(** Computes the mean of a distribution that lives on a linear space. *)
val mean : (module Linear with type t = 'elt) -> 'elt emp -> 'elt

(** Computes the variance of a distribution on floats. Note that we use the
    biased estimator for empirical distributions, so care should be taken
    for small samples sizes (e.g. #samples > 50 is good). *)
val variance : float emp -> float

(** Remove data points which are [nsigmas] standard deviations above (or below)
    the mean. *)
val remove_outliers : nsigmas:float -> float emp -> float emp

(** Returns the raw data underlying an empirical distribution. *)
val raw_data_empirical : 'elt emp -> [ `Empirical of 'elt array ]

(** Returns the raw data underlying a finitely supported density. *)
val raw_data_density : 'elt fin_den -> [ `Density of ('elt * float) list ]

(** Returns the raw data underlying a finitely supported probability. *)
val raw_data_probability :
  'elt fin_prb -> [ `Probability of ('elt * float) list ]

(** Pretty print density or finitely supported probability. *)
val pp_fin_fun :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a fin_fun -> unit

(** Projects out the ordered structure underlying the support of the finitely
    supported function. *)
val proj_ordered : 'a fin_fun -> (module Ordered with type t = 'a)

(** Shannon entropy (ie computed with base 2 logarithms). *)
val shannon_entropy : float fin_prb -> float

(** Biased coin. Raises an error if [bias] is not in [0,1]. *)
val coin : bias:float -> bool fin_prb

(** Exponential distribution via inverse CDF. *)
val exponential : rate:float -> float gen

(** Gaussian distribution via Box-Muller transform.
    Returns a pair of _independent_ gaussian variates with
    prescribed mean and standard deviation. *)
val gaussian : mean:float -> std:float -> float gen

(** Binomial distribution.
    [binomial p n] returns the probability of having
    [k] successes over [n] experiments, according to
    a biased coin [p]. *)
val binomial : bool fin_prb -> int -> int fin_prb
