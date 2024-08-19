# `Tezos_benchmark_alpha`

This library is dedicated to sampling Michelson values and in particular
Michelson programs.

## Architecture

This library provides a sampling-based interface for well-typed
Michelson generation. Internally, this library is built on a sampler for an
intermediate language called Mikhailsky post-composed with a function to
map Mikhailsky terms to Michelson ones.

### Layer 1: Mikhailsky
  Mikhailsky corresponds to "Michelson with typed holes". Mikhailsky terms
  are encoded inside Micheline. The library `lib_benchmark_type_inference`
  provides the language definition as well as a type inference engine.

### Layer 2: Sampling Mikhailsky terms
  We sample Mikhailsky terms using a Markov chain where transitions correspond
  to local rewriting rules. The state space of the Markov chain is defined
  in `State_space` module. The rewriting infrastructure is provided in the
  `Kernel` module by instantiating `lib_micheline_rewriting`.
  Rewrites are checked to preserved well-typedness in the Mikhailsky sense
  using the type inference engine provided with Mikhailsky. The `Rules`
  module defines all rewriting rules, for both Mikhailsky _programs_
  (submodule `Rules.Instruction`) and _data_
  (submodule `Rules.Data_rewrite_leaves`). The function `Rules.rewriting`
  performs the enumeration of possible rewritings.

  The Markov chain is biased to sample terms of a specified
  size using the Metropolis-Hasting functors provided by `StaTz`.
  The instantiation of this Markov chain is defined in the `Sampler`
  module.

### Layer 3:
  Once we can sample Mikhaislky terms of a specified size, we need
  to convert them to Michelson ones. This is performed in two steps.
  - In the first step, we use the `Autocomplete` module to fill holes
    in Mikhailsky terms (resp. data) with well-typed code (resp. data).
    This is a relatively ad-hoc process.
  - The last step is to convert Mikhaislky to Michelson using the
    `Michelson` module.
