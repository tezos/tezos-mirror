Snoop usage tutorial
====================

We present a typical benchmark workflow using ``octez-snoop``.
We'll consider the case of the ``blake2b`` hashing function, which
is used among other things to hash blocks, operations and contexts:

.. code-block:: ocaml

   Tezos_crypto.Blake2B.hash_bytes : ?key:bytes -> bytes list -> Tezos_crypto.Blake2B.t

At the time of writing, this function is a thin wrapper which
concatenates the list of bytes and passes it to the ``blake2b``
implementation provided by `HACL* <https://github.com/hacl-star/hacl-star>`_.

Step 1: Defining the benchmark
------------------------------

Benchmarks correspond to OCaml modules implementing the ``Benchmark.S`` signature.
These must then be registered via the ``Registration.register`` function.
Of course, for this registration to happen, the file containing the benchmark
and the call to ``Registration.register`` should be linked with ``octez-snoop``.
See :doc:`snoop_arch` for complementary details.

We'll define the benchmark module chunk by chunk and describe each part.
For a starter, names are associated to various entities, and we use namespaces
to help organize them.

.. code-block:: ocaml

   open Tezos_benchmark

   let ns = Namespace.(make root "example")

Benchmarks are referenced by ``name``. The ``info`` field is a brief
description of the benchmark. ``module_filename`` will help users finding where
the benchmark is implemented simply using the command line interface. If we need to generate OCaml code to a
specific location, we can set it with the ``generated_code_destination`` field.
Finally, there's also a system of ``tags`` that allows listing benchmarks by
kind.

.. code-block:: ocaml

   module Blake2b_bench : Benchmark.S = struct
     let name = ns "Blake2b_example"
     let info = "Illustrating tezos-benchmark by benchmarking blake2b"
     let module_filename = __FILE__
     let generated_code_destination = None
     let tags = ["example"]

Typically, a benchmark will depend on a set of parameters corresponding e.g. to
the parameters of the samplers used to generate input data to the function
being benchmarked. This corresponds to the type ``config``. A ``default_config``
is provided, which can be overridden by specifying a well-formatted JSON file.
This is made possible by defining a ``config_encoding`` using the
:doc:`data-encoding <data_encoding>` library.

.. code-block:: ocaml

     type config = {max_bytes : int}
     let default_config = {max_bytes = 1 lsl 16}
     let config_encoding =
       let open Data_encoding in
       conv
         (fun {max_bytes} -> max_bytes)
         (fun max_bytes -> {max_bytes})
         (obj1 (req "max_bytes" int31))

Benchmarking involves measuring the execution time of some piece of code and
using the recorded execution time to fit a model. As explained in
:doc:`snoop_arch`, a model is in fact a function of three parameters: a
``workload``, the vector of free parameters to be fitted, and a name for future
reference. The ``workload`` corresponds to the information on the input of the
function being benchmarked required to predict its execution time. Typically, it
corresponds to some notion of "size" of the input. In order to be saved to disk,
we must define a ``workload_encoding`` as well. The ``workload`` type is abstract
from the outside of the module, however, for plotting purposes, it is necessary
to exhibit a vector-like structure on these workloads. The ``workload_to_vector``
function maps workloads to sparse vectors. If one is not interested in plotting,
this function can be made to always return ``Sparse_vec.String.zero``.

.. code-block:: ocaml

     type workload = {nbytes : int}
     let workload_encoding =
       let open Data_encoding in
       conv
         (fun {nbytes} -> nbytes)
         (fun nbytes -> {nbytes})
         (obj1 (req "nbytes" int31))
     let workload_to_vector {nbytes} =
       Sparse_vec.String.of_list [("nbytes", float_of_int nbytes)]

We expect the execution time of ``Blake2b.hash_bytes`` to be proportional
to the number of bytes being hashed, with possibly a small constant-time overhead.
Hence, we pick an ``affine`` model. The ``affine`` model is generic, of the form
:math:`\text{affine}(n) = \theta_0 + \theta_1 \times n` with :math:`\theta_i` the free
parameters. One must explain how to convert the ``workload`` to the argument ``n``.
This is the purpose of the ``conv`` parameter.

.. code-block:: ocaml

     let models =
       [ ( "blake2b",
           Model.make
             ~conv:(fun {nbytes} -> (nbytes, ()))
             ~model:
               (Model.affine
                  ~name
                  ~intercept:(Free_variable.of_namespace (ns "blake2b_const"))
                  ~coeff:(Free_variable.of_namespace (ns "blake2b_ns_p_byte"))) ) ]

Finally, we can define the actual benchmark. The function to be defined
is ``create_benchmarks``, which expects to be given an ``rng_state``,
a ``bench_num`` and a ``config`` and returns a list of suspensions, each
suspension yielding a benchmark when evaluated.

One might wonder why this particular signature has been chosen, instead of
returning directly a list of benchmarks, or simply requiring a benchmark
generator to be defined.

- The current signature allows for setup code to be shared by all benchmarks
  being generated (not the case here).
- Returning a list of suspensions allows to delay the sampling process
  and the memory allocation associated to benchmark generation until
  actually needed, hence preventing memory leaks.

The auxiliary function ``blake2b_benchmark`` is in charge of
preparing a ``closure``, corresponding to a call to ``Blake2b.hash_bytes``
applied to a random ``bytes``, and the associated ``workload``, containing the
size of the random ``bytes``. We want benchmarks to be easily replayable
given a seed, hence the closure-generation function is parameterized with
an explicit ``rng_state`` of type ``Random.State.t``.

.. code-block:: ocaml

     let blake2b_benchmark rng_state config () =
       let nbytes =
         Base_samplers.sample_in_interval
           rng_state
           ~range:{min = 1; max = config.max_bytes}
       in
       let bytes = Base_samplers.uniform_bytes rng_state ~nbytes in
       let workload = {nbytes} in
       (* The closure here is the piece of code to be benchmarked. *)
       let closure () = ignore (Tezos_crypto.Blake2B.hash_bytes [bytes]) in
       Generator.Plain {workload; closure}
     let create_benchmarks ~rng_state ~bench_num config =
       List.repeat bench_num (blake2b_benchmark rng_state config)
   end (* module Blake2b_bench *)

This concludes the definition of the benchmark. Let's register it:

.. code-block:: ocaml

   let () = Registration.register (module Blake2b_bench)

Step 2: Running the benchmark, inferring parameters, and generating code
------------------------------------------------------------------------

# Running the octez-snoop generate code for benchmarks command.

Step 3: checking the generated files
------------------------------------

# Showing the generated code and the plot to compare the model against the measures.

Step 4: options
---------------

# Changing bench-num and nsamples for faster benchmarking.
