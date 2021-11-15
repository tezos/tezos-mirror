Snoop and the Michelson Interpreter
===================================

In order to evaluate gas costs for the Michelson interpreter, we want to have a collection of benchmarks and models that cover every instruction of the language. This document explains how to modify the files in :src:`src/proto_alpha/lib_benchmarks_proto/` in order to fully integrate a new instruction for ``tezos-snoop``.

Interpreter Workload
--------------------

We first need to update the ``interpreter_workload.ml`` file. It specifies the data that is collected and saved as a ``workload`` for each benchmark. First, we add an entry to ``instruction_name`` with the name of the new instruction prefixed with ``N_``, it will be the name ``tezos-snoop`` will refer to this instruction as. Then add it to the list ``all_instructions``, and update ``string_of_instruction_name``. Now, we can add a function to the ``Instructions`` module. This function should take a number, possibly zero, of integer arguments that defines our workload. We want these arguments to be the different parameters that may influence the benchmarking time. For instance, the time ``DIG`` takes to be executed varies with the depth of digging, so the ``depth`` should be in the workload:

.. code-block:: ocaml

   module Instructions = struct
     (...)
     let dig depth = ir_sized_step N_IDig (unary "depth" depth)

We make these functions by calling ``ir_sized_step`` with the instruction name we defined above, and a list of arguments, usually formed with the helper functions ``nullary``, ``unary``, ``binary``, etc... Each argument must be given a descriptive name, like ``length``, or ``int1`` and ``int2``. Note that the order of the elements matters, as it will be the order in which they will be passed to the model later.

The only step left here is to update the ``extract_ir_sized_step`` function: update the pattern matching to include the new instruction, extract the relevant data from the instruction and the stack, and call the newly built function.

.. code-block:: ocaml

   let extract_ir_sized_step :
    fun ctxt instr stack ->
     let open Script_typed_cps_ir in
     match (instr, stack) with
     (...)
     | (INil (_, _), _) ->
         Instructions.nil
     | (IDig (_, n, _, _), _) ->
         Instructions.dig n
     | (IConcat_bytes (_, _), (ss, _)) ->
         let list_size = Size.list ss in
         let total_bytes =
           List.fold_left
             (fun x s -> Size.(add x (bytes s)))
             Size.zero
             ss.elements
         in
         Instructions.concat_bytes list_size total_bytes

Here are three examples: the first one is ``Nil``, its execution time is constant, so its function is called as is, and its workload will be empty. For ``Dig``, the depth needs to be passed to the ``Instructions.dig`` function, and it will simply be saved in its workload, to be used later for the model. The last example requires some computation: the execution time of ``Concat_bytes`` depends on the size of the list on top of the stack, as well as the total number of bytes to concatenate. We calculate these values here, so we can retrieve the appropriate workload.

In that last example, we use the module ``Size`` to convert the bytes into their size as an integer. It is oftentimes more appropriate to consider the size of the arguments, instead of their values, as the actual workload on the instructions.

.. code-block:: ocaml

   match (instr, stack) with
     (...)
     | (IMul_teznat (_, _), (x, (y, _))) ->
         Instructions.mul_teznat (Size.mutez x) (Size.integer y)

In this example, and for most mathematical operations in Michelson, the computation time actually depends on the size in memory of the arguments, not their values. Here, the size of an integer is the number of bytes needed to represent them, likewise for mutez. In this kind of scenario, we will make use of the ``Size`` module.


Interpreter Models
------------------

We now need to specify a cost model for the instruction in ``interpreter_model.ml``. Said models are in the module ``Models``, which already contains many generic ones.

.. code-block:: ocaml

   module Models = struct
     let const1_model name =
       Model.unknown_const1 ~const:(fv (sf "%s_const" name))

     let affine_model name =
       Model.affine
         ~intercept:(fv (sf "%s_const" name))
         ~coeff:(fv (sf "%s_coeff" name))

     (...)

They directly derive from generic models available in ``model.ml``. ``const1_model`` is used for constant-time instructions, ``affine_model`` is for instructions with cost function :math:`\lambda size. const + coeff * size`, where ``size`` is a value that appears in the workload, while ``const`` and ``coeff`` are free variables, etc... Note that the arity of the model must match the number of elements in the workload. Some instructions may require a specific model that does not yet exist. In this case, we can add it to the ``Models`` module, like ``join_tickets_model``, which has arity 4, and is exclusively used for the instruction ``IJoin_tickets``.

The affine model expects an argument for the constant value, called ``intercept``. It is the execution time of the instruction when the workload is 0. Some models assume that the intercept is 0, like the linear models. Otherwise, we may want to make two benchmarks for the instruction, one of which made exclusively for the computation of that ``intercept``.

When a model has been chosen, we can update the function ``ir_model``. Add your instruction to the pattern matching, and return the following

.. code-block:: ocaml

   model_[i] instr_or_cont ([your_model] name)

where ``your_model`` is your choosen model, and ``i`` its arrity. For instance:

.. code-block:: ocaml

   | N_INil ->
        model_0 instr_or_cont (const1_model name)
   | N_IDig | N_IDug | N_IDipN | N_IDropN | N_IDupN ->
        model_1 instr_or_cont (affine_model name)
   | N_IConcat_bytes ->
        model_2 instr_or_cont (concat_model name)


Interpreter benchmarks
----------------------

The interpreter benchmarks are located in ``interpreter_benchmarks.ml``, in the module ``Registration_section``, which is itself divided in multiple sub-modules for each family of instructions. Most benchmarks are created and registered using the following helper function ``benchmark``, or one of its derivatives.

.. code-block:: ocaml

   val benchmark:
       ?amplification:int ->
       ?intercept:bool ->
       name:Interpreter_worload.instruction_name ->
       kinstr_and_stack_sampler:(Default_config.config ->
                                 Random.State.t ->
                                 unit ->
                                 ex_stack_and_kinstr) ->
       unit


This function builds the ``Benchmark.S`` module and registers it, doing most of the work defined in Step 1 of the :doc:`usage example <snoop_example>`. Its arguments are as follows:

* ``amplification``, if provided, is the number of times an operation must be run in a single execution of the benchmark. If not specified, the instruction will be run only once. It is useful for instance when the operation itself takes very little time, and the calls to the timer take the most of the benchmark runtime.
* ``intercept`` (default ``false``) is ``true`` if and only if the benchmark is covering the intercept case. It can be used when the expected intercept for the chosen model is not 0. The user should then make sure that the provided sampler generates zero workload for the instruction.
* ``name`` is simply the name of the benchmarked instruction. It will also appear in the name of the registered benchmark. When searching for a benchmark in snoop, the names are structured as follows: ``<name>{_intercept}_<protocol_version>``, where ``<protocol_version>`` is either the version number of the protocol, or ``alpha``. ``_intercept`` only appears if the previous argument was set to ``true``. The function cannot register two benchmarks with the same ``name``, unless they have different ``intercept`` values.
* ``kinstr_and_stack_sampler`` is a function that, given a configuration and a random state, returns a stack and a ``kinstr`` on which the benchmark will be performed. If ``intercept`` is ``ŧrue``, this sampler should be so that zero workload is generated for the instruction.

Some other functions are provided to cover some simpler cases, they exist mostly to avoid creating a full sampler for both the stack and the instruction.

* ``simple_benchmark`` can be used when the stack can be whatever (as long as it is compatible with the benchmarked instruction). Its optional argument ``intercept_stack`` is the stack on which the intercept case will be run. If this argument is provided, two benchmarks will be registered for the instruction.
* ``benchmark_with_stack_sampler`` requires a sampler only for the stack, useful when the ``kinstr`` is fixed, which happens most of the time. ``benchmark_with_fixed_stack`` requires a fixed stack instead of a sampler. Both variations also exist for ``simple_benchmark``.
* Finally, ``continuation_benchmark`` works similarly to ``benchmark``, but for continuations instead of instructions.


Examples
~~~~~~~~

A simple benchmark
++++++++++++++++++

.. code-block:: ocaml

   let () =
      simple_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_ISwap
        ~kinstr:(ISwap (kinfo_unitunit, halt_unitunit))
        ()

Benchmark for ``ISwap``. Here the execution time is small enough that we want to use an amplification factor to obtain reasonable timings. The definition of ``kinstr`` uses some predefined values for ``kinfo`` and ``kinstr``, available at the begining of the registration section.

.. code-block:: ocaml

  let kinfo kstack_ty = {iloc = 0; kstack_ty}
  let halt stack_ty = IHalt (kinfo stack_ty)
  let halt_unit = halt (unit @$ bot)
  let halt_unitunit = halt (unit @$ unit @$ bot)
  let kinfo_unit = kinfo (unit @$ bot)
  let kinfo_unitunit = kinfo (unit @$ unit @$ bot)

In our example, ``ISwap`` is preformed for two unit types on a stack with nothing else. There is no need to specify a stack sampler, the function ``simple_benchmark`` infers it automatically from the given ``kinstr``.


A simple benchmark with an intercept stack
++++++++++++++++++++++++++++++++++++++++++
.. code-block:: ocaml

   let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IConcat_bytes
        ~intercept_stack:(Script_ir_translator.list_empty, ((), ()))
        ~kinstr:
          (IConcat_bytes (kinfo (list bytes @$ bot), halt (bytes @$ bot)))
        ()

For ``IConcat_bytes``, the intercept case would be an empty list: we are concatenating zero bytes on a list with zero elements, so both values of the workload are zero, as expected. In the non intercept case, the stack is randomly generated in accordance to the specification, which in this case is simply a random list of bytes. The sampling parameters, such as the bounds for the size of the list and its elements, can be controlled by a config file at runtime.

A less simple benchmark
+++++++++++++++++++++++
.. code-block:: ocaml

   let () =
      let dig = Micheline.(Prim (0, I_DIG, [Int (0, Z.of_int 0)], [])) in
      benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_IDig
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dig in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let dig n = Micheline.(Prim (0, I_DIG, [Int (0, Z.of_int n)], [])) in
      benchmark
        ~name:Interpreter_workload.N_IDig
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dig (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

Here we define two benchmarks, the first being the interception case, which is when ``DIG`` is called with the integer ``0``, which is enough since the depth is exactly the workload. Here the sampler uses ``parse_instr``, which builds the correct ``kinstr`` and ``stack``, given a Micheline node and a generated stack (here we use ``long_stack``, a big stack of unit types). The ``kinstr`` here must also be sampled, because we want to benchmark the instruction for different workloads, i.e different depths, which are sampled with ``sample_depth``.

Testing
-------

Assuming the workload is correctly defined for our benchmarking and modeling needs, we just need to check if the chosen model fits the data. For that, we can refer to the :doc:`usage example <snoop_example>`, and follow the given steps. If the resulting plot shows that the predicted execution time fits the empirical data, then it should be good. Otherwise, it should provide some insight to choose a more fitting model for the instruction.
