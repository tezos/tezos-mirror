Adding a new instruction to Michelson language
==============================================


From time to time a need arises to enrich Michelson language with an additional
instruction. It might be an intimidating task, especially to someone who is not
very familiar with the interpreter code. Thus, here is a quick tutorial
outlining the most important steps required to add a new Michelson instruction.
outlining the most important steps required to add a new Michelson instruction.

Adding a new instruction requires providing the following elements (detailed in the rest of this document):

1. :ref:`a primitive <add_mich_primitive>`
2. :ref:`an internal representation <add_mich_ir>` (or IR for short)
3. :ref:`a translation rule <script_translator>`
4. :ref:`an execution rule <add_mich_execution_rule>`
5. :ref:`a gas cost model <add_mich_gas_model>`
6. :ref:`some documentation <add_mich_documentation>`

Once all of the above are provided, we may try compiling the protocol. We will
most likely encounter some non-exhaustive pattern matching errors, but after the
following steps are complete, these will usually be trivial to fix.

There are a lot of comments in the code base explaining further details about
the functions and data types described here. They are attached either to their
definitions in ``.ml`` files or in corresponding ``.mli`` files if definitions
are public.

For each function and data type mentioned on this page there is a pointer to the source
code file where it can be found.

.. _add_mich_primitive:

Michelson primitives
--------------------

The first thing we need to provide for creating a new instruction is a Michelson
primitive. A primitive is a textual representation of our instruction in the
Michelson code. Michelson primitives are organized into 5 namespaces:

1. keywords (lowercase words such as ``parameter``, ``storage``, ``code``)
2. types (lowercase words)
3. constructors (capitalised words)
4. constants (lowercase word ``constant`` followed by a string)
5. instructions (all uppercase words)

Of these only the last group is of interest to us here. ``DUP``, ``CONS`` and
``ADD`` are some examples of primitives representing instructions. By convention
constructor names for primitives representing instructions in the interpreter
implementation (as opposed to: in Michelson code) should start with capital
letter ``I``, for instance ``I_ADD`` for ``ADD`` instruction.

Primitives should be added with great care, though, as in binary representation
they are currently encoded as single bytes, so there can be no more than 256 of
them. Exceeding that number will force us to reserve more bytes for some
primitives, which will incur some space and performance overheads for storing
and executing contracts. Therefore, whenever possible we should prefer reusing
existing primitives to adding new ones. Fortunately, we can take advantage of
the fact that Michelson has a strong type system, ensuring that a same
primitives can be used differently in several contexts. Thus, by allowing an
existing primitive in a new context we can effectively add instructions without
adding new primitives.

For instance suppose we have a ``MAP`` instruction and primitive, which expects
a list at the top of the stack, and which recursively applies some given code
block to elements of the list, thus creating a new list, which is then put back
at the top of the stack. Suppose we now want a similar instruction for options,
instead of lists of elements. So far ``MAP`` is only considered valid in a very
particular state of the stack. By adding a new translation rule (see
:ref:`Script translator <script_translator>`), we can allow the same primitive
to be used when there is an option instead of the list at the top of the stack,
thus effectively creating a new instruction. This may be seen as "overloading"
the primitive for different argument types.

When adding a new primitive, it should be mapped to the appropriate namespace in
``namespace`` function (in
:src:`src/proto_alpha/lib_protocol/michelson_v1_primitives.ml`). In case of an
instruction, the appropriate namespace is ``Instr_namespace``. It is also necessary to map the primitive
constructor to the string representing it in Michelson code in
``prim_of_string`` and ``string_of_prim`` in the same module.

See :src:`src/proto_alpha/lib_protocol/michelson_v1_primitives.ml` for a
complete list of primitives known to Michelson and their respective textual
representations.

See also :doc:`../shell/micheline` to learn more about the concrete syntax of
Michelson language.

Michelson types
---------------

Before we talk about internal representations, we need to have a brief look at
how the interpreter handles types of Michelson expressions internally. Types
``ty`` and ``stack_ty`` are both defined in
:src:`src/proto_alpha/lib_protocol/script_typed_ir.ml`. Type ``ty`` enumerates
all types known to the Michelson interpreter and is parametrised by the
underlying OCaml type in which Michelson values are actually stored in memory.
Thus, when values of the ty type are pattern-matched on, their type parameters
help convince the OCaml compiler that the underlying Michelson expressions or
instructions are compatible (i.e. may be unified), and so that the execution
model for Michelson is type-safe.

``stack_ty`` is a collection of ``ty`` values occurring in a particular order,
which represents the type of the whole stack. The outermost value corresponds to
the top of the stack, while the innermost one is always ``Bot_t``, which
corresponds to the bottom of the stack. Note that ``stack_ty`` is parametrised
by two type parameters. The first one is the OCaml counterpart of the type of
the value at the top of the stack; the other combines the OCaml counterparts of
types of the remainder of the stack in the form of a *cons list*, i.e. a pair,
whose first element is a type and the other – another pair containing the
remainder of the list.

This is a recurring pattern within the interpreter: type parameters
corresponding to types of Michelson stacks usually come in pairs. As we will
shortly see, the first parameter in these pairs always corresponds to the type
of element at the top of the stack; the other – to the remainder of the stack.

.. _add_mich_ir:

Internal representation (IR)
----------------------------

A primitive is what represents our instruction in the Michelson script. Now we
need an IR, which is a piece of data containing all the information necessary to
actually execute the instruction. It should belong to the type::

    type ('before_top, 'before, 'result_top, 'result) kinstr = (* ... *)

which is defined in ``lib_protocol/script_typed_ir.ml``. As the module's name
suggests, this representation is already guaranteed to be well-typed. The type
``kinstr`` is the type of well-typed instruction sequences. Adding a new
instruction consists in defining a new constructor of the ``kinstr`` type
representing sequences starting with the new instruction, providing a rule which
constructs them from the primitive corresponding to the new instruction and a
rule which interprets them when the script in executed.

Instructions are parametrised by 4 type parameters:

1. The type of the top element of the stack prior to execution
2. The type of the remainder of the stack prior to execution
3. The type of the top element of the stack following execution
4. The type of the remainder of the stack following execution

For instance ``ICar`` instruction is defined as::

  | ICar :
      ('a * 'b, 's) kinfo * ('a, 's, 'r, 'f) kinstr
      -> ('a * 'b, 's, 'r, 'f) kinstr

The reason why the third parameter of the resulting ``kinstr`` is ``'r`` and not
simply ``'a`` (which is the type of the first element of the pair at the top of the
initial stack) is because this constructor also contains the next instruction,
which produces a value of some arbitrary type ``'r``. However, note
that this next instruction should expect ``'a`` at the top of its initial stack.

New instructions are added by extending the ``kinstr`` type with additional
variants. The variant should contain all the information necessary to execute
the instruction and its type parameters must ensure that the shape of the stack
prior to execution will provide arguments for the instruction and that the stack
following the execution will contain its result.

Each IR should also contain an instance of type ``kinfo``, which holds the
information about the type of the stack prior to the instruction's execution.
This information is mostly used for error reporting and logging. Another thing
each instruction's IR should contain is a continuation, i.e., another instruction
that is going to be executed next. This way the whole script can be represented
as a single instruction constructed by sequencing many instructions together.
See the next section to learn how such a sequence is constructed in the process
of translation.

Instructions taking all their arguments from the stack will require nothing more
than the above data, as the constructor itself will inform the interpreter, what
to do. In more complex cases, especially when some control transfer is involved,
some additional information may be needed. For example a ``MAP`` instruction
(regardless of the type it operates on) requires some more instructions (a
sub-program) to be executed to map one value into another. A ``PUSH``
instruction needs to know the type and the value it should push on the stack,
and so on.

.. _script_translator:

The translator
--------------

Now that we have chosen a primitive to represent our instruction in the code and
an internal representation (IR), we need to provide a rule that translates the
former into the latter. ``parse_instr`` function in
:src:`src/proto_alpha/lib_protocol/script_ir_translator.ml` is responsible for
this. Notice that the function ``parse_instr``, despite what its name suggests,
matches on pre-parsed Micheline AST. Micheline parser is not a part of the
protocol and therefore must be run by the client before the script is submitted
to the node in binary-encoded form.

The translator iterates over the AST instruction after instruction, maintaining
the type of the stack after each operation. This way it can also type check the
script in a single run. Additionally this makes information about the "current"
type of the stack available when parsing each consecutive instruction. Notice
that this function not only matches on Michelson primitives themselves, but also
on the type of the stack implied by translating previous instructions. This is
what lets us distinguish between different contexts in which the same primitive
may have different meaning. For instance the ``MAP`` instruction that we
mentioned before may perform mapping either on lists or on options, depending on
what the current type of the stack might be. Of course, the internal
representation will be different in each case. If the primitive and the current
stack type does not match any possibility, it's a typing error and the whole
script is considered ill-typed.

Because the IR is well-typed by construction, it is necessary for
``parse_instr`` translates the script and also type check it at the same time.
Motivations for this are twofold. For one thing type checking gives us strong
static guarantees about the behaviour of the interpreter. In particular it's
guaranteed to only produce well-typed Michelson values, which means each
instruction receives input that it expects. Secondly, the interpreter does not
have to check for types of data it finds on the stack (they're guaranteed to be
correct), which makes for faster execution. For this reason it is essential that
each IR contains a value of type ``kinfo`` (or an equivalent thereof), from
which the translator can obtain the type the stack should have after this
instruction is executed. Function ``kinfo_of_kinstr`` in
:src:`src/proto_alpha/lib_protocol/script_typed_ir.ml` is responsible for this
extraction.

An interesting situation occurs with instructions regulating control flow. These
usually receive one or more pieces of code (sub-programs) to execute depending
on some runtime conditions. ``MAP``, ``IF_LEFT``, ``IF_NONE`` are all examples
of such instructions. These sub-programs must also be well-typed. Moreover, if
there's more than one like in case of ``IF_LEFT`` or ``IF_NONE`` instructions, a
certain relationship between types of these sub-programs must hold. In
particular, they must ensure, that the entire instruction will always render a
resulting stack of the same type.

To ensure this, these sub-programs must be type-checked (and translated)
recursively. The aforementioned relationships between sub-programs' types should
be guaranteed by the constructor of the ``kinstr`` variant. However, in case of
multiple execution branches (sub-programs) their types must be unified before
proceeding. This is what ``merge_stacks`` function is for. It accepts two stack
types and verifies if they're equal. If so, the unified stack type is returned,
otherwise it results in a type error.

The precise return type of ``parse_instr`` is ``judgement`` defined in
:src:`src/proto_alpha/lib_protocol/script_ir_translator.ml`::

  type ('a, 's, 'b, 'u) cinstr = {
    apply :
      'r 'f. ('a, 's) kinfo -> ('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
  }

  type ('a, 's, 'b, 'u) descr = {
    loc : Script.location;
    bef : ('a, 's) stack_ty;
    aft : ('b, 'u) stack_ty;
    instr : ('a, 's, 'b, 'u) cinstr;
  }

  (* ... *)

  type ('a, 's) judgement =
    | Typed : ('a, 's, 'b, 'u) descr -> ('a, 's) judgement
    | Failed : {
      descr : 'b 'u. ('b, 'u) stack_ty -> ('a, 's, 'b, 'u) descr;
    }
      -> ('a, 's) judgement

This is quite a bit involved, so we'll explain these types and their role one at
a time. ``cinstr`` is an intermediate representation of an instruction whose
continuation is not yet known. As we can see, it's a function actually, which
given another instruction (the ``kinstr`` argument), sequences it after the
current one, which is held in the function's closure. A precise stack
description (a ``kinfo``) is also required, but note the initial type of the
stack is already predetermined by the type of ``cinstr``. The resulting type of
the instruction passed into the function, on the other hand, can be anything, as
witnessed by the **forall** annotation ``'r 'f.``. The construction of
``cinstr`` is such that we can already create it without knowing what the next
instruction will be (as opposed to a ``kinstr``, which should already contain
instruction's continuation).

A ``descr`` is a complete description of an instruction. It combines a
``cinstr`` with precise descriptions of the stack type before and after the
operation. These stack types are typically assembled by inspecting the initial
stack type passed to ``parse_instr`` (which we pattern match on in the said
function). Location is typically copied from the Micheline pattern as is and it
only serves the error-reporting purposes in case type-checking failed at a later
step.

Finally a successful typing judgement contains a full description of an
instruction, but is only parametrised by its input type. The return type is
existential and can only be retrieved by inspecting the ``aft`` field of the
contained description. This is vital, because when calling the type-checking
procedure, we don't yet know the return type of the program. In fact the whole
point of it is (in a sense) to learn that type. Typically when adding typing
rules for new instructions, we only need to be concerned with the successful
result (``Typed`` constructor). ``Failed`` is returned when the instruction does
not produce an output stack. This is true for instance of ``FAILWITH``
instruction, which immediately terminates the execution. Hence, it cannot be
given any sound type and therefore must be treated specially.

As already mentioned above, some instructions also receive pieces of code
(sub-programs) as their arguments. As these sub-programs must themselves be
well-typed, typically we will recursively call ``non_terminal_recursion``
function on them. If it succeeds in type-checking the sub-program, we use its
description to convert it into the type of the whole instruction (which will
usually be slightly different than the type of the sub-program). If it fails,
however, the error will be transparently propagated up the call stack.

``Failed`` judgements are treated differently by different instructions. Some,
like ``MAP`` convert them to typing errors, other unify them with any type the
other program branch might have, effectively treating them as if they had type
``forall 'a. 'a``. In fact, this is precisely the type that Michelson
specification assigns to the ``FAILWITH`` instruction.

.. _add_mich_execution_rule:

The interpreter
---------------

The interpreter is the heart of the Michelson implementation. This specific implementation
follows the small-step approach: at each execution step, the interpreter rewrites a machine
configuration made of a value stack and a continuation stack. Therefore, the interpreter takes a
script's IR, a storage and an input to the script as arguments, generates the
initial stack containing the storage content and the input, and then executes
the script, returning the final content of the stack. It's defined in
:src:`src/proto_alpha/lib_protocol/script_interpreter.ml` by the ``execute``
function.

The ``execute`` function does some preliminary preparations and then passes control to
the ``step`` function, which encodes the interpretation loop and where execution
rules for all instructions are given. The ``step`` function accepts:

1. Context and step constants (see below)
2. Remaining gas
3. Instruction
4. Continuation
5. The top value on the stack (called *accu*)
6. The remainder of the stack

Typically this function computes the new value of the stack and then calls
itself recursively with a new instruction (already available in the ``kinstr``
value). In some cases, however, some additional action may be required either
before or after the instruction is processed. These additional actions are
usually related to the control flow.

For instance, in the case of our ``MAP`` instruction for options, if it finds
``None`` at the top of the stack, will leave it without a change and simply
proceed to the next instruction. If, however, it finds ``Some x``, it should
pass control over to the sub-program given as parameter. This sub-program does
not expect an option, though, it expects our ``x`` unwrapped. Similarly it does
not return an option, but an arbitrary result which should be wrapped in a
``Some`` constructor after control returns to the main program. Without this
additional action, the types of stacks produced by the two branches would differ
and the program would be ill-typed. To remedy this and similar problems, the
interpreter also defines the ``continuation`` type (defined in
:src:`src/proto_alpha/lib_protocol/script_typed_ir.ml`). Whenever the control is
passed over to a sub-program, the ``next`` function can be called to manage the
flow of control around the sub-program (for instance executing it multiple times
in case of a loop). Also, each Michelson program ends with a special instruction
``IHalt``, which calls the same ``next`` function.

The ``next`` function occupies itself with continuations stack (argument no. 4
on the list above). Typically before transferring control to a sub-program, an
appropriate continuation is pushed on the continuation stack to manage its
outcome and resume execution of the main program. Only if there's no
continuation to perform, the program is actually terminated. In the ``MAP``
instruction example it would be wrapping the value at the top of the stack in
the ``Some`` constructor to match the type of the other branch. We simply put an
appropriate continuation in the continuation stack before executing the
sub-program with the rest of the main program appended as continuation.

Sometimes adding an instruction may involve adding a new continuation as well.
However, continuations are completely internal to the interpreter. They neither
have a representation in the Michelson code nor are they ever involved in
translation. A continuation is a value of type ``('a, 'b, 'c, 'd) continuation``
defined in :src:`src/proto_alpha/lib_protocol/script_typed_ir.ml`. Similarly to
an instruction, a new constructor of this type should contain all the
information required to execute the continuation. For instance ``KCons``
continuation contains an instruction and a continuation which should be executed
after it. The special ``KNil`` continuation marks the end of execution. After
the continuation is defined, it can be used freely in the interpreter.

The step constants passed to the function along with the context contain some
important information about the transaction itself, like the sender and the
target, the amount transferred and so on. See ``step_constants`` type definition
in :src:`src/proto_alpha/lib_protocol/script_interpreter.ml` for more details.

.. _add_mich_gas_model:

The Gas model
-------------

Each Michelson instruction also has a corresponding *gas model*, which estimates
how much gas should be consumed by the interpreter when executing the
instruction. The main goal of this is to prevent scripts from falling into
infinite loops and to protect bakers from abuse of their computing power. Of
course, it's impossible to make sure statically that a Michelson program always
terminates, but its computing time can be checked dynamically and that's the
main purpose of gas.

The gas model is a function approximating the time and computational resources
consumed by executing an instruction depending on the size of its argument.
Typically the role of the programmer adding an instruction is to define the
function for the instruction, but leaving any constant values in it abstract.
Additionally, a benchmark needs to be defined in order to find the appropriate
values for these constants. At a later point benchmarks are run on a dedicated
machine and constant values are filled in based on the results. It is essential
that these benchmarks are always run on the same hardware so that they return
comparable results.

More information on the gas model and on benchmarking the interpreter can be
found on the pages dedicated to the ``snoop`` library: :doc:`snoop_interpreter`.

.. _add_mich_documentation:

Documentation
-------------

Last but not least, the new instruction needs to be documented.
After all, nobody will ever use it otherwise!

Documenting the instruction may involve:

- Editing the :doc:`Michelson language page <../alpha/michelson>` (only the page corresponding to the Alpha protocol needs be changed, because injected protocols cannot be extended with new instructions).
- Updating the `Michelson reference website <https://tezos.gitlab.io/michelson-reference/>`__, by modifying the associated `repository <https://gitlab.com/tezos/michelson-reference>`__.
