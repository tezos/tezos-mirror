Michelson: the language of Smart Contracts in Tezos
===================================================

This specification gives a detailed formal semantics of the Michelson
language and a short explanation of how smart contracts are executed
and interact in the blockchain.

The language is stack-based, with high level data types and primitives,
and strict static type checking. Its design cherry picks traits from
several language families. Vigilant readers will notice direct
references to Forth, Scheme, ML and Cat.

A Michelson program is a series of instructions that are run in
sequence: each instruction receives as input the stack resulting from the
previous instruction, and rewrites it for the next one. The stack
contains both immediate values and heap allocated structures. All values
are immutable and garbage collected.

The types of the input and output stack are fixed and monomorphic,
and the program is typechecked before being introduced into the system.
No smart contract execution can fail because an instruction has been
executed on a stack of unexpected length or contents.

This specification gives the complete instruction set, type system and
semantics of the language. It is meant as a precise reference manual,
not an easy introduction. Even though, some examples are provided at
the end of the document and can be read first or at the same time as
the specification. The document also starts with a less formal
explanation of the context: how Michelson code interacts with the
blockchain.

.. _address_prefixes_alpha:

Semantics of smart contracts and transactions
---------------------------------------------

The Tezos ledger currently has two types of accounts that can hold
tokens (and be the destinations of transactions).

  - An implicit account is a non programmable account, whose tokens
    are spendable and delegatable by a public key. Its address is
    directly the public key hash, and starts with ``tz1``, ``tz2``,
    ``tz3`` or ``tz4``.
  - A smart contract is a programmable account. A transaction to such
    an address can provide data, and can fail for reasons decided by
    its Michelson code. Its address is a unique hash that depends on
    the operation that led to its creation, and starts with ``KT1``.

From Michelson, they are indistinguishable. A safe way to think about
this is to consider that implicit accounts are smart contracts that
always succeed in receiving tokens, and do nothing else.

Finally, addresses prefixed with ``sr1`` identify :doc:`smart rollups <./smart_rollups>`.

Intra-transaction semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alongside their tokens, smart contracts keep a piece of storage. Both
are ruled by a specific logic specified by a Michelson program. A
transaction to a smart contract will provide an input value and in
option some tokens, and in return, the smart contract can modify its
storage and transfer its tokens.

The Michelson program receives as input a stack containing a single
pair whose first element is an input value and second element the
content of the storage space. It must return a stack containing a
single pair whose first element is the list of internal operations
that it wants to emit, and second element is the new contents of the
storage space. Alternatively, a Michelson program can fail, explicitly
using a specific opcode, or because something went wrong that could
not be caught by the type system (e.g. gas exhaustion).

A bit of polymorphism can be used at contract level, with a
lightweight system of named entrypoints: instead of an input value,
the contract can be called with an entrypoint name and an argument,
and these two components are transformed automatically in a simple and
deterministic way to an input value. This feature is available both
for users and from Michelson code. See the dedicated section.

Inter-transaction semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~~

An operation included in the blockchain is a sequence of "external
operations" signed as a whole by a source address. These operations
are of three kinds:

  - Transactions to transfer tokens to implicit accounts or tokens and
    parameters to a smart contract (or, optionally, to a specified
    entrypoint of a smart contract).
  - Originations to create new smart contracts from its Michelson
    source code, an initial amount of tokens transferred from the
    source, and an initial storage contents.
  - Delegations to assign the tokens of the source to the stake of
    another implicit account (without transferring any tokens).

Smart contracts can also emit "internal operations". These are run
in sequence after the external transaction completes, as in the
following schema for a sequence of two external operations.

::

    +------+----------------+-------+----------------+
    | op 1 | internal ops 1 |  op 2 | internal ops 2 |
    +------+----------------+-------+----------------+

Smart contracts called by internal transactions can in turn also emit
internal operation. The interpretation of the internal operations
of a given external operation uses a stack, as in the following
example, also with two external operations.

::

   +-----------+---------------+--------------------------+
   | executing | emissions     | resulting stack          |
   +-----------+---------------+--------------------------+
   | op 1      | 1a, 1b, 1c    | 1a, 1b, 1c               |
   | op 1a     | 1ai, 1aj      | 1ai, 1aj, 1b, 1c         |
   | op 1ai    |               | 1aj, 1b, 1c              |
   | op 1aj    |               | 1b, 1c                   |
   | op 1b     | 1bi           | 1bi, 1c                  |
   | op 1bi    |               | 1c                       |
   | op 1c     |               |                          |
   | op 2      | 2a, 2b        | 2a, 2b                   |
   | op 2a     | 2ai           | 2ai, 2b                  |
   | op 2ai    | 2ai1          | 2ai1, 2b                 |
   | op 2ai1   |               | 2b                       |
   | op 2b     | 2bi           | 2bi                      |
   | op 2bi    | 2bi1          | 2bi1                     |
   | op 2bi1   | 2bi2          | 2bi2                     |
   | op 2bi2   |               |                          |
   +-----------+---------------+--------------------------+

Failures
~~~~~~~~

All transactions can fail for a few reasons, mostly:

  - Not enough tokens in the source to spend the specified amount.
  - The script took too many execution steps.
  - The script failed programmatically using the ``FAILWITH`` instruction.

External transactions can also fail for these additional reasons:

  - The signature of the external operations was wrong.
  - The code or initial storage in an origination did not typecheck.
  - The parameter in a transfer did not typecheck.
  - The destination did not exist.
  - The specified entrypoint did not exist.

All these errors cannot happen in internal transactions, as the type
system catches them at operation creation time. In particular,
Michelson has two types to talk about other accounts: ``address`` and
``contract t``. The ``address`` type merely gives the guarantee that
the value has the form of a Tezos address. The ``contract t`` type, on
the other hand, guarantees that the value is indeed a valid, existing
account whose parameter type is ``t``. To make a transaction from
Michelson, a value of type ``contract t`` must be provided, and the
type system checks that the argument to the transaction is indeed of
type ``t``. Hence, all transactions made from Michelson are well
formed by construction.

In any case, when a failure happens, either total success or total
failure is guaranteed. If a transaction (internal or external) fails,
then the whole sequence fails and all the effects up to the failure
are reverted. These transactions can still be included in blocks, and
the transaction fees are given to the implicit account who baked the
block.

Language semantics
------------------

This specification explains in a symbolic way the computation performed by the
Michelson interpreter on a given program and initial stack to produce
the corresponding resulting stack. The Michelson interpreter is a pure
function: it only builds a result stack from the elements of an initial
one, without affecting its environment. This semantics is then naturally
given in what is called a big step form: a symbolic definition of a
recursive reference interpreter. This definition takes the form of a
list of rules that cover all the possible inputs of the interpreter
(program and stack), and describe the computation of the corresponding
resulting stacks.

Rules form and selection
~~~~~~~~~~~~~~~~~~~~~~~~

The rules have the main following form.

::

    > (syntax pattern) / (initial stack pattern)  =>  (result stack pattern)
        iff (conditions)
        where (recursions)
        and (more recursions)

The left hand side of the ``=>`` sign is used for selecting the rule.
Given a program and an initial stack, one (and only one) rule can be
selected using the following process. First, the toplevel structure of
the program must match the syntax pattern. This is quite simple since
there are only a few non-trivial patterns to deal with instruction
sequences, and the rest is made of trivial patterns that match one
specific instruction. Then, the initial stack must match the initial
stack pattern. Finally, some rules add extra conditions over the values
in the stack that follow the ``iff`` keyword. Sometimes, several rules
may apply in a given context. In this case, the one that appears first
in this specification is to be selected. If no rule applies, the result
is equivalent to the one for the explicit ``FAILWITH`` instruction. This
case does not happen on well-typed programs, as explained in the next
section.

The right hand side describes the result of the interpreter if the rule
applies. It consists in a stack pattern, whose parts are either
constants, or elements of the context (program and initial stack) that
have been named on the left hand side of the ``=>`` sign.

Recursive rules (big step form)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, the result of interpreting a program is derived from the
result of interpreting another one (as in conditionals or function
calls). In these cases, the rule contains a clause of the following
form.

::

    where (intermediate program) / (intermediate stack)  =>  (partial result)

This means that this rule applies in case interpreting the intermediate
state on the left gives the pattern on the right.

The left hand sign of the ``=>`` sign is constructed from elements of
the initial state or other partial results, and the right hand side
identify parts that can be used to build the result stack of the rule.

If the partial result pattern does not actually match the result of the
interpretation, then the result of the whole rule is equivalent to the
one for the explicit ``FAILWITH`` instruction. Again, this case does not
happen on well-typed programs, as explained in the next section.

Format of patterns
~~~~~~~~~~~~~~~~~~

Code patterns are of one of the following syntactical forms.

-  ``INSTR`` (an uppercase identifier) is a simple instruction (e.g.
   ``DROP``).
-  ``INSTR (arg) ...`` is a compound instruction, whose arguments can be
   code, data or type patterns (e.g. ``PUSH nat 3``).
-  ``{ (instr) ; ... }`` is a possibly empty sequence of instructions,
   (e.g. ``IF { SWAP ; DROP } { DROP }``), nested sequences can drop the
   braces.
-  ``name`` is a pattern that matches any program and names a part of
   the matched program that can be used to build the result.
-  ``_`` is a pattern that matches any instruction.

Stack patterns are of one of the following syntactical forms.

-  ``[FAILED]`` is the special failed state.
-  ``[]`` is the empty stack.
-  ``(top) : (rest)`` is a stack whose top element is matched by the
   data pattern ``(top)`` on the left, and whose remaining elements are
   matched by the stack pattern ``(rest)`` on the right (e.g.
   ``x : y : rest``).
-  ``name`` is a pattern that matches any stack and names it in order to
   use it to build the result.
-  ``_`` is a pattern that matches any stack.

Data patterns are of one of the following syntactical forms.

-  integer/natural number literals, (e.g. ``3``).
-  string literals, (e.g. ``"contents"``).
-  raw byte sequence literals (e.g. ``0xABCDEF42``).
-  ``Tag`` (capitalized) is a symbolic constant, (e.g. ``Unit``,
   ``True``, ``False``).
-  ``(Tag (arg) ...)`` tagged constructed data, (e.g. ``(Pair 3 4)``).
-  a code pattern for first class code values.
-  ``name`` to name a value in order to use it to build the result.
-  ``_`` to match any value.

The domain of instruction names, symbolic constants and data
constructors is fixed by this specification. Michelson does not let the
programmer introduce its own types.

Be aware that the syntax used in the specification may differ from
the :ref:`concrete syntax <ConcreteSyntax_alpha>`. In particular
some instructions are annotated with types that are not present in the
concrete language because they are synthesized by the typechecker.

Shortcuts
~~~~~~~~~

Sometimes, it is easier to think (and shorter to write) in terms of
program rewriting than in terms of big step semantics. When it is the
case, and when both are equivalents, we write rules of the form:

::

    p / S  =>  S''
    where   p' / S'  =>  S''

using the following shortcut:

::

    p / S  =>  p' / S'

The concrete language also has some syntax sugar to group some common
sequences of operations as one. This is described in this specification
using a simple regular expression style recursive instruction rewriting.

.. _michelson_type_system_alpha:

Introduction to the type system and notations
---------------------------------------------

This specification describes a type system for Michelson. To make things
clear, in particular to readers that are not accustomed to reading
formal programming language specifications, it does not give a
typechecking or inference algorithm. It only gives an intentional
definition of what we consider to be well-typed programs. For each
syntactical form, it describes the stacks that are considered well-typed
inputs, and the resulting outputs.

The type system is sound, meaning that if a program can be given a type,
then if run on a well-typed input stack, the interpreter will never
apply an interpretation rule on a stack of unexpected length or
contents. Also, it will never reach a state where it cannot select an
appropriate rule to continue the execution. Well-typed programs do not
block, and do not go wrong.

Type notations
~~~~~~~~~~~~~~

The specification introduces notations for the types of values, terms
and stacks. Apart from a subset of value types that appear in the form
of type annotations in some places throughout the language, it is
important to understand that this type language only exists in the
specification.

A stack type can be written:

-  ``[]`` for the empty stack.
-  ``(top) : (rest)`` for the stack whose first value has type ``(top)``
   and queue has stack type ``(rest)``.

Instructions, programs and primitives of the language are also typed,
their types are written:

::

    (type of stack before) -> (type of stack after)

The types of values in the stack are written:

-  ``identifier`` for a primitive data-type (e.g. ``bool``).
-  ``identifier (arg)`` for a parametric data-type with one parameter
   type ``(arg)`` (e.g. ``list nat``).
-  ``identifier (arg) ...`` for a parametric data-type with several
   parameters (e.g. ``map string int``).
-  ``[ (type of stack before) -> (type of stack after) ]`` for a code
   quotation, (e.g. ``[ int : int : [] -> int : [] ]``).
-  ``lambda (arg) (ret)`` is a shortcut for
   ``[ (arg) : [] -> (ret) : [] ]``.

Meta type variables
~~~~~~~~~~~~~~~~~~~

The typing rules introduce meta type variables. To be clear, this has
nothing to do with polymorphism, which Michelson does not have. These
variables only live at the specification level, and are used to express
the consistency between the parts of the program. For instance, the
typing rule for the ``IF`` construct introduces meta variables to
express that both branches must have the same type.

Here are the notations for meta type variables:

-  ``'a`` for a type variable.
-  ``'A`` for a stack type variable.
-  ``_`` for an anonymous type or stack type variable.

Typing rules
~~~~~~~~~~~~

The system is syntax directed, meaning that it defines a single
typing rule for each syntax construct. A typing rule restricts the type
of input stacks that are authorized for this syntax construct, links the
output type to the input type, and links both of them to the
subexpressions when needed, using meta type variables.

Typing rules are of the form:

::

    (syntax pattern)
    :: (type of stack before) -> (type of stack after) [rule-name]
       iff (premises)

Where premises are typing requirements over subprograms or values in the
stack, both of the form ``(x) :: (type)``, meaning that value ``(x)``
must have type ``(type)``.

A program is shown well-typed if one can find an instance of a rule that
applies to the toplevel program expression, with all meta type variables
replaced by non variable type expressions, and of which all type
requirements in the premises can be proven well-typed in the same
manner. For the reader unfamiliar with formal type systems, this is
called building a typing derivation.

Here is an example typing derivation on a small program that computes
``(x+5)*10`` for a given input ``x``, obtained by instantiating the
typing rules for instructions ``PUSH``, ``ADD`` and for the sequence, as
found in the next sections. When instantiating, we replace the ``iff``
with ``by``.

::

    { PUSH nat 5 ; ADD ; PUSH nat 10 ; MUL }
    :: [ nat : [] -> nat : [] ]
       by { PUSH nat 5 ; ADD }
          :: [ nat : [] -> nat : [] ]
             by PUSH nat 5
                :: [ nat : [] -> nat : nat : [] ]
                   by 5 :: nat
            and ADD
                :: [ nat : nat : [] -> nat : [] ]
      and { PUSH nat 10 ; MUL }
          :: [ nat : [] -> nat : [] ]
             by PUSH nat 10
                :: [ nat : [] -> nat : nat : [] ]
                   by 10 :: nat
            and MUL
                :: [ nat : nat : [] -> nat : [] ]

Producing such a typing derivation can be done in a number of manners,
such as unification or abstract interpretation. In the implementation of
Michelson, this is done by performing a recursive symbolic evaluation of
the program on an abstract stack representing the input type provided by
the programmer, and checking that the resulting symbolic stack is
consistent with the expected result, also provided by the programmer.

Side note
~~~~~~~~~

As with most type systems, it is incomplete. There are programs that
cannot be given a type in this type system, yet that would not go wrong
if executed. This is a necessary compromise to make the type system
usable. Also, it is important to remember that the implementation of
Michelson does not accept as many programs as the type system describes
as well-typed. This is because the implementation uses a simple single
pass typechecking algorithm, and does not handle any form of
polymorphism.

Types and instructions
----------------------

The complete sets of Michelson types and instructions are detailed in the
`interactive Michelson reference page <https://tezos.gitlab.io/michelson-reference/>`__.

- Specifically, it contains synthesis tables for `types <https://tezos.gitlab.io/michelson-reference/#types>`__
  and for `instructions <https://tezos.gitlab.io/michelson-reference/#instructions>`_.
- Instructions are also organized by `categories <https://tezos.gitlab.io/michelson-reference/#instructions-by-category>`__.
- Each instruction is precisely defined using typing and semantic inference rules.

Removed instructions and types
------------------------------

:doc:`../protocols/005_babylon` deprecated the following instructions. Because no smart
contract used these on Mainnet before they got deprecated, they have been
removed. The Michelson type-checker will reject any contract using them.

-  ``CREATE_CONTRACT { parameter 'p ; storage 'g ; code ... }``:
   Forge a new contract from a literal.

   ::

      Γ ⊢ CREATE_CONTRACT { parameter 'p ; storage 'g ; code ... }
      :: key_hash : option key_hash : bool : bool : mutez : 'g : 'S
      ⇒ operation : address : 'S

   There is a new version of this instruction, see its `documentation <https://tezos.gitlab.io/michelson-reference/#instr-CREATE_CONTRACT>`__.

-  ``CREATE_ACCOUNT``: Forge an account creation operation.

   ::

      Γ ⊢ CREATE_ACCOUNT :: key_hash : option key_hash : bool : mutez : 'S
      ⇒ operation : address : 'S

   Takes as argument the manager, optional delegate, the delegatable flag
   and finally the initial amount taken from the currently executed
   contract. This instruction originates a contract with two entrypoints;
   ``%default`` of type ``unit`` that does nothing and ``%do`` of type
   ``lambda unit (list operation)`` that executes and returns the
   parameter if the sender is the contract's manager.

-  ``STEPS_TO_QUOTA``: Push the remaining steps before the contract
   execution must terminate.

   ::

      Γ ⊢ STEPS_TO_QUOTA :: 'S ⇒ nat : 'S

:doc:`../protocols/016_mumbai` deprecated the following
type. Because no smart contract used it on Mainnet before it got
deprecated, it has been removed. The Michelson type-checker will
reject any contract using it.

-  ``tx_rollup_l2_address``: An address used to identify an account in
   a transaction rollup ledger. It is the hash of a BLS public key,
   used to authenticate layer-2 operations to transfer tickets from
   this account.

Macros
------

In addition to the instructions listed in the `interactive Michelson reference manual <https://tezos.gitlab.io/michelson-reference/>`__,
several extensions have been added to the language's concrete syntax. If you are
interacting with the node via RPC, bypassing the client, which expands away
these macros, you will need to desugar them yourself.

These macros are designed to be unambiguous and reversible, meaning that
errors are reported in terms of desugared syntax. Below you'll see
these macros defined in terms of other syntactic forms. That is how
these macros are seen by the node.

Compare
~~~~~~~

Syntactic sugar exists for merging ``COMPARE`` and comparison
combinators, and also for branching.

-  ``CMP{EQ|NEQ|LT|GT|LE|GE}``

::

    > CMP(\op) / S  =>  COMPARE ; (\op) / S

-  ``IF{EQ|NEQ|LT|GT|LE|GE} bt bf``

::

    > IF(\op) bt bf / S  =>  (\op) ; IF bt bf / S

-  ``IFCMP{EQ|NEQ|LT|GT|LE|GE} bt bf``

::

    > IFCMP(\op) / S  =>  COMPARE ; (\op) ; IF bt bf / S

Fail
~~~~

The ``FAIL`` macros is equivalent to ``UNIT; FAILWITH`` and is callable
in any context since it does not use its input stack.

-  ``FAIL``

::

    > FAIL / S  =>  UNIT; FAILWITH / S

Assertion macros
~~~~~~~~~~~~~~~~

All assertion operations are syntactic sugar for conditionals with a
``FAIL`` instruction in the appropriate branch. When possible, use them
to increase clarity about illegal states.

-  ``ASSERT``

::

    > ASSERT  =>  IF {} {FAIL}

-  ``ASSERT_{EQ|NEQ|LT|LE|GT|GE}``

::

    > ASSERT_(\op)  =>  IF(\op) {} {FAIL}

-  ``ASSERT_CMP{EQ|NEQ|LT|LE|GT|GE}``

::

    > ASSERT_CMP(\op)  =>  IFCMP(\op) {} {FAIL}

-  ``ASSERT_NONE``

::

    > ASSERT_NONE  =>  IF_NONE {} {FAIL}

-  ``ASSERT_SOME``

::

    > ASSERT_SOME @x =>  IF_NONE {FAIL} {RENAME @x}

-  ``ASSERT_LEFT``

::

    > ASSERT_LEFT @x =>  IF_LEFT {RENAME @x} {FAIL}

-  ``ASSERT_RIGHT``

::

    > ASSERT_RIGHT @x =>  IF_LEFT {FAIL} {RENAME @x}

Syntactic Conveniences
~~~~~~~~~~~~~~~~~~~~~~

These macros are simply more convenient syntax for various common
operations.

-  ``P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R``: A syntactic sugar
   for building nested pairs. In the case of right combs, ``PAIR n`` is more efficient.

::

    > PA(\right)R / S => DIP ((\right)R) ; PAIR / S
    > P(\left)IR / S => (\left)R ; PAIR / S
    > P(\left)(\right)R =>  (\left)R ; DIP ((\right)R) ; PAIR / S

A good way to quickly figure which macro to use is to mentally parse the
macro as ``P`` for pair constructor, ``A`` for left leaf and ``I`` for
right leaf. The macro takes as many elements on the stack as there are
leaves and constructs a nested pair with the shape given by its name.

Take the macro ``PAPPAIIR`` for instance:

::

    P A  P P A  I    I R
    ( l, ( ( l, r ), r ))

A typing rule can be inferred:

::

   PAPPAIIR
   :: 'a : 'b : 'c : 'd : 'S  ->  (pair 'a (pair (pair 'b 'c) 'd))

-  ``UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R``: A syntactic sugar
   for destructing nested pairs. These macros follow the same convention
   as the previous one.

::

    > UNPA(\right)R / S => UNPAIR ; DIP (UN(\right)R) / S
    > UNP(\left)IR / S => UNPAIR ; UN(\left)R / S
    > UNP(\left)(\right)R => UNPAIR ; DIP (UN(\right)R) ; UN(\left)R / S

-  ``C[AD]+R``: A syntactic sugar for accessing fields in nested pairs. In the case of right combs, ``CAR k`` and ``CDR k`` are more efficient.

::

    > CA(\rest=[AD]+)R / S  =>  CAR ; C(\rest)R / S
    > CD(\rest=[AD]+)R / S  =>  CDR ; C(\rest)R / S

-  ``CAR k``: Access the ``k`` -th part of a right comb of size ``n > k + 1``. ``CAR 0`` is equivalent to ``CAR`` and in general ``CAR k`` is equivalent to ``k`` times the ``CDR`` instruction followed by once the ``CAR`` instruction. Note that this instruction cannot access the last element of a right comb; ``CDR k`` should be used for that.

::

    > CAR n / S  =>  GET (2n+1) / S

-  ``CDR k``: Access the rightmost element of a right comb of size ``k``. ``CDR 0`` is a no-op, ``CDR 1`` is equivalent to ``CDR`` and in general ``CDR k`` is equivalent to ``k`` times the ``CDR`` instruction. Note that on a right comb of size ``n > k >= 2``, ``CDR k`` will return the right comb composed of the same elements but the ``k`` leftmost ones.

::

    > CDR n / S  =>  GET (2n) / S

-  ``IF_SOME bt bf``: Inspect an optional value.

::

    > IF_SOME bt bf / S  =>  IF_NONE bf bt / S

-  ``IF_RIGHT bt bf``: Inspect a value of a union.

::

    > IF_RIGHT bt bf / S  =>  IF_LEFT bf bt / S

-  ``SET_CAR``: Set the left field of a pair. This is equivalent to ``SWAP; UPDATE 1``.

::

    > SET_CAR  =>  CDR ; SWAP ; PAIR

-  ``SET_CDR``: Set the right field of a pair. This is equivalent to ``SWAP; UPDATE 2``.

::

    > SET_CDR  =>  CAR ; PAIR

-  ``SET_C[AD]+R``: A syntactic sugar for setting fields in nested
   pairs. In the case of right combs, ``UPDATE n`` is more efficient.

::

    > SET_CA(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CAR ; SET_C(\rest)R } ; CDR ; SWAP ; PAIR } / S
    > SET_CD(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CDR ; SET_C(\rest)R } ; CAR ; PAIR } / S

-  ``MAP_CAR`` code: Transform the left field of a pair.

::

    > MAP_CAR code  =>  DUP ; CDR ; DIP { CAR ; code } ; SWAP ; PAIR

-  ``MAP_CDR`` code: Transform the right field of a pair.

::

    > MAP_CDR code  =>  DUP ; CDR ; code ; SWAP ; CAR ; PAIR

-  ``MAP_C[AD]+R`` code: A syntactic sugar for transforming fields in
   nested pairs.

::

    > MAP_CA(\rest=[AD]+)R code / S   =>
        { DUP ; DIP { CAR ; MAP_C(\rest)R code } ; CDR ; SWAP ; PAIR } / S
    > MAP_CD(\rest=[AD]+)R code / S   =>
        { DUP ; DIP { CDR ; MAP_C(\rest)R code } ; CAR ; PAIR } / S

Concrete syntax
---------------
.. _ConcreteSyntax_alpha:

The concrete language is very close to the formal notation of the
specification. Its structure is extremely simple: an expression in the
language can only be one of the five following constructs.

1. An integer in decimal notation.
2. A character string.
3. A byte sequence in hexadecimal notation prefixed by ``0x``.
4. The application of a primitive to a sequence of expressions.
5. A sequence of expressions.

This simple five cases notation is called :doc:`../shell/micheline`.

In the Tezos protocol, the primitive ``constant`` with a single
character string applied has special meaning. See
:doc:`global_constants`.

Constants
~~~~~~~~~

There are three kinds of constants:

1. Integers or naturals in decimal notation.
2. Strings, with some usual escape sequences: ``\n``, ``\\``,
   ``\"``. Unescaped line-breaks (both ``\n`` and ``\r``) cannot
   appear in a Michelson string. Moreover, the current version of
   Michelson restricts strings to be the printable subset of 7-bit
   ASCII, namely characters with codes from within ``[32, 126]`` range,
   plus the escaped characters mentioned above.
3. Byte sequences in hexadecimal notation, prefixed with ``0x``.

Differences with the formal notation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The concrete syntax follows the same lexical conventions as the
specification: instructions are represented by uppercase identifiers,
type constructors by lowercase identifiers, and constant constructors
are capitalized.

All domain specific constants are Micheline constants with specific
formats. Some have two variants accepted by the data type checker: a
readable one in a string, and an optimized one using a more compact
encoding.

-  ``mutez`` amounts are written as naturals.
-  ``timestamp``\ s are written either using ``RFC3339`` notation
   in a string (readable), or as the number of seconds since Epoch
   (when positive) or before Epoch (when negative) (optimized).
-  ``contract``\ s, ``address``\ es, ``key``\ s and ``signature``\ s
   are written as strings, in their usual Base58 encoded versions
   (readable), or as their raw bytes (optimized).
-  ``bls12_381_g1``\ s and ``bls12_381_g2``\ s are written as their raw bytes, using a big-endian point encoding, `as specified here <https://docs.rs/bls12_381/latest/bls12_381/notes/serialization/index.html#bls12-381-serialization>`__.
-  ``bls12_381_fr``\ s are written as their raw bytes, using a little-endian encoding.

The optimized versions should not reach the RPCs, the protocol code
will convert to optimized by itself when forging operations, storing
to the database, and before hashing to get a canonical representation
of a datum for a given type.

To prevent errors, control flow primitives that take instructions as
parameters require sequences in the concrete syntax.

::

    IF { instr1_true ; instr2_true ; ... }
       { instr1_false ; instr2_false ; ... }

.. _syntax_of_scripts_alpha:

Main program structure
~~~~~~~~~~~~~~~~~~~~~~

The toplevel of a smart contract file must be an un-delimited sequence
of three primitive applications (in no particular order) that provide its
``code``, ``parameter`` and ``storage`` fields.

See the next section for a concrete example.

Annotations
-----------

The annotation mechanism of Michelson provides ways to better track
data on the stack and to give additional type constraints. Except for
a single exception specified just after, annotations are only here to
add constraints, *i.e.* they cannot turn an otherwise rejected program
into an accepted one. The notable exception to this rule is for
entrypoints: the semantics of the ``CONTRACT`` and ``SELF`` instructions vary depending on
their constructor annotations, and some contract origination may fail due
to invalid entrypoint constructor annotations.

Stack visualization tools like the Michelson's Emacs mode print
annotations associated with each type in the program, as propagated by
the typechecker as well as variable annotations on the types of elements
in the stack. This is useful as a debugging aid.

We distinguish three kinds of annotations:

- type annotations, written ``:type_annot``,
- variable annotations, written ``@var_annot``,
- and field or constructors annotations, written ``%field_annot``.

Type annotations
~~~~~~~~~~~~~~~~

Each type can be annotated with at most one type annotation. They are
used to give names to types. For types to be equal, their unnamed
version must be equal and their names must be the same or at least one
type must be unnamed.

For instance, the following Michelson program which put its integer
parameter in the storage is not well typed:

.. code-block:: michelson

    parameter (int :p) ;
    storage (int :s) ;
    code { UNPAIR ; SWAP ; DROP ; NIL operation ; PAIR }

Whereas this one is:

.. code-block:: michelson

    parameter (int :p) ;
    storage int ;
    code { UNPAIR ; SWAP ; DROP ; NIL operation ; PAIR }

Inner components of composed typed can also be named.

::

   (pair :point (int :x_pos) (int :y_pos))

Push-like instructions, that act as constructors, can also be given a
type annotation. The stack type will then have on top a type with a corresponding name.

::

   UNIT :t
   :: 'A -> (unit :t) : 'A

   PAIR :t
   :: 'a : 'b : 'S -> (pair :t 'a 'b) : 'S

   SOME :t
   :: 'a : 'S -> (option :t 'a) : 'S

   NONE :t 'a
   :: 'S -> (option :t 'a) : 'S

   LEFT :t 'b
   :: 'a : 'S -> (or :t 'a 'b) : 'S

   RIGHT :t 'a
   :: 'b : 'S -> (or :t 'a 'b) : 'S

   NIL :t 'a
   :: 'S -> (list :t 'a) : 'S

   EMPTY_SET :t 'elt
   :: 'S -> (set :t 'elt) : 'S

   EMPTY_MAP :t 'key 'val
   :: 'S -> (map :t 'key 'val) : 'S

   EMPTY_BIG_MAP :t 'key 'val
   :: 'S -> (big_map :t 'key 'val) : 'S


A no-op instruction ``CAST`` ensures the top of the stack has the
specified type, and change its type if it is compatible. In particular,
this allows to change or remove type names explicitly.

::

   CAST 'b
   :: 'a : 'S   ->   'b : 'S
      iff  'a = 'b

   > CAST t / a : S  =>  a : S


Variable annotations
~~~~~~~~~~~~~~~~~~~~

Variable annotations can only be used on instructions that produce
elements on the stack. An instruction that produces ``n`` elements on
the stack can be given at most ``n`` variable annotations.

The stack type contains both the types of each element in the stack, as
well as an optional variable annotation for each element. In this
sub-section we note:

- ``[]`` for the empty stack,
- ``@annot (top) : (rest)`` for the stack whose first value has type ``(top)`` and is annotated with variable annotation ``@annot`` and whose queue has stack type ``(rest)``.

The instructions which do not accept any variable annotations are:

::

   DROP
   SWAP
   DIG
   DUG
   IF_NONE
   IF_LEFT
   IF_CONS
   ITER
   IF
   LOOP
   LOOP_LEFT
   DIP
   FAILWITH

The instructions which accept at most one variable annotation are:

::

   DUP
   PUSH
   UNIT
   SOME
   NONE
   PAIR
   CAR
   CDR
   LEFT
   RIGHT
   NIL
   CONS
   SIZE
   MAP
   MEM
   EMPTY_SET
   EMPTY_MAP
   EMPTY_BIG_MAP
   UPDATE
   GET
   LAMBDA
   LAMBDA_REC
   EXEC
   ADD
   SUB
   CONCAT
   MUL
   OR
   AND
   XOR
   NOT
   ABS
   ISNAT
   INT
   NEG
   EDIV
   LSL
   LSR
   COMPARE
   EQ
   NEQ
   LT
   GT
   LE
   GE
   ADDRESS
   CONTRACT
   SET_DELEGATE
   IMPLICIT_ACCOUNT
   NOW
   LEVEL
   AMOUNT
   BALANCE
   HASH_KEY
   CHECK_SIGNATURE
   BLAKE2B
   SOURCE
   SENDER
   SELF
   SELF_ADDRESS
   CAST
   RENAME
   CHAIN_ID
   NAT
   BYTES

The instructions which accept at most two variable annotations are:

::

   UNPAIR
   CREATE_CONTRACT

Annotations on instructions that produce multiple elements on the stack
will be used in order, where the first variable annotation is given to
the top-most element on the resulting stack. Instructions that produce
``n`` elements on the stack but are given less than ``n`` variable
annotations will see only their top-most stack type elements annotated.

::

   UNPAIR @first @second
   :: pair 'a 'b : 'S
      ->  @first 'a : @second 'b : 'S

   UNPAIR @first
   :: pair 'a 'b : 'S
      ->  @first 'a : 'b : 'S

A no-op instruction ``RENAME`` allows to rename variables in the stack
or to erase variable annotations in the stack.

::

   RENAME @new
   :: @old 'a ; 'S -> @new 'a : 'S

   RENAME
   :: @old 'a ; 'S -> 'a : 'S


Field and constructor annotations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Components of pair types, option types and or types can be annotated
with a field or constructor annotation. This feature is useful to encode
records fields and constructors of sum types.

::

   (pair :point
         (int %x)
         (int %y))

The previous Michelson type can be used as visual aid to represent the
record type (given in OCaml-like syntax):

::

   type point = { x : int ; y : int }

Similarly,

::

   (or :t
       (int %A)
       (or
          (bool %B)
          (pair %C
                (nat %n1)
                (nat %n2))))

can be used to represent the algebraic data type (in OCaml-like syntax):

::

   type t =
     | A of int
     | B of bool
     | C of { n1 : nat ; n2 : nat }


Field annotations are part of the type (at the same level as type name
annotations), and so types with differing field names (if present) are
not considered equal.

Instructions that construct elements of composed types can also be
annotated with one or multiple field annotations (in addition to type
and variable annotations).

::

   PAIR %fst %snd
   :: 'a : 'b : 'S -> (pair ('a %fst) ('b %snd)) : 'S

   LEFT %left %right 'b
   :: 'a : 'S -> (or ('a %left) ('b %right)) : 'S

   RIGHT %left %right 'a
   :: 'b : 'S -> (or ('a %left) ('b %right)) : 'S

To improve readability and robustness, instructions ``CAR`` and ``CDR``
accept one field annotation. For the contract to type check, the name of
the accessed field in the destructed pair must match the one given here.

::

   CAR %fst
   :: (pair ('a %fst) 'b) : S -> 'a : 'S

   CDR %snd
   :: (pair 'a ('b %snd)) : S -> 'b : 'S


Syntax
~~~~~~

Primitive applications can receive one or many annotations.

An annotation is a sequence of characters that matches the regular
expression ``@%|@%%|%@|[@:%][_0-9a-zA-Z][_0-9a-zA-Z\.%@]*``.
Note however that ``@%``, ``@%%`` and ``%@`` are
:ref:`special annotations <SpecialAnnotations_alpha>` and are not allowed everywhere.

Annotations come after the primitive name and before its potential arguments.

::

    (prim @v :t %x arg1 arg2 ...)


Ordering between different kinds of annotations is not significant, but
ordering among annotations of the same kind is. Annotations of the same
kind must be grouped together.

For instance these two annotated instructions are equivalent:

::

   PAIR :t @my_pair %x %y

   PAIR %x %y :t @my_pair

An annotation can be empty, in this case it will mean *no annotation*
and can be used as a wildcard. For instance, it is useful to annotate
only the right field of a pair instruction ``PAIR % %right`` or to
ignore field access constraints, *e.g.* in the macro ``UNPPAIPAIR %x1 %
%x3 %x4``.

Annotations and macros
~~~~~~~~~~~~~~~~~~~~~~

Macros also support annotations, which are propagated on their expanded
forms. As with instructions, macros that produce ``n`` values on the
stack accept ``n`` variable annotations.

::

   DUU+P @annot
   > DUU(\rest=U*)P @annot / S  =>  DIP (DU(\rest)P @annot) ; SWAP / S

   C[AD]+R @annot %field_name
   > CA(\rest=[AD]+)R @annot %field_name / S  =>  CAR ; C(\rest)R @annot %field_name / S
   > CD(\rest=[AD]+)R @annot %field_name / S  =>  CDR ; C(\rest)R @annot %field_name / S

   CMP{EQ|NEQ|LT|GT|LE|GE} @annot
   > CMP(\op) @annot / S  =>  COMPARE ; (\op) @annot / S

The variable annotation on ``SET_C[AD]+R`` and ``MAP_C[AD]+R`` annotates
the resulting toplevel pair while its field annotation is used to check
that the modified field is the expected one.

::

   SET_C[AD]+R @var %field
   > SET_CAR @var %field =>  CDR %field ; SWAP ; PAIR @var
   > SET_CDR @var %field =>  CAR %field ; PAIR @var
   > SET_CA(\rest=[AD]+)R @var %field / S   =>
     { DUP ; DIP { CAR ; SET_C(\rest)R %field } ; CDR ; SWAP ; PAIR @var } / S
   > SET_CD(\rest=[AD]+)R  @var %field/ S   =>
     { DUP ; DIP { CDR ; SET_C(\rest)R %field } ; CAR ; PAIR @var } / S

   MAP_C[AD]+R @var %field code
   > MAP_CAR code  =>  DUP ; CDR ; DIP { CAR %field ; code } ; SWAP ; PAIR @var
   > MAP_CDR code  =>  DUP ; CDR %field ; code ; SWAP ; CAR ; PAIR @var
   > MAP_CA(\rest=[AD]+)R @var %field code / S   =>
     { DUP ; DIP { CAR ; MAP_C(\rest)R %field code } ; CDR ; SWAP ; PAIR @var} / S
   > MAP_CD(\rest=[AD]+)R @var %field code / S   =>
    { DUP ; DIP { CDR ; MAP_C(\rest)R %field code } ; CAR ; PAIR @var} / S

Macros for nested ``PAIR`` accept multiple annotations. Field
annotations for ``PAIR`` give names to leaves of the constructed
nested pair, in order.  This next snippet gives examples instead of
generic rewrite rules for readability purposes.

::

   PAPPAIIR @p %x1 %x2 %x3 %x4
   :: 'a : 'b : 'c : 'd : 'S
      -> @p (pair ('a %x1) (pair (pair ('b %x) ('c %x3)) ('d %x4))) : 'S

   PAPAIR @p %x1 %x2 %x3
   :: 'a : 'b : 'c : 'S  ->  @p (pair ('a %x1) (pair ('b %x) ('c %x3))) : 'S

Annotations for nested ``UNPAIR`` are deprecated.

Automatic variable and field annotations inferring
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When no annotation is provided by the Michelson programmer, the
typechecker infers some annotations in specific cases. This greatly
helps users track information in the stack for bare contracts.

For unannotated accesses with ``CAR`` and ``CDR`` to fields that are
named will be appended (with an additional ``.`` character) to the pair
variable annotation.

::

   CDAR
   :: @p (pair ('a %foo) (pair %bar ('b %x) ('c %y))) : 'S ->  @p.bar.x 'b : 'S

If fields are not named but the pair is still named in the stack then
``.car`` or ``.cdr`` will be appended.

::

   CDAR
   :: @p (pair 'a (pair 'b 'c)) : 'S ->  @p.cdr.car 'b : 'S

If the original pair is not named in the stack, but a field annotation
is present in the pair type the accessed value will be annotated with a
variable annotation corresponding to the field annotation alone.

::

   CDAR
   :: (pair ('a %foo) (pair %bar ('b %x) ('c %y))) : 'S ->  @bar.x 'b : 'S

A similar mechanism is used for context dependent instructions:

::

   ADDRESS  :: @c contract _ : 'S   ->   @c.address address : 'S

   CONTRACT 'p  :: @a address : 'S   ->   @a.contract contract 'p : 'S

   BALANCE :: 'S   ->   @balance mutez : 'S

   SOURCE  :: 'S   ->   @source address : 'S

   SENDER  :: 'S   ->   @sender address : 'S

   SELF  :: 'S   ->   @self contract 'p : 'S

   SELF_ADDRESS  :: 'S   ->   @self address : 'S

   AMOUNT  :: 'S   ->   @amount mutez : 'S

   NOW  :: 'S   ->   @now timestamp : 'S

   LEVEL :: 'S  ->   @level nat : 'S

Inside nested code blocks, bound items on the stack will be given a
default variable name annotation depending on the instruction and stack
type (which can be changed). For instance the annotated typing rule for
``ITER`` on lists is:

::

   ITER body
   :: @l (list 'e) : 'A  ->  'A
      iff body :: [ @l.elt e' : 'A -> 'A ]

Special annotations
~~~~~~~~~~~~~~~~~~~
.. _SpecialAnnotations_alpha:

The special variable annotations ``@%`` and ``@%%`` can be used on instructions
``CAR``, ``CDR``, and ``UNPAIR``. It means to use the accessed field name (if any) as
a name for the value on the stack. The following typing rule
demonstrates their use for instruction ``CAR``.

::

   CAR @%
   :: @p (pair ('a %fst) ('b %snd)) : 'S   ->   @fst 'a : 'S

   CAR @%%
   :: @p (pair ('a %fst) ('b %snd)) : 'S   ->   @p.fst 'a : 'S

The special field annotation ``%@`` can be used on instructions
``PAIR``, ``LEFT`` and ``RIGHT``. It means to use the variable
name annotation in the stack as a field name for the constructed
element. Two examples with ``PAIR`` follows, notice the special
treatment of annotations with ``.``.

::

   PAIR %@ %@
   :: @x 'a : @y 'b : 'S   ->   (pair ('a %x) ('b %y)) : 'S

   PAIR %@ %@
   :: @p.x 'a : @p.y 'b : 'S   ->  @p (pair ('a %x) ('b %y)) : 'S
   :: @p.x 'a : @q.y 'b : 'S   ->  (pair ('a %x) ('b %y)) : 'S

Entrypoints
-----------

The specification up to this point has been mostly ignoring existence
of entrypoints: a mechanism of contract level polymorphism. This
mechanism is optional, non intrusive, and transparent to smart
contracts that don't use them. This section is to be read as a patch
over the rest of the specification, introducing rules that apply only
in presence of contracts that make use of entrypoints.

Defining and calling entrypoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Entrypoints piggyback on the constructor annotations. A contract with
entrypoints is basically a contract that takes a disjunctive type (a
nesting of ``or`` types) as the root of its input parameter, decorated
with constructor annotations. An extra check is performed on these
constructor annotations: a contract cannot define two entrypoints with
the same name.

An external transaction can include an entrypoint name alongside the
parameter value. In that case, if there is a constructor annotation
with this name at any position in the nesting of ``or`` types, the
value is automatically wrapped into the according constructors. If the
transaction specifies an entrypoint, but there is no such constructor
annotation, the transaction fails.

For instance, suppose the following input type.

``parameter (or (or (nat %A) (bool %B)) (or %maybe_C (unit %Z) (string %C)))``

The input values will be wrapped as in the following examples.

::

   +------------+-----------+---------------------------------+
   | entrypoint | input     | wrapped input                   |
   +------------+-----------+---------------------------------+
   | %A         | 3         | Left (Left 3)                   |
   | %B         | False     | Left (Right False)              |
   | %C         | "bob"     | Right (Right "bob")             |
   | %Z         | Unit      | Right (Left Unit)               |
   | %maybe_C   | Right "x" | Right (Right "x")               |
   | %maybe_C   | Left Unit | Right (Left Unit)               |
   +------------+-----------+---------------------------------+
   | not given  | value     | value (untouched)               |
   | %BAD       | _         | failure, contract not called    |
   +------------+-----------+---------------------------------+

The ``default`` entrypoint
~~~~~~~~~~~~~~~~~~~~~~~~~~

A special semantics is assigned to the ``default`` entrypoint. If the
contract does not explicitly declare a ``default`` entrypoint, then it
is automatically assigned to the root of the parameter
type. Conversely, if the contract is called without specifying an
entrypoint, then it is assumed to be called with the ``default``
entrypoint. This behaviour makes the entrypoint system completely
transparent to contracts that do not use it.

This is the case for the previous example, for instance. If a value is
passed to such a contract specifying entrypoint ``default``, then the
value is fed to the contract untouched, exactly as if no entrypoint
was given.

A non enforced convention is to make the entrypoint ``default`` of
type unit, and to implement the crediting operation (just receive the
transferred tokens).

A consequence of this semantics is that if the contract uses the
entrypoint system and defines a ``default`` entrypoint somewhere else
than at the root of the parameter type, then it must provide an
entrypoint for all the paths in the toplevel disjunction. Otherwise,
some parts of the contracts would be dead code.

Another consequence of setting the entrypoint somewhere else than at
the root is that it makes it impossible to send the raw values of the
full parameter type to a contract. A trivial solution for that is to
name the root of the type. The conventional name for that is ``root``.

Let us recapitulate this by tweaking the names of the previous example.

``parameter (or %root (or (nat %A) (bool %B)) (or (unit %default) string))``

The input values will be wrapped as in the following examples.

::

   +------------+---------------------+-----------------------+
   | entrypoint | input               | wrapped input         |
   +------------+---------------------+-----------------------+
   | %A         | 3                   | Left (Left 3)         |
   | %B         | False               | Left (Right False)    |
   | %default   | Unit                | Right (Left Unit)     |
   | %root      | Right (Right "bob") | Right (Right "bob")   |
   +------------+---------------------+-----------------------+
   | not given  | Unit                | Right (Left Unit)     |
   | %BAD       | _                   | failure, contract not |
   +------------+---------------------+-----------------------+

Calling entrypoints from Michelson
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Michelson code can also produce transactions to a specific entrypoint.

For this, both types ``address`` and ``contract`` have the ability to
denote not just an address, but a pair of an address and an
entrypoint. The concrete notation is ``"address%entrypoint"``.
Note that ``"address"`` is strictly equivalent to ``"address%default"``,
and for clarity, the second variant is forbidden in the concrete syntax.

When the ``TRANSFER_TOKENS`` instruction is called, it places the
entrypoint provided in the contract handle in the transaction.

The ``CONTRACT t`` instruction has a variant ``CONTRACT %entrypoint
t``, that works as follows. Note that ``CONTRACT t`` is strictly
equivalent to ``CONTRACT %default t``, and for clarity, the second
variant is forbidden in the concrete syntax.

::

   +---------------+---------------------+------------------------------------------+
   | input address | instruction         | output contract                          |
   +---------------+---------------------+------------------------------------------+
   | "addr"        | CONTRACT t          | (Some "addr") if contract exists, has a  |
   |               |                     | default entrypoint of type t, or has no  |
   |               |                     | default entrypoint and parameter type t  |
   +---------------+---------------------+------------------------------------------+
   | "addr%name"   | CONTRACT t          | (Some "addr%name") if addr exists and    |
   +---------------+---------------------+ has an entrypoint %name of type t        |
   | "addr"        | CONTRACT %name t    |                                          |
   +---------------+---------------------+------------------------------------------+
   | "addr%_"      | CONTRACT %_ t       | None                                     |
   +---------------+---------------------+------------------------------------------+

Similarly, the ``SELF`` instruction has a variant ``SELF %entrypoint``,
that is only well-typed if the current contract has an entrypoint named ``%entrypoint``.

-  ``SELF %entrypoint``

::

    :: 'S   ->   contract 'p : 'S
       where   contract 'p is the type of the entrypoint %entrypoint of the current contract

Implicit accounts are considered to have a single ``default``
entrypoint of type ``Unit``.

JSON syntax
-----------

Micheline expressions are encoded in JSON like this:

-  An integer ``N`` is an object with a single field ``"int"`` whose
   value is the decimal representation as a string.

   ``{ "int": "N" }``

-  A string ``"contents"`` is an object with a single field ``"string"``
   whose value is the decimal representation as a string.

   ``{ "string": "contents" }``

-  A sequence is a JSON array.

   ``[ expr, ... ]``

- A primitive application is an object with two fields ``"prim"`` for
  the primitive name and ``"args"`` for the arguments (that must
  contain an array). A third optional field ``"annots"`` contains a
  list of annotations, including their leading ``@``, ``%`` or ``:``
  sign.

   ``{ "prim": "pair", "args": [ { "prim": "nat", "args": [] }, { "prim": "nat", "args": [] } ], "annots": [":t"] }``

As in the concrete syntax, all domain specific constants are encoded as
strings.

Development tools
-----------------

To ease the development of Michelson scripts, some tools are provided
to Michelson developers.

Emacs mode
~~~~~~~~~~

`Emacs <https://www.gnu.org/software/emacs/>`_ can be used as a practical environment for writing,
editing and debugging Michelson programs. `Install it <https://www.gnu.org/software/emacs/>`_ and follow the
configuration instructions in the Michelson Emacs README `here <https://gitlab.com/tezos/tezos/-/tree/master/emacs>`__.

Interactive toplevel
~~~~~~~~~~~~~~~~~~~~

An interactive Michelson toplevel (also known as a `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`__)
built on the :doc:`../user/mockup` mode of Octez client is available in
``scripts/michelson_repl.sh``, the typical usage is:

::

   $ octez-client --mode mockup --base-dir /tmp/mockup create mockup
   $ rlwrap scripts/michelson_repl.sh
   > UNIT
     { Stack_elt unit Unit }
   > UNIT
     { Stack_elt unit Unit ; Stack_elt unit Unit }
   > COMPARE
     { Stack_elt int 0 }

Examples
--------

Contracts in the system are stored as a piece of code and a global data
storage. The type of the global data of the storage is fixed for each
contract at origination time. This is ensured statically by checking on
origination that the code preserves the type of the global data. For
this, the code of the contract is checked to be of  type
``lambda (pair 'arg 'global) -> (pair (list operation) 'global)`` where
``'global`` is the type of the original global store given on origination.
The contract also takes a parameter and returns a list of internal operations,
hence the complete calling convention above. The internal operations are
queued for execution when the contract returns.

Empty contract
~~~~~~~~~~~~~~

The simplest contract is the contract for which the ``parameter`` and
``storage`` are all of type ``unit``. This contract is as follows:

.. code-block:: michelson

    code { CDR ;           # keep the storage
           NIL operation ; # return no internal operation
           PAIR };         # respect the calling convention
    storage unit;
    parameter unit;


Example contract with entrypoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following contract maintains a number in its storage. It has two
entrypoints ``add`` and ``sub`` to modify it, and the default
entrypoint, of type ``unit`` will reset it to ``0``.

::

   { parameter (or (or (nat %add) (nat %sub)) (unit %default)) ;
     storage int ;
     code { AMOUNT ; PUSH mutez 0 ; ASSERT_CMPEQ ; UNPAIR ;
            IF_LEFT
              { IF_LEFT { ADD } { SWAP ; SUB } }
              { DROP ; DROP ; PUSH int 0 } ;
            NIL operation ; PAIR } }


Example contract with recursive lambda
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following contract computes the factorial of the given parameter
using a recursive function and then saves the result in the storage.

In Michelson regular functions start with a stack containing a single
value, the function argument. If the function is of type ``lambda int
int``, when calling the function the stack will have just an
``int``. Recursive functions start with two values, the argument and
the function itself. Therefore, if the recursive function is of type
``lambda int int`` then, when it is being called, the stack will have
an ``int`` at the top and a ``lambda int int`` at the bottom.

In this recursive factorial we can see the first branch of the ``IF``,
this is the base case. The second one performs the recursive call. To
do that, we need to access the function. This is what the ``DUP 3``
instruction does. Then we decrement the argument and finally make the
recursive call with ``EXEC``.

::

    { parameter int;
      storage int;
      code { CAR ;
	     LAMBDA_REC  int int
			 { DUP;
			   EQ;
			   IF { PUSH int 1 }
			      { DUP;
				DUP 3;
				PUSH int 1;
				DUP 4;
				SUB;
				EXEC;
				MUL};
			   DIP { DROP 2 }};
	     SWAP;
	     EXEC;
	     NIL operation;
	     PAIR}}

Multisig contract
~~~~~~~~~~~~~~~~~

The multisig is a typical access control contract. The ownership of
the multisig contract is shared between ``N`` participants represented
by their public keys in the contract's storage. Any action on the
multisig contract needs to be signed by ``K`` participants where the
threshold ``K`` is also stored in the storage.

To avoid replay of the signatures sent to the contract, the signed
data include not only a description of the action to perform but also
the address of the multisig contract and a counter that gets
incremented at each successful call to the contract.

The multisig commands of :ref:`Octez command line client <client_manual_alpha>`
use this
smart contract. Moreover, `functional correctness of this contract has
been verified
<https://gitlab.com/nomadic-labs/mi-cho-coq/blob/master/src/contracts_coq/multisig.v>`__
using the Coq proof assistant.


.. code-block:: michelson

   parameter (pair
                (pair :payload
                   (nat %counter) # counter, used to prevent replay attacks
                   (or :action    # payload to sign, represents the requested action
                      (pair :transfer    # transfer tokens
                         (mutez %amount) # amount to transfer
                         (contract %dest unit)) # destination to transfer to
                      (or
                         (option %delegate key_hash) # change the delegate to this address
                         (pair %change_keys          # change the keys controlling the multisig
                            (nat %threshold)         # new threshold
                            (list %keys key)))))     # new list of keys
                (list %sigs (option signature)));    # signatures

   storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;

   code
     {
       UNPAIR ; SWAP ; DUP ; DIP { SWAP } ;
       DIP
         {
           UNPAIR ;
           # pair the payload with the current contract address, to ensure signatures
           # can't be replayed across different contracts if a key is reused.
           DUP ; SELF ; ADDRESS ; CHAIN_ID ; PAIR ; PAIR ;
           PACK ; # form the binary payload that we expect to be signed
           DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
         } ;

       # Check that the counters match
       UNPAIR @stored_counter; DIP { SWAP };
       ASSERT_CMPEQ ;

       # Compute the number of valid signatures
       DIP { SWAP } ; UNPAIR @threshold @keys;
       DIP
         {
           # Running count of valid signatures
           PUSH @valid nat 0; SWAP ;
           ITER
             {
               DIP { SWAP } ; SWAP ;
               IF_CONS
                 {
                   IF_SOME
                     { SWAP ;
                       DIP
                         {
                           SWAP ; DIIP { DIP { DUP } ; SWAP } ;
                           # Checks signatures, fails if invalid
                           CHECK_SIGNATURE ; ASSERT ;
                           PUSH nat 1 ; ADD @valid } }
                     { SWAP ; DROP }
                 }
                 {
                   # There were fewer signatures in the list
                   # than keys. Not all signatures must be present, but
                   # they should be marked as absent using the option type.
                   FAIL
                 } ;
               SWAP
             }
         } ;
       # Assert that the threshold is less than or equal to the
       # number of valid signatures.
       ASSERT_CMPLE ;
       DROP ; DROP ;

       # Increment counter and place in storage
       DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;

       # We have now handled the signature verification part,
       # produce the operation requested by the signers.
       NIL operation ; SWAP ;
       IF_LEFT
         { # Transfer tokens
           UNPAIR ; UNIT ; TRANSFER_TOKENS ; CONS }
         { IF_LEFT {
                     # Change delegate
                     SET_DELEGATE ; CONS }
                   {
                     # Change set of signatures
                     DIP { SWAP ; CAR } ; SWAP ; PAIR ; SWAP }} ;
       PAIR }

Views
~~~~~

Here is an example using views, consisting of two contracts.
The first contract defines two views at toplevel that are named ``add_v`` and ``mul_v``.

::

    { parameter nat;
      storage nat;
      code { CAR; NIL operation ; PAIR };
      view "add_v" nat nat { UNPAIR; ADD };
      view "mul_v" nat nat { UNPAIR; MUL };
    }


The second contract calls the ``add_v`` view of the above contract and obtains a result immediately.

::

    { parameter (pair nat address) ;
      storage nat ;
      code { CAR ; UNPAIR; VIEW "add_v" nat ;
             IF_SOME { } { FAIL }; NIL operation; PAIR }; }



Full grammar
------------

::

    <data> ::=
      | <int constant>
      | <string constant>
      | <byte sequence constant>
      | Unit
      | True
      | False
      | Pair <data> <data> ...
      | Left <data>
      | Right <data>
      | Some <data>
      | None
      | Lambda_rec <instruction>
      | { <data> ; ... }
      | { Elt <data> <data> ; ... }
      | instruction
    <natural number constant> ::=
      | [0-9]+
    <int constant> ::=
      | <natural number constant>
      | -<natural number constant>
    <string constant> ::=
      | "<string content>*"
    <string content> ::=
      | \"
      | \r
      | \n
      | \t
      | \b
      | \\
      | [^"\]
    <byte sequence constant> ::=
      | 0x[0-9a-fA-F]+
    <instruction> ::=
      | { <instruction> ... }
      | DROP
      | DROP <natural number constant>
      | DUP
      | DUP <natural number constant>
      | SWAP
      | DIG <natural number constant>
      | DUG <natural number constant>
      | PUSH <type> <data>
      | SOME
      | NONE <type>
      | UNIT
      | NEVER
      | IF_NONE { <instruction> ... } { <instruction> ... }
      | PAIR
      | PAIR <natural number constant>
      | CAR
      | CDR
      | UNPAIR
      | UNPAIR <natural number constant>
      | LEFT <type>
      | RIGHT <type>
      | IF_LEFT { <instruction> ... } { <instruction> ... }
      | NIL <type>
      | CONS
      | IF_CONS { <instruction> ... } { <instruction> ... }
      | SIZE
      | EMPTY_SET <comparable type>
      | EMPTY_MAP <comparable type> <type>
      | EMPTY_BIG_MAP <comparable type> <type>
      | MAP { <instruction> ... }
      | ITER { <instruction> ... }
      | MEM
      | GET
      | GET <natural number constant>
      | UPDATE
      | UPDATE <natural number constant>
      | IF { <instruction> ... } { <instruction> ... }
      | LOOP { <instruction> ... }
      | LOOP_LEFT { <instruction> ... }
      | LAMBDA <type> <type> { <instruction> ... }
      | LAMBDA_REC <type> <type> { <instruction> ... }
      | EXEC
      | APPLY
      | DIP { <instruction> ... }
      | DIP <natural number constant> { <instruction> ... }
      | FAILWITH
      | CAST
      | RENAME
      | CONCAT
      | SLICE
      | PACK
      | UNPACK <type>
      | ADD
      | SUB
      | MUL
      | EDIV
      | ABS
      | ISNAT
      | INT
      | NEG
      | LSL
      | LSR
      | OR
      | AND
      | XOR
      | NOT
      | COMPARE
      | EQ
      | NEQ
      | LT
      | GT
      | LE
      | GE
      | SELF
      | SELF_ADDRESS
      | CONTRACT <type>
      | TRANSFER_TOKENS
      | SET_DELEGATE
      | CREATE_CONTRACT { <instruction> ... }
      | IMPLICIT_ACCOUNT
      | VOTING_POWER
      | NOW
      | LEVEL
      | AMOUNT
      | BALANCE
      | CHECK_SIGNATURE
      | BLAKE2B
      | KECCAK
      | SHA3
      | SHA256
      | SHA512
      | HASH_KEY
      | SOURCE
      | SENDER
      | ADDRESS
      | CHAIN_ID
      | TOTAL_VOTING_POWER
      | PAIRING_CHECK
      | SAPLING_EMPTY_STATE <natural number constant>
      | SAPLING_VERIFY_UPDATE
      | TICKET
      | READ_TICKET
      | SPLIT_TICKET
      | JOIN_TICKETS
      | OPEN_CHEST
      | BYTES
      | NAT
    <type> ::=
      | <comparable type>
      | option <type>
      | list <type>
      | set <comparable type>
      | operation
      | contract <type>
      | ticket <comparable type>
      | pair <type> <type> ...
      | or <type> <type>
      | lambda <type> <type>
      | map <comparable type> <type>
      | big_map <comparable type> <type>
      | bls12_381_g1
      | bls12_381_g2
      | bls12_381_fr
      | sapling_transaction <natural number constant>
      | sapling_state <natural number constant>
      | chest
      | chest_key
    <comparable type> ::=
      | unit
      | never
      | bool
      | int
      | nat
      | string
      | chain_id
      | bytes
      | mutez
      | key_hash
      | key
      | signature
      | timestamp
      | address
      | option <comparable type>
      | or <comparable type> <comparable type>
      | pair <comparable type> <comparable type> ...


Reference implementation
------------------------

The language is implemented in OCaml as follows:

-  The lower internal representation is written as a GADT whose type
   parameters encode exactly the typing rules given in this
   specification. In other words, if a program written in this
   representation is accepted by OCaml's typechecker, it is guaranteed
   type-safe. This is of course also valid for programs not
   handwritten but generated by OCaml code, so we are sure that any
   manipulated code is type-safe.

   In the end, what remains to be checked is the encoding of the typing
   rules as OCaml types, which boils down to half a line of code for
   each instruction. Everything else is left to the venerable and well
   trusted OCaml.

-  The interpreter is basically the direct transcription of the
   rewriting rules presented above. It takes an instruction, a stack and
   transforms it. OCaml's typechecker ensures that the transformation
   respects the pre and post stack types declared by the GADT case for
   each instruction.

   The only things that remain to be reviewed are value dependent
   choices, such as we did not swap true and false when
   interpreting the IF instruction.

-  The input, untyped internal representation is an OCaml ADT with
   only 5 grammar constructions: ``String``, ``Int``, ``Bytes``, ``Seq`` and
   ``Prim``. It is the target language for the parser, since not all
   parsable programs are well typed, and thus could simply not be
   constructed using the GADT.

-  The typechecker is a simple function that recognizes the abstract
   grammar described in section X by pattern matching, producing the
   well-typed, corresponding GADT expressions. It is mostly a checker,
   not a full inferrer, and thus takes some annotations (basically the
   input and output of the program, of lambdas and of uninitialized maps
   and sets). It works by performing a symbolic evaluation of the
   program, transforming a symbolic stack. It only needs one pass over
   the whole program.

   Here again, OCaml does most of the checking, the structure of the
   function is very simple, what we have to check is that we transform a
   ``Prim ("If", ...)`` into an ``If``, a ``Prim ("Dup", ...)`` into a
   ``Dup``, etc.

.. michelson_tzt_alpha:

TZT, a Syntax extension for writing unit tests
----------------------------------------------

This section describes the TZT format, an extension of the Michelson
language allowing to run Michelson unit tests at a finer level than a
full smart contract script. This extension adds syntax to specify an
instruction (or sequence of instructions) to test, a concrete input
stack and the expected output stack.

These unit tests can be useful for both smart contract developers who
need to independently test various parts of the smart contracts they
develop and to the developers of new implementations of the Michelson
interpreter who need to check that their new implementations behave as
the reference implementation by passing `a conformance test suite
<https://gitlab.com/tezos/tzt-reference-test-suite>`__.

Similarly to Michelson scripts, the concrete syntax of TZT unit tests
is :doc:`../shell/micheline`.

TZT unit test files usually have the extension ``.tzt``. A unit test
file describes a single unit test. It consists of a Micheline sequence
of primitive applications (see :doc:`../shell/micheline`), in no particular order. This is
:ref:`similar to Michelson scripts <syntax_of_scripts_alpha>` but
the set of primitives allowed at the toplevel differ; in Michelson
scripts, the allowed toplevel primitives are ``parameter``
(mandatory), ``storage`` (mandatory), ``code`` (mandatory), and
``view`` (optional and repeated). For TZT unit tests, the toplevel
primitives which can be used are:

 - ``input``,
 - ``code``,
 - ``output``,
 - ``now``,
 - ``sender``,
 - ``source``,
 - ``chain_id``,
 - ``self``,
 - ``parameter``,
 - ``amount``,
 - ``balance``,
 - ``other_contracts``, and
 - ``big_maps``.

Mandatory primitives
~~~~~~~~~~~~~~~~~~~~

Each of the mandatory primitives ``input``, ``code``, and ``output``
must occur exactly once in a unit test file in no particular order.

The ``input`` primitive is used to declare the input stack (see the
:ref:`syntax of concrete stacks <syntax_of_concrete_stacks_alpha>`).

The ``code`` primitive is used to declare the instruction or sequence
of instructions to execute.

The ``output`` primitive is used to declare if the execution is
expected to succeed or fail and what result is expected from the
execution. For executions expected to succeed, the argument of the
``output`` primitive is simply the expected output stack (see the
:ref:`syntax of errors <syntax_of_errors_alpha>`). For executions
expected to fail, the argument is the expected error. In both cases,
the :ref:`wildcard pattern <omitting_parts_of_the_output_alpha>` can
be used to omit part of the expected output.

The simplest test which can be written asserts that executing no
instruction on the empty stack successfully returns the empty stack:

::

   input {};
   code {};
   output {}

Here is a slightly more involved test which demonstrates the effect of the `SWAP
<https://tezos.gitlab.io/michelson-reference/#instr-SWAP>`__ instruction:

::

   input
     {
       Stack_elt nat 8 ;
       Stack_elt bool False
     };
   code SWAP;
   output
     {
       Stack_elt bool False ;
       Stack_elt nat 8
     }

It is possible to test the effect of several instructions by wrapping them in a sequence:

::

   input
     {
       Stack_elt nat 8 ;
       Stack_elt bool False
     };
   code { SWAP ; SWAP };
   output
     {
       Stack_elt nat 8 ;
       Stack_elt bool False
     }

Here is an example showing how to test the ``FAILWITH`` instruction:

::

   input {Stack_elt nat 2};
   code FAILWITH;
   output (Failed 2)

Optional primitives
~~~~~~~~~~~~~~~~~~~

Optional primitives are used to set the execution context for the
test. Each of the optional primitives can be used at most once, in no
particular order.

 - ``amount`` (optional, defaults to 0): the amount, expressed in
   mutez, that should be pushed by the `AMOUNT
   <https://tezos.gitlab.io/michelson-reference/#instr-AMOUNT>`__
   instruction

 - ``balance`` (optional, defaults to 0): the balance, expressed in
   mutez, that should be pushed by the `BALANCE
   <https://tezos.gitlab.io/michelson-reference/#instr-BALANCE>`__
   instruction

 - ``now`` (optional, defaults to ``"1970-01-01T00:00:00Z"``): the
   timestamp that should be pushed by the `NOW
   <https://tezos.gitlab.io/michelson-reference/#instr-NOW>`__
   instruction

 - ``sender`` (optional, defaults to
   ``"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"``): the sender address
   that should be pushed by the `SENDER
   <https://tezos.gitlab.io/michelson-reference/#instr-SENDER>`__
   instruction

 - ``source`` (optional, defaults to
   ``"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"``): the source address
   that should be pushed by the `SOURCE
   <https://tezos.gitlab.io/michelson-reference/#instr-SOURCE>`__
   instruction

 - ``chain_id`` (optional, defaults to ``"NetXdQprcVkpaWU"``): the
   chain identifier that should be pushed by the `CHAIN_ID
   <https://tezos.gitlab.io/michelson-reference/#instr-CHAIN_ID>`__
   instruction

 - ``self`` (optional, defaults to
   ``"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"``): the address that
   should be pushed by the `SELF
   <https://tezos.gitlab.io/michelson-reference/#instr-SELF>`__ and
   `SELF_ADDRESS
   <https://tezos.gitlab.io/michelson-reference/#instr-SELF_ADDRESS>`__
   instructions

 - ``parameter`` (optional, defaults to ``unit``): the type of the
   parameter of the contract pushed by the `SELF
   <https://tezos.gitlab.io/michelson-reference/#instr-SELF>`__
   instruction

 - ``other_contracts`` (optional, defaults to ``{}``): mapping between
   the contract addresses that are assumed to exist and their
   parameter types (see the :ref:`syntax of other contracts
   specifications <syntax_of_other_contracts_alpha>`)

 - ``big_maps`` (optional, defaults to ``{}``): mapping between
   integers representing ``big_map`` indices and descriptions of big
   maps (see the :ref:`syntax of extra big maps specifications
   <syntax_of_extra_big_maps_alpha>`)

The following test example asserts that the default value for the `NOW
<https://tezos.gitlab.io/michelson-reference/#instr-NOW>`__
instruction is the unix epoch:

::

   input {};
   code NOW;
   output { Stack_elt timestamp "1970-01-01T00:00:00Z" }

The following example shows how to use the ``now`` toplevel primitive
to make the `NOW
<https://tezos.gitlab.io/michelson-reference/#instr-NOW>`__
instruction return a chosen timestamp:

::

   input {};
   now "2020-01-08T07:13:51Z";
   code NOW;
   output { Stack_elt timestamp "2020-01-08T07:13:51Z" }

.. _syntax_of_concrete_stacks_alpha:

Syntax of concrete stacks
~~~~~~~~~~~~~~~~~~~~~~~~~

A concrete stack is written as a Micheline sequence whose elements are
of the form ``Stack_elt <ty> <x>`` where ``<x>`` is a Michelson value
and ``<ty>`` is its type. For example, ``{ Stack_elt bool True ;
Stack_elt nat 42 }`` is a concrete stack of length 2 whose top element
is the boolean ``True`` and the bottom element is the natural number
``42``.

.. _omitting_parts_of_the_output_alpha:

Omitting parts of the output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any part of the ``output`` specification can be replaced with the
wildcard pattern ``_``.

For example, let's consider the following test of the ``PAIR`` instruction:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output {Stack_elt (pair bool string) (Pair True "foo")}

Omitting the ``True`` argument to the ``Pair`` primitive can be done as follows:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output {Stack_elt (pair bool string) (Pair _ "foo")}

Omitting the ``Pair`` primitive:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output {Stack_elt (pair bool string) (_ True "foo")}

Omitting the ``pair bool string`` type:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output {Stack_elt _ (Pair True "foo")}

Omitting the resulting stack element:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output {_}

Omitting all of the output:

::

   input {Stack_elt bool True; Stack_elt string "foo"};
   code PAIR;
   output _

The difference between the last two examples is that ``output {_}``
means that the instruction is expected to successfully return a stack
of length 1 while ``output _`` means that nothing in particular is
expected from the execution of the instruction, not even being
successful.

The wildcard pattern is typically used to omit unspecified aspects of
the Michelson language when writing portable tests; in particular the
cryptographic nonces in values of type ``operation`` (see the
:ref:`syntax of concrete operations
<syntax_of_concrete_operations_alpha>`) or implementation-specific
parts of error outputs (see the :ref:`syntax of errors
<syntax_of_errors_alpha>`).

.. _output_normalization_alpha:

Output normalization
~~~~~~~~~~~~~~~~~~~~

The input and output stacks can use the readable and optimized formats
for Michelson values and even mix the formats; for a test to pass, the
expected output does not need to syntactically match the result of the
execution but only to match up to conversion between optimized and
readable formats; the TZT test runner is responsible for normalizing
the actual output and the expected one to common format. This means in
particular that conversion between readable and optimized formats can
be tested by using ``{}`` as the ``code`` instruction sequence to
test; for example these two tests pass:

::

   input {Stack_elt address 0x0000e7670f32038107a59a2b9cfefae36ea21f5aa63c};
   code {};
   output {Stack_elt address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"}

::

   input {Stack_elt address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"};
   code {};
   output {Stack_elt address 0x0000e7670f32038107a59a2b9cfefae36ea21f5aa63c}

This normalization feature is however incompatible with using the
:ref:`wildcard pattern <omitting_parts_of_the_output_alpha>` in the
output; when using wildcards the output must be formatted using the
readable format so the following test does not pass:

::

   input {Stack_elt address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"};
   code {};
   output {Stack_elt _ 0x0000e7670f32038107a59a2b9cfefae36ea21f5aa63c}

but the following test does pass:

::

   input {Stack_elt address 0x0000e7670f32038107a59a2b9cfefae36ea21f5aa63c};
   code {};
   output {Stack_elt _ "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"}

.. _syntax_of_errors_alpha:

Syntax of errors
~~~~~~~~~~~~~~~~

To test that the execution of an instruction fails, the following
syntaxes can be used instead of the output stack as the argument of the
``output`` toplevel primitive to specify which error the instruction is expected to
raise:

 - ``(StaticError <error description>)``: an error occurred before the
   instruction was executed; the error description format is
   unspecified so consider using a :ref:`wildcard
   <omitting_parts_of_the_output_alpha>` such as ``(StaticError _)``
   to write portable tests;

 - ``(Failed <value>)``: the execution reached a ``FAILWITH``
   instruction and the topmost element of the stack at this point was
   ``<value>``;

 - ``MutezOverflow``: an addition or multiplication on type ``mutez``
   produced a result which was too large to be represented as a value
   of type ``mutez``;

 - ``MutezUnderflow``: a mutez subtraction resulted in a negative
   value. This should only happen in the case of the deprecated
   ``mutez`` case of the ``SUB`` instruction;

 - ``GeneralOverflow``: the number of bits to shift using the ``LSL``
   or ``LSR`` instruction was too large;


The following example shows how to test a runtime failure; it asserts
that the `FAILWITH
<https://tezos.gitlab.io/michelson-reference/#instr-FAILWITH>`__
instruction produces a runtime error containing the top of the stack.

::

   input { Stack_elt nat 4 ; Stack_elt bytes 0x };
   code FAILWITH;
   output (Failed 4)

The following example shows how to test type checking failure; it
asserts that the `DUP
<https://tezos.gitlab.io/michelson-reference/#instr-DUP>`__
instruction cannot be used on an empty stack.

::

   input {};
   code DUP;
   output (StaticError _)

The following example shows another kind of static failure: a string
cannot be passed as argument to the `DUP
<https://tezos.gitlab.io/michelson-reference/#instr-DUP>`__
instruction.

::

   input { Stack_elt nat 8 };
   code { DUP "foo" };
   output (StaticError _)

.. _syntax_of_concrete_operations_alpha:

Syntax of concrete operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `operation type
<https://tezos.gitlab.io/michelson-reference/#type-operation>`__ has
no concrete syntax in Michelson. In order to specify the result of the
operation forging instructions `TRANSFER_TOKENS
<https://tezos.gitlab.io/michelson-reference/#instr-TRANSFER_TOKENS>`__,
`CREATE_CONTRACT
<https://tezos.gitlab.io/michelson-reference/#instr-CREATE_CONTRACT>`__,
and `SET_DELEGATE
<https://tezos.gitlab.io/michelson-reference/#instr-SET_DELEGATE>`__ ,
the following data constructors are added:

 - ``Transfer_tokens``,
 - ``Create_contract``, and
 - ``Set_delegate``.

They take as arguments the inputs of the corresponding operation
forging instructions plus a cryptographic nonce represented as a byte
sequence. The result of ``TRANSFER_TOKENS``, ``CREATE_CONTRACT``,
and ``SET_DELEGATE`` have respectively the following shapes:

 - ``Transfer_tokens <argument> <amount in mutez> <address of destination> <nonce>``,
 - ``Create_contract { <script> } <optional delegate> <initial balance in mutez> <initial storage> <nonce>``, and
 - ``Set_delegate <optional delegate> <nonce>``.

The computation of the cryptographic nonce is not specified. To write
portable tests, the nonces appearing in output stack expectations
should be replaced by :ref:`a wildcard pattern
<omitting_parts_of_the_output_alpha>`.

Here is an example unit test for the ``SET_DELEGATE`` instruction used
to set the delegate of the current contract to the account at address
``tz1NwQ6hkenkn6aYYio8VnJvjtb4K1pfeU1Z``:

::

  input { Stack_elt (option key_hash) (Some "tz1NwQ6hkenkn6aYYio8VnJvjtb4K1pfeU1Z") } ;
  code SET_DELEGATE ;
  output { Stack_elt operation (Set_delegate (Some "tz1NwQ6hkenkn6aYYio8VnJvjtb4K1pfeU1Z") _) }

.. _syntax_of_other_contracts_alpha:

Syntax of other contracts specifications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behaviour of the `CONTRACT
<https://tezos.gitlab.io/michelson-reference/#instr-CONTRACT>`__
instruction depends on whether or not its input is the address of an
originated contract accepting the expected type as parameter. To test
it, the ``other_contract`` toplevel primitive can be used to specify
which contracts are assumed to be originated and which type they
accept as parameter.

The mapping given to the ``other_contract`` toplevel primitive is a
Micheline sequence whose elements have the form ``Contract "KT1..."
<ty>`` where ``"KT1..."`` is a valid smart contract address and
``<ty>`` is the type of its parameter. Each address should appear at
most once and the order is irrelevant.

.. _syntax_of_extra_big_maps_alpha:

Syntax of extra big maps specifications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behaviour of the instructions operating on type `big_map
<https://tezos.gitlab.io/michelson-reference/#type-big_map>`__ depend
on the contents of big maps stored in the context. To test them, the
``big_maps`` toplevel primitive can be used to specify the types and
contents of the big maps which are assumed to be present.

The mapping given to the ``big_maps`` toplevel primitive is a
Micheline sequence whose elements have the form ``Big_map <i> <kty>
<vty> { Elt <k1> <v1>; Elt <k2> <v2>; ...}`` where ``<i>`` is an
integer (the identifier of the big map), ``<kty>`` is the comparable
type of keys, ``<vty>`` is the type of values, each ``<ki>`` is of
type ``<kty>`` and each ``<vi>`` is of type ``<vty>``. Each identifier
should appear at most once and the order in which big maps are
specified is irrelevant but each ``{ Elt <k1> <v1>; Elt <k2> <v2>;
...}`` description of big map contents should be given in increasing
order of keys.

The following example tests the `GET
<https://tezos.gitlab.io/michelson-reference/#instr-GET>`__
instruction in the `big_map
<https://tezos.gitlab.io/michelson-reference/#type-big_map>`__ case:

::

   big_maps { Big_map 4 string nat { Elt "bar" 42 } };
   input { Stack_elt (big_map string nat) 4 };
   code { PUSH string "foo"; GET };
   output { Stack_elt (option nat) None }
