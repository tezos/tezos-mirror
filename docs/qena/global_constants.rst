Global Constants
================

The size limit for :doc:`Michelson <michelson>` contracts is quite small, limited to 60
kilobytes as of Granada protocol. Global constants are a feature added
in Hangzhou protocol that enables the re-use of user-defined Micheline chunks in Michelson scripts, allowing
for larger and more complex contracts on the chain. It works in the
following way:

-  Fragments of Michelson code (written in the :doc:`Micheline format <../shell/micheline>`) are
   registered on the chain via a new operation
   ``register_global_constant``. An example expression might be the
   integer ``999`` or the lambda expression ``{ PUSH int 999; ADD }``
-  Included in the receipt of the operation is a hash of the expression
   registered. For example the hash ``999`` is
   ``expruQN5r2umbZVHy6WynYM8f71F8zS4AERz9bugF8UkPBEqrHLuU8``.
-  Constants can be referenced inside a Michelson script with the new
   primitive ``constant``. For example, we could write a lambda
   equivalent to the one above like so:
   ``{ PUSH int (constant "expruQN5r2umbZVHy6WynYM8f71F8zS4AERz9bugF8UkPBEqrHLuU8"); ADD }``

Global Constant Registration
----------------------------

The new ``register_global_constant`` operation includes an object with a
single key ``"value"``, the value of which is the Micheline expression
to be registered.

You can submit this operation conveniently through a new :doc:`octez-client <cli-commands>` command.
For example, the command:

.. code:: sh

    octez-client register global constant "999" from bootstrap1 --burn-cap 0.017

would result in the output:

::

   Node is bootstrapped.
   Estimated gas: 1440 units (will add 100 for safety)
   Estimated storage: 68 bytes added (will add 20 for safety)
   Operation successfully injected in the node.
   Operation hash is 'onsFknW5iWa6eiTYqAghY4peQZ7JYQUJg5fR8MwAQkMKjXfNqGf'
   NOT waiting for the operation to be included.
   Use command
     octez-client wait for onsFknW5iWa6eiTYqAghY4peQZ7JYQUJg5fR8MwAQkMKjXfNqGf to be included --confirmations 5 --branch BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU
   and/or an external block explorer to make sure that it has been included.
   This sequence of operations was run:
     Manager signed operations:
       From: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
       Fee to the baker: ꜩ0.000385
       Expected counter: 1
       Gas limit: 1540
       Storage limit: 88 bytes
       Balance updates:
         tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ................ -ꜩ0.000385
         fees(the baker who will include this operation,0) ... +ꜩ0.000385
       Register Global:
         Value: 999
         This global constant registration was successfully applied
         Balance updates:
           tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.017
         Consumed gas: 1440
         Storage size: 68 bytes
         Global address: expruQN5r2umbZVHy6WynYM8f71F8zS4AERz9bugF8UkPBEqrHLuU8

As you can see, the address of the constant is returned in the operation
receipt in the field ``Global address``. This address is the Base58-encode Blake2b
hash of the binary serialization of the registered Micheline expression.
This means constants are content-addressable - given a particular Micheline
expression, you can always calculate its on-chain address and check if it’s registered.

A few points about registering global constants:

- Global constants may contain references to other constants; however,
  any referenced constants must already be registered on the chain. As a
  corollary, you cannot have cyclic references.
- Global constants are not type-checked before registration - any
  valid Micheline expression may be registered. That said, attempting
  to originate a contract that uses a constant in an ill-typed way will
  fail.
- The total depth of the expression registered as a constant (after
  expanding all constant references) may not exceed 10,000.
- The total number of nodes in the Micheline expression being
  registered (after expanding all constant references) may not exceed
  the ``max_micheline_node_count`` protocol constant. As of Hangzhou
  this is 50,000.
- The total number of bytes in the Micheline expression being
  registered (after expanding all constant references) may not exceed
  the ``max_micheline_bytes_limit`` protocol constant. As of Hangzhou
  this is 50,000.

Originating a Contract that uses Global Constants
-------------------------------------------------

A global constant can be referenced in Michelson scripts via the
primitive ``constant``, which accepts a single string argument, being
the hash of the expression to be referenced at runtime. This primitive
can be used to replace any Micheline node in the bodies of the
``parameter``, ``storage``, ``code``, or ``view`` fields of a Michelson script. For
example, we replace every instance of the type ``lambda unit unit`` and
value 999 with their respective hashes:

.. code-block:: michelson

     parameter (constant "exprtYirrFwYKm6yKLzJNtYRbq49zedYq16BonRvMzHiwSbUekB9YL");
     storage (big_map int (constant "exprtYirrFwYKm6yKLzJNtYRbq49zedYq16BonRvMzHiwSbUekB9YL"));
     code {
       PUSH int (constant "expruQN5r2umbZVHy6WynYM8f71F8zS4AERz9bugF8UkPBEqrHLuU8");
       # <rest of code>
     };

The full expansion of this contract would be:

.. code-block:: michelson

     parameter (lambda unit unit);
     storage (big_map int (lambda unit unit));
     code {
       PUSH int 999;
       # <rest of code>
     };

During origination, all constants are expanded recursively. The
operation will fail if the resulting contract is ill-typed. Global
constant expansion consumes gas; thus, the operation may also fail due
to gas exhaustion.

Global Constants at Runtime
---------------------------

Contracts that use global constants are semantically equivalent to the
contract with all constants expanded.

Note that using the `UNPACK <https://tezos.gitlab.io/michelson-reference/#instr-UNPACK>`__
operation to deserialize a lambda which contains a reference to a global
constant is not supported. Similarly, originating a contract which contains
a reference to a global constant using the
`CREATE_CONTRACT <https://tezos.gitlab.io/michelson-reference/#instr-CREATE_CONTRACT>`__
instruction will also fail.
