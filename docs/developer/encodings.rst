Encodings
=========

Throughout the Tezos protocol, data is serialized so that it can be used via RPC, written to disk, or placed in a block.
Conversely, data is deserialized to in-memory data structures, for performing operations on it, such as checking the validity of a transaction.
These serialization and deserialization operations rely on *encodings*, that is, data encoding rules for the various Tezos data structures, towards/from several forms.
Thus, there exists binary encodings and JSON encodings.

Tools
-----

For performing serialization/deserialization of data structures into binary form within OCaml code, refer to the documentation of the :doc:`data encoding library <data_encoding>`.

For studying and understanding all the different encodings, the definitive source of truth is the ``tezos-codec`` tool.
It is a command-line tool allowing users and developers to:

- describe the binary and JSON encoding schemas for all the supported data structures (see :ref:`codec_describe`)
- encode and decode data into/from binary or JSON form, using a specific encoding (see :ref:`codec_encode`)

You may refer to the ``tezos-codec`` :ref:`online manual <codec_manual>` for more details.
The rest of this page gives a gentle introduction to this tool by showing on some examples how to perform the two tasks above.

Note that for the particular case of Micheline expressions, the ``tezos-client`` tool can also be used to convert between several data representations, covering not only the binary and JSON representations, but also OCaml and Michelson notations (see :ref:`client_convert`).

.. _codec_describe:

How to read the output of ``tezos-codec``
-----------------------------------------

The list of data structures that can be encoded/decoded may be obtained as follows::

  $ tezos-codec list encodings

The binary encoding of any supported data structure can be described using the following command::

  $ tezos-codec describe <id> binary schema

Similarly, the JSON encoding of any supported data structure can be described using the following command::

  $ tezos-codec describe <id> json schema

However, the output of the above commands is rather verbose, so here is a short introduction for how to read the schemas produced by ``tezos-codec``.

JSON schemas
~~~~~~~~~~~~

The descriptions of JSON schemas are rather self-explaining.
For instance, the encoding of a Micheline expression is done according to a few simple :ref:`JSON encoding principles <micheline_json>`, that can be retrieved by doing::

  $ tezos-codec describe alpha.script.expr json schema

The schema output by this command is as follows (slightly abbreviated and reformatted for better readability):

.. code-block::

  $alpha.script.expr
  $alpha.michelson.v1.primitives:
    "ADD"
    | "IF_LEFT"
    | "SELF_ADDRESS"
    ...
    | "code"
  $alpha.script.expr:
    { "int": $bignum } /* Int */
    || { "string": $unistring } /* String */
    || { "bytes": /^[a-zA-Z0-9]+$/ } /* Bytes */
    || [ $micheline.alpha.michelson_v1.expression ... ] /* Sequence */
    || { /* Generic prim (any number of args with or without annot) */
         "prim": $alpha.michelson.v1.primitives,
         "args"?: [ $micheline.alpha.michelson_v1.expression ... ],
         "annots"?: [ string ... ] }
  $bignum:
    /* Big number: Decimal representation of a big number */
    string
  $micheline.alpha.michelson_v1.expression:
    ...
  $unistring:
    /* Universal string representation:
       Either a plain UTF8 string, or a sequence of bytes for strings that
       contain invalid byte sequences. */
    string || { "invalid_utf8_string": [ integer ∈ [0, 255] ... ] }

The schema starts with the main non-terminal (here, a Micheline expression, denoted by ``$alpha.script.expr``), whose definition appears lower in the schema.
The schema also defines all the non-terminals which are used, directly or indirectly, in this definition.
Note that we omitted in the listing above the definition of non-terminal ``$micheline.alpha.michelson_v1.expression``, because it is identical to that of the main non-terminal ``$alpha.script.expr``.

As can be seen, non-terminals are defined as disjunctions of JSON elements such as constants, objects, and arrays.
Some attached comments further clarify the meaning of most alternatives or fields.

Binary schemas
~~~~~~~~~~~~~~

The descriptions of binary schemas are more complex to some extent, mainly for two reasons:

- Binary schemas are lower level than the JSON schemas.
  Thus, the encoding of elementary types has to be precisely defined: strings must include a field containing their length; discriminated unions must include a field containing a tag, whose possible values must be enumerated; the precise binary layout of various integer types must be made explicit, and so on.
- The binary encodings are optimized for certain common cases, in order to save space.
  For instance, Micheline primitive applications with one or two arguments uses specialized encodings that are more compact (see the :ref:`binary encoding principles <micheline_bin>` for Micheline).

To illustrate these differences, let us consider the same example as above, that of a Micheline expression::

  $ tezos-codec describe alpha.script.expr binary schema

The binary schema produced by this command is as follows (abbreviated and reformatted for better readability)::

  +-----------------+----------------------+----------+
  | Name            | Size                 | Contents |
  +=================+======================+==========+
  | Unnamed field 0 | Determined from data | $X_8     |
  +-----------------+----------------------+----------+


  Z.t
  ***
  A variable length sequence of bytes, encoding a Zarith number. ...
  +------+----------------------+----------+
  | Name | Size                 | Contents |
  +======+======================+==========+
  | Z.t  | Determined from data | bytes    |
  +------+----------------------+----------+

  micheline.alpha.michelson_v1.expression (Determined from data, 8-bit tag)
  *************************************************************************

  Int (tag 0)
  ===========
  +------+----------------------+------------------------+
  | Name | Size                 | Contents               |
  +======+======================+========================+
  | Tag  | 1 byte               | unsigned 8-bit integer |
  +------+----------------------+------------------------+
  | int  | Determined from data | $Z.t                   |
  +------+----------------------+------------------------+

  String (tag 1)
  ==============
  +-----------------------+----------+-------------------------+
  | Name                  | Size     | Contents                |
  +=======================+==========+=========================+
  | Tag                   | 1 byte   | unsigned 8-bit integer  |
  +-----------------------+----------+-------------------------+
  | # bytes in next field | 4 bytes  | unsigned 30-bit integer |
  +-----------------------+----------+-------------------------+
  | string                | Variable | bytes                   |
  +-----------------------+----------+-------------------------+

  Sequence (tag 2)
  ================
  ...

  Prim (no args, annot) (tag 3)
  =============================
  ...

  Prim (no args + annot) (tag 4)
  ==============================
  ...

  Generic prim (any number of args with or without annot) (tag 9)
  ===============================================================
  ...

  Bytes (tag 10)
  ==============
  ...

  alpha.michelson.v1.primitives (Enumeration: unsigned 8-bit integer):
  ********************************************************************
  +-------------+-----------------------+
  | Case number | Encoded string        |
  +=============+=======================+
  | 0           | parameter             |
  +-------------+-----------------------+
  | 1           | storage               |
  +-------------+-----------------------+
  | 2           | code                  |
  +-------------+-----------------------+
  ...
  +-------------+-----------------------+
  | 140         | GET_AND_UPDATE        |
  +-------------+-----------------------+

The binary schema starts with the binary layout of the main non-terminal (here, ``alpha.script.expr``), and also defines the other non-terminals that are used, directly or indirectly in this definition.
Each definition forms a section (whose heading is underlined by all-"*" lines).
Sections corresponding to disjunctions are further structured in subsections (whose headings are underlined by all-"=" lines), one for each possible value of the discriminating tag.

For instance:

- The layout of an ``Int`` as a "bignum" is explicitly defined as a Zarith number (non-terminal ``Z.t``).
- The layout of a ``String`` starts with a field containing the number of bytes in the string.
- The values of the discriminating tag are 0 for ``Int`` expressions, 1 for ``String`` expressions, and so on.
- The encoding of expressions involving a primitive operator application defines both the generic case of an arbitrary number of operators (the same as in the JSON schema above), and a number of specialized common cases (zero operator with or without annotations, etc.).
- The operators themselves are encoded as an enumeration of values (non-terminal ``alpha.michelson.v1.primitives``).

.. _codec_encode:

How to encode/decode values
---------------------------

Beyond examining the various available encodings, the ``tezos-codec`` tool can also be used to encode and decode data.
This can be useful for developers when debugging, but also for end users when trying to understand the contents of a block or transaction, for instance.

Let us consider a few examples of encoding and decoding some commonly used types.

Strings
~~~~~~~

To encode a string as a Micheline expression, proceed as follows::

  $ tezos-codec encode alpha.script.expr from '{"string":"Hello world!"}'
  010000000c48656c6c6f20776f726c6421

As can be seen, strings are serialized as follows:

- a leading ``01`` tag to indicate type string
- four bytes (eight hex chars) to indicate the length of the string: ``0000000c = 0x0c = 12`` in our case
- the string represented by its ASCII values: ``48656c6c6f20576f726c6421`` in our case

The same tool can be used in the other direction, to decode a byte sequence representing a serialized string expression::

  $ tezos-codec decode alpha.script.expr from '010000000c48656c6c6f20776f726c6421'
  { "string": "Hello world!" }

Integers
~~~~~~~~

There are various encoding for integers, including:

- ``ground.int16``: Signed 16 bit integers
- ``ground.uint16``: Unsigned 16 bit integers
- ``ground.Z``: Arbitrary precision integers

which can be detailed by describing their schemas, e.g.::

  $ tezos-codec describe ground.Z binary schema
  ...
  A variable length sequence of bytes, encoding a Zarith number. Each byte has
  a running unary size bit: the most significant bit of each byte tells is this
  is the last byte in the sequence (0) or if there is more to read (1). The
  second most significant bit of the first byte is reserved for the sign
  (positive if zero). Size and sign bits ignored, data is then the binary
  representation of the absolute value of the number in little endian order.

To illustrate the Zarith representation, let us encode the Micheline representation of the number ``1,000,000`` (one million)::

  $ tezos-codec encode alpha.script.expr from '{"int":"1000000"}'
  0080897a

Here:

- The first byte ``00`` indicates that the type is integer.
- The number is represented by the bytes ``80897a`` for 1000000 (1 million).

Reading each byte from left to right, in binary form::

  0x80897a = 0b10000000, 0b10001001, 0b01111010

- The first bit in each byte indicates whether it is the last byte (0) in the sequence or if there is more to read (1).
- The second bit in the first byte indicates that this is a positive number.
- The remaining bits are then ``0b000000``, ``0b0001001``, ``0b1111010``.
  Reversing the byte order (because little-endian) we get: ``0b11110100001001000000`` = ``0xf4240`` = ``1000000``.

Pairs
~~~~~

Let us see how an OCaml pair is encoded::

  $ tezos-codec encode alpha.script.expr from '{"prim":"Pair","args":[{"int":"1"},{"int":"2"}]}'
  070700010002

Here:

- ``07``: the first tag denotes the micheline constructor. ``Pair 1 2`` is a primitive application with 2 arguments and no annotation. The corresponding tag is ``0x07``.
- ``07``: the next tag denotes the Michelson primitive ``Pair``. It so happens that the corresponding tag is also ``0x07``.
- ``0001``: encoding of the integer 1
- ``0002``: encoding of the integer 2

Let's try another example, the encoding of the value ``Left 1`` of type ``or nat bool``::

  $ tezos-codec encode alpha.script.expr from '{"prim":"Left","args":[{"int":"1"}]}'
  05050001

Here:

- ``05``: the expression ``Left 1`` is a primitive application with one argument and no annotations. The corresponding tag is ``0x05``.
- ``05``: the michelson primitive is ``Left``, for which the corresponding tag is also ``0x05``.
- ``0001``: encoding of the integer 1.

Operations
~~~~~~~~~~

Finally, let us consider a more complex example.
Assume that we try to understand an operation included in a block.
We can decode the binary string as follows::

  $ tezos-codec decode alpha.operation from '008f1d96e2783258ff663f03dacfe946c026a5d194c73d1987b3da73fadea7d46c008cb5baedee4dc3ec261dfcf57a9600bb0a8e26c0f00bdd85a0018452ac02e0a712000153957451d3cc83a71e26b65ea2391a1b16713d2d009595facf847a72b4c3fe231c0e4185e68e9b2875aa3c639382c86bcf0af23699f47fe66a6550ade936a5b59d5919ad20703885750314e0c368b277de39e7d10a'
  { "branch": "BKiXcfN1ZTXnNNbTWSRArSWzVFc6om7radWq5mTqGX6rY4P2Uhe",
    "contents":
      [ { "kind": "transaction",
          "source": "tz1YU2zoyCkXPKEA4jknSpCpMs7yUndVNe3S", "fee": "1520",
          "counter": "2622173", "gas_limit": "10500", "storage_limit": "300",
          "amount": "300000",
          "destination": "tz2FwBnXhuXvPAUcr1aF3uX84Z6JELxrdYxD" } ],
    "signature":
      "sighZMqWz5G8drK1VTsmTnQBFEQ9kxQQxL88NFh8UaqDEJ3R3mzgR3g81azadZ9saPwsWga3kEPsyfbzrXm6ueuDvx3pQ5Q9" }

In order to understand how the transaction has been decoded from the binary sequence, we have to examine the encoding schema of a block operation::

  $ tezos-codec describe alpha.operation binary schema
  +-----------+----------+---------------------------------------------+
  | Name      | Size     | Contents                                    |
  +===========+==========+=============================================+
  | branch    | 32 bytes | bytes                                       |
  +-----------+----------+---------------------------------------------+
  | contents  | Variable | sequence of $alpha.operation.alpha.contents |
  +-----------+----------+---------------------------------------------+
  | signature | 64 bytes | bytes                                       |
  +-----------+----------+---------------------------------------------+
  ...
  alpha.operation.alpha.contents (Determined from data, 8-bit tag)
  ****************************************************************
  ...
  Transaction (tag 108)
  =====================
  +----------------------------------+----------------------+-------------------------------------+
  | Name                             | Size                 | Contents                            |
  +==================================+======================+=====================================+
  | Tag                              | 1 byte               | unsigned 8-bit integer              |
  +----------------------------------+----------------------+-------------------------------------+
  | source                           | 21 bytes             | $public_key_hash                    |
  +----------------------------------+----------------------+-------------------------------------+
  | fee                              | Determined from data | $N.t                                |
  +----------------------------------+----------------------+-------------------------------------+
  | counter                          | Determined from data | $N.t                                |
  +----------------------------------+----------------------+-------------------------------------+
  | gas_limit                        | Determined from data | $N.t                                |
  +----------------------------------+----------------------+-------------------------------------+
  | storage_limit                    | Determined from data | $N.t                                |
  +----------------------------------+----------------------+-------------------------------------+
  | amount                           | Determined from data | $N.t                                |
  +----------------------------------+----------------------+-------------------------------------+
  | destination                      | 22 bytes             | $alpha.contract_id                  |
  +----------------------------------+----------------------+-------------------------------------+
  | ? presence of field "parameters" | 1 byte               | boolean (0 for false, 255 for true) |
  +----------------------------------+----------------------+-------------------------------------+
  | parameters                       | Determined from data | $X_0                                |
  +----------------------------------+----------------------+-------------------------------------+

Using the above information, the sample binary sequence can be broken down as follows::

  branch
  = 0x008f1d96e2783258ff663f03dacfe946c026a5d194c73d1987b3da73fadea7d4
  = BKiXcfN1ZTXnNNbTWSRArSWzVFc6om7radWq5mTqGX6rY4P2Uhe

  tag = 0x6c = 108 (transaction)

  source
  = 0x008cb5baedee4dc3ec261dfcf57a9600bb0a8e26c0
  = tz1YU2zoyCkXPKEA4jknSpCpMs7yUndVNe3S

  fee = 0xf00b = 1520
  counter = 0xdd85a001 = 2622173
  gas_limit = 0x8452 = 10500
  storage_limit = 0xac02 = 300
  amount = 0xe0a712 = 300000

  destination
  = 0x000153957451d3cc83a71e26b65ea2391a1b16713d2d
  = tz2FwBnXhuXvPAUcr1aF3uX84Z6JELxrdYxD

  has_parameters = 0x00 = false

  signature
  = 0x9595facf847a72b4c3fe231c0e4185e68e9b2875aa3c639382c86bcf0af23699f47fe66a6550ade936a5b59d5919ad20703885750314e0c368b277de39e7d10a
  = sighZMqWz5G8drK1VTsmTnQBFEQ9kxQQxL88NFh8UaqDEJ3R3mzgR3g81azadZ9saPwsWga3kEPsyfbzrXm6ueuDvx3pQ5Q9

As usual, ``tezos-codec`` can be used the other way around, to encode the same transaction::

  $ tezos-codec encode alpha.operation from '{ "branch": "BKiXcfN1ZTXnNNbTWSRArSWzVFc6om7radWq5mTqGX6rY4P2Uhe", "contents": [ { "kind": "transaction", "source": "tz1YU2zoyCkXPKEA4jknSpCpMs7yUndVNe3S", "fee": "1520", "counter": "2622173", "gas_limit": "10500", "storage_limit": "300", "amount": "300000", "destination": "tz2FwBnXhuXvPAUcr1aF3uX84Z6JELxrdYxD" } ], "signature": "sighZMqWz5G8drK1VTsmTnQBFEQ9kxQQxL88NFh8UaqDEJ3R3mzgR3g81azadZ9saPwsWga3kEPsyfbzrXm6ueuDvx3pQ5Q9" }'

.. COMMENT:
  We could also very well publish encodings, as this has proved useful to guiding developers in the past.

.. _client_convert:

How to convert Micheline with ``tezos-client``
----------------------------------------------

The ``tezos-client`` can be used to convert Micheline expressions between the following forms: binary, JSON, Michelson, and OCaml.

Note that the client has to be run in conjunction to a running node for the following commands to work (unless option ``--protocol`` is specified)::

  $ tezos-client convert data '(Pair 1 2)' from michelson to binary
  0x070700010002
  $ tezos-client convert data 0x070700010002 from binary to michelson
  (Pair 1 2)
  $ tezos-client convert data 0x070700010002 from binary to json
  { "prim": "Pair", "args": [ { "int": "1" }, { "int": "2" } ] }
  $ tezos-client convert data 0x070700010002 from binary to ocaml
  Prim (0, D_Pair, [Int (1, Z.one); Int (2, Z.of_int 2)], [])
