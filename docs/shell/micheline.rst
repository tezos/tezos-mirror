Micheline
=========

Micheline is a data format comparable to JSON, XML, S-expressions, and
YAML. Its main purpose is to serve as the concrete syntax for :doc:`the
Michelson language <../active/michelson>`.

Syntax
------

The structure of a Micheline node is very simple: a node can only be
one of the five following constructs:

1. An integer in decimal notation.
2. A character string delimited by the double quotation character ``"``.
3. A byte sequence in hexadecimal notation prefixed by ``0x``.
4. The application of a primitive to a whitespace-delimited list of
   nodes and annotations.
5. A sequence of nodes delimited by curly braces (``{`` and
   ``}``) and separated by semi-colons (``;``).

Primitives
~~~~~~~~~~

Micheline primitives are character strings containing only
alphanumeric or underscore (``_``) characters. Additional restrictions
on primitives can also be put to achieve the equivalent of JSON and
XML schemas; for example each protocol specifies a fixed list of
Michelson primitives. This restriction impacts the way Micheline nodes
are serialized to binary format (see `Binary serialization`_).

Annotations
~~~~~~~~~~~

Micheline annotations start by one of the special characters ``@``,
``:``, ``$``, ``&``, ``%``, ``!``, and ``?`` followed by any number of
alphanumeric, underscore (``_``), period (``.``), percent sign
(``%``), and at sign (``@``) characters.

Character strings
~~~~~~~~~~~~~~~~~

The following usual escape sequences are allowed inside strings:
``\n``, ``\\``, ``\"``. Unescaped line-breaks (both ``\n`` and ``\r``)
cannot appear in Micheline strings. Since Micheline strings are
delimited by double quotes, the double quote character cannot appear
unescaped inside a string.

Primitive applications
~~~~~~~~~~~~~~~~~~~~~~

A primitive application is a primitive followed by zero, one, or
several arguments (either nodes or annotations)::

    prim arg1 arg2

When a primitive application with more than 0 arguments is the
argument of another primitive application, it must be wrapped with
parentheses. This wrapping is also allowed for applications without
arguments.

::

    prim (prim1 arg11 arg12) (prim2 arg21 arg22)

Sequences
~~~~~~~~~

Successive nodes can be grouped as a single sequence node using curly
braces as delimiters and semicolon as separators.

::

    { expr1 ; expr2 ; expr3 ; expr4 }

A sequence can be passed as argument to a primitive.

::

    prim arg1 arg2 { arg3_expr1 ; arg3_expr2 }

Primitive applications right inside a sequence cannot be wrapped.

::

    { (prim arg1 arg2) } # is not ok

Indentation
~~~~~~~~~~~

To remove ambiguities for human readers, the Micheline parser enforces
some indentation rules.

-  For sequences:

   -  All nodes in a sequence must be aligned on the same column.
   -  An exception is made when consecutive nodes fit on the same
      line, as long as the first of them is correctly aligned.
   -  All nodes in a sequence must be indented to the right of the
      opening curly brace by at least one column.
   -  The closing curly brace cannot be on the left of the opening one.

-  For primitive applications:

   -  All arguments in an application must be aligned on the same
      column.
   -  An exception is made when consecutive arguments fit on the same
      line, as long as the first of them is correctly aligned.
   -  All arguments in a sequence must be indented to the right of the
      primitive name by at least one column.

Comments
~~~~~~~~

A hash sign (``#``) anywhere outside of a string literal will make the
rest of the line (and itself) completely ignored, as in the following
example.

::

    { PUSH nat 1 ; # pushes 1
      PUSH nat 2 ; # pushes 2
      ADD }        # computes 2 + 1

Comments that span on multiple lines or that stop before the end of the
line can also be written, using C-like delimiters (``/* ... */``).

File encoding
~~~~~~~~~~~~~

Micheline files must be encoded in UTF-8. Non-ASCII characters can
only appear in comments and strings.

BNF grammar
~~~~~~~~~~~

Formally, Micheline nodes have the following BNF grammar::

  <natural> ::= [0-9]+
  <char> ::= \" | \r | \n | \t | \b | \\ | [^"\]
  <hexadecimal> ::= [0-9a-fA-F][0-9a-fA-F]
  <primitive> ::= [a-zA-Z_0-9]+
  <annotation> ::= [@:$&%!?][_0-9a-zA-Z\.%@]*

  <node> ::= <integer> | <string> | <bytes> | <primitive application> | <sequence>
  <integer> ::= <natural> | - <natural>
  <string> ::= " <char>* "
  <bytes> ::= 0x<hexadecimal>*
  <primitive application> ::= <primitive> <argument>*
  <argument> ::= <integer> | <string> | <bytes> | <primitive> | ( <primitive application> ) | <sequence> | <annotation>
  <sequence> ::= { nodes }
  <nodes> ::= | <node> | <node> ; | <node> ; <nodes>


Serialization
-------------

Micheline nodes can be converted to JSON and binary formats. The JSON
conversion is useful for example to interact with a Tezos node using
RPCs. The binary encoding is used to store Micheline nodes in blocks.

Both forms of serialization are achieved using :doc:`../developer/data_encoding`.

Locations
~~~~~~~~~

Locations are used in error messages to indicate which part of a
Micheline file is faulty. Micheline file locations are composed of two
points; a starting point and a stopping point. Each point is a pair of
a line number (counted from 1) and a column number (counted from 0).

Locations only make sense when we have access to the source file and
they are lost when serializing Micheline nodes to either JSON or
binary. During translation, locations are replaced by a *canonical*
representation of locations. A canonical location is a single integer;
the root of the node has a canonical location of zero, and each node
adds one in the order of infix traversal.

A mapping between file locations and canonical locations is produced
during the serialization so that errors reported by tools working on
the serialized form can be printed in a meaningful way to users.

.. _micheline_json:

Conversion to JSON
~~~~~~~~~~~~~~~~~~

The JSON conversion is very straightforward:

- integers are encoded as JSON objects with a single ``"int"`` field
  whose value is a JSON string;
- strings are encoded as JSON objects with a single ``"string"`` field
  whose value is either a JSON string or an array of bytes if the
  string to encode is not valid in JSON;
- byte sequences are encoded as JSON objects with a single "bytes"
  field whose value is a JSON string containing the hexadecimal representation of the byte sequence without the ``0x`` prefix.
- sequences are encoded as JSON arrays;
- primitive applications are encoded as JSON objects with three fields
  named ``"prim"``, ``"args"``, and ``"annots"`` containing
  respectively the primitive name, the JSON array of argument nodes,
  and the JSON array of annotations. Both ``"args"`` and ``"annots"``
  are optional and default to the empty array.

.. _micheline_bin:

Binary serialization
~~~~~~~~~~~~~~~~~~~~

The binary serialization of Micheline is more complex because it
contains some space optimisations for the very common cases of
primitive applications with few arguments. Moreover, the binary
encoding depends on the way primitives are encoded.

The documentation for the binary encoding specialized to Michelson
primitives of the Alpha protocol can be generated by the command
``octez-codec describe alpha.script.expr binary schema``.

Usage of the OCaml Micheline library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the Octez codebase, Micheline nodes are handled by a small library
named ``lib_micheline``. This library is used in the Octez client
(responsible for parsing the Michelson files, expanding macros, and
encoding the result to JSON) and in the Tezos protocol (responsible
for decoding from JSON, type checking, and serializing to binary
Michelson scripts and values).

The library is composed of three modules: ``Micheline``,
``Micheline_parser``, and ``Micheline_printer``. Only the
``Micheline`` module is needed for the protocol so its signature is
the only one exported in the protocol environment.

The ``Micheline`` module defines the type of Micheline nodes. This
type is parameterised by the type of locations ``'l`` and the type of
primitives ``'p``.

::

  type annot = string list

  type ('l, 'p) node =
    | Int of 'l * Z.t
    | String of 'l * string
    | Bytes of 'l * Bytes.t
    | Prim of 'l * 'p * ('l, 'p) node list * annot
    | Seq of 'l * ('l, 'p) node list


Since printing and parsing are provided by the two other modules of
the library, the only way to produce or consume Micheline nodes from
this module is through the encodings.  To encode a Micheline node, we
first canonicalize its locations using the ``extract_locations`` function
that takes a ``('l, 'p) node`` as argument and returns both a ``'p
canonical`` and an association list between ``canonical_location``\ s
and ``'l`` locations. The type ``'p canonical`` is an abstract version
of ``(canonical_location, 'p) node`` that ensures the invariant on the
numbering of canonical locations presented in the `Locations`_ section
and ``canonical_location`` is an alias of ``int``.

If we know an encoding for the ``'p`` primitives, we can derive an
encoding for ``'p canonical`` using the ``canonical_encoding``
function and then produce JSON and binary outputs using the functions
from :doc:`../developer/data_encoding`.

In the other direction, from a ``'p canonical`` produced by an
encoding, either we know how to map its canonical locations to file
locations and we can use the ``inject_locations`` function to do so or
we want to keep the locations canonical and use the ``root`` function
to obtain a plain node.


The ``Micheline_printer`` module exports two pretty-printing functions
based on the ``Format`` standard OCaml module: ``print_expr`` and
``print_expr_unwrapped``. The difference is that ``print_expr`` wraps
its output in parentheses if it is a primitive application with at
least one argument. The input of these printing functions can be
produced from a ``'p canonical`` by the ``printable`` function.


The ``Micheline_parser`` module can be used to produce Micheline nodes
from strings and files. It is done in two stages: tokenization (also
known as lexing) and parsing. Tokenization is done by the ``tokenize``
function that uses the ``uutf`` OCaml library to decode UTF-8
characters. For the parsing phase, two functions ``parse_expression``
and ``parse_toplevel`` are exported; the former produces a single
Micheline node while the second parses a list of expressions separated
by semicolons. They both take an optional boolean parameter named
``check``; setting it to ``false`` disables indentation checking.

Other tools and resources
-------------------------

The following links are not part of the Octez OCaml code base but are
reimplementations of parts of ``lib_micheline`` in other tools and
languages:

- https://medium.com/the-cryptonomic-aperiodical/the-magic-and-mystery-of-the-micheline-binary-format-33bf85699bef
  describes the binary serialization of Micheline nodes. This
  documents the ConseilJS library but assumes no familiarity with it.

- A Micheline parser written using the Menhir parser generator is
  available as part of the Mi-Cho-Coq framework:
  https://gitlab.com/nomadic-labs/mi-cho-coq/-/blob/master/src/michocoq/micheline_parser.vy
  it is incomplete as it does not support annotations.

- An online converter between Micheline files and their JSON encoding
  (improperly called "Michelson")
  developed as part of Cryptonomic hackathon and documented here:
  https://scalac.io/blog/we-hakked-tezos-and-created-a-micheline-michelson-translator/
