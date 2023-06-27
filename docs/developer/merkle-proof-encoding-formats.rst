Merkle Proof Encoding Formats
=============================

A Merkle proof is a datum which demonstrates that a `Merkle tree <https://en.wikipedia.org/wiki/Merkle_tree>`_ has a given value.
Typically a Merkle root and a subtree of a Merkle tree are used as a Merkle proof.
Verification is done by computing the Merkle root and checking it is the same as the given hash.
In Octez, Merkle proofs are used for Optimistic Rollups (see :doc:`../active/smart_rollups`) in the event
an invalid hash is submitted from a layer 2 node to layer 1.
An honest layer 2 node can then present a Merkle proof to demonstrate that the previously submitted hash is in fact fraudulent.

This document shows the encoding format of the Merkle proof implemented in :src:`src/lib_context/merkle_proof_encoding/merkle_proof_encoding.ml`.
There are 2 versions of encodings (defined as V1 and V2), each generating 2 types of
Merkle proofs (named tree_proof and stream_proof), for 2 types of Irmin
Trees (32-tree and binary tree). The data structure is defined in
:src:`src/lib_context/sigs/context.ml` (:package-api:`API <tezos-context/Tezos_context_merkle_proof_encoding/index.html>`) as below.
The internal structure of Irmin, which is used to manage contexts in Octez, appears in it.
Encoding formats give the conversion between a ``tree_proof`` and a byte sequence,
and between a ``stream_proof`` and a byte sequence.


.. code:: ocaml

   type index = int
   type hash = Context_hash.t (* 32-byte Blake2b hash *)

   type kinded_hash =[
     | `Value of hash
     | `Node of hash
   ]

   type 'a inode = {length : int; proofs : (index * 'a) list}

   type 'a inode_extender = {length : int; segment : index list; proof : 'a}

   type tree
     | Value of value
     | Blinded_value of hash
     | Node of (step * tree) list
     | Blinded_node of hash
     | Inode of inode_tree inode
     | Extender of inode_tree inode_extender

   type inode_tree =
     | Blinded_inode of hash
     | Inode_values of (step * tree) list
     | Inode_tree of inode_tree inode
     | Inode_extender of inode_tree inode_extender

   type elt =
     | Value of value
     | Node of (step * kinded_hash) list
     | Inode of hash inode
     | Inode_extender of hash inode_extender

   type stream = elt Seq.t

   type tree_proof = { version : int; before : kinded_hash; after : kinded_hash; state : tree }

   type stream_proof = { version : int; before : kinded_hash; after : kinded_hash; state : stream }

``before`` and ``after`` in both proofs are Merkle roots and denote that
the Merkle root of a tree changed from ``before`` to ``after`` by applying the get/set operations for the tree.
``tree_proof`` is a partial tree containing the hashes of nodes needed for verification and the values the operations refer to.
It omits extra nodes and hashes that are recoverable by re-computing.
``stream proof`` can produce on demand a partial Merkle tree corresponding to a ``tree_proof``.

.. toctree::
   :maxdepth: 1
   :caption: Merkle proof formats

   ./merkle-proof-formats/v1-tree32
   ./merkle-proof-formats/v1-tree2
   ./merkle-proof-formats/v2-tree32
   ./merkle-proof-formats/v2-tree2

Note: all encoding versions use big endian.

Notation Example
----------------

In order to explain the notation used for describing the encodings,
let us consider the ``Bytes`` format as an example (this format is not actually used).
In our notation, the format is defined as in the table below.

+-----------------------+-----------------------+-----------------------+
| Name                  | Size                  | Contents              |
+=======================+=======================+=======================+
| tag                   | 1 byte                | 0b000000yy where yy   |
|                       |                       | is tag for length     |
|                       |                       | (0b00 for p = 1, 0b01 |
|                       |                       | for p = 2, 0b10 for p |
|                       |                       | = 4, 0b11 for p = 8)  |
+-----------------------+-----------------------+-----------------------+
| length                | p bytes               | int                   |
+-----------------------+-----------------------+-----------------------+
| content               | (length) bytes        | bytes                 |
+-----------------------+-----------------------+-----------------------+

This format means that given a byte sequence ``b``, the first 1
byte of ``b`` represents the tag which determines the number of bytes ``p``
of the next record ``length``; the next ``p`` bytes of ``b`` represent the length of
the content as an integer, and the following (length) bytes represent the
content.

``0b000000yy`` in the description of ``tag`` means that the first 6 bits of
the tag must be ``0``, and we represent “yy” as the last 2 bits of the
tag.

For example, when ``b`` is ``0x000c48656c6c6f20576f726c6421``, ``tag`` is ``0x00``
(so the byte length of ``length`` is 1), ``length`` is ``0x0c`` (so the
byte length of ``content`` is 12) and ``content`` is
``0x48656c6c6f20576f726c6421``

See also
--------

Another implementation of the encoder for the Merkle proofs, written in the Rust language,
is available `here <https://gitlab.com/dailambda/merkle-proof-encoder/>`__.
