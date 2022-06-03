V1 for Tree2
============

Same as :doc:`./v1-tree32`
except `Inode_proofs <#inode-proofs>`__,
`Inode_proofs_of_hash <#inode-proofs-of-hash>`__ and
`Segment <#segment>`__

Tree Proof
------------

Inode_proofs
~~~~~~~~~~~~~~

The number of subtrees of ``Inode`` or ``Inode_trees`` is up to 2

===================== ======== ================================
Name                  Size     Contents 
===================== ======== ================================
(optional) inode_tree variable 0th :ref:`Inode_tree <v1-tree32-inode-tree>`
(optional) inode_tree variable 1st :ref:`Inode_tree <v1-tree32-inode-tree>`
===================== ======== ================================

Stream Proof
------------

Inode_proofs_of_hash
~~~~~~~~~~~~~~~~~~~~~~

=============== ======== ======================================
Name            Size     Contents
=============== ======== ======================================
(optional) hash variable 0th :ref:`Optional_hash <v1-tree32-optional-hash>`
(optional) hash variable 1st :ref:`Optional_hash <v1-tree32-optional-hash>`
=============== ======== ======================================

Other Components
----------------

Segment
~~~~~~~~~

-  The segment int is in 1 bit
-  10\* is filled at the end of the bytes
-  ``n`` segments need ``(n+8)/8`` bytes

::

   ex: Encoding of [a; b; c; d; e; ..; z]

   |76543210|7654.. ..       |76543210|
   |abcdefgh|ijkl.. ..        vwxyz100|

   |76543210|7654.. ..  43210|76543210|
   |abcdefgh|ijkl.. ..  uvwxy|z1000000|

   |76543210|7654.. .. 543210|76543210|
   |abcdefgh|ijkl.. .. uvwxyz|10000000|

======= ============== ==============================
Name    Size           Contents
======= ============== ==============================
length  1 byte         < 256
content (length) bytes 1bit integers with termination
======= ============== ==============================
