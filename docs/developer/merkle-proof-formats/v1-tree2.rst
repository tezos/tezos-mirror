V1 for Tree2
============

Same as :doc:`./v1-tree32`
except `Inode_proofs <#inode-proofs>`__,
`Inode_proofs_of_hash <#inode-proofs-of-hash>`__ and
`Segment <#segment>`__

Tree Proof
----------

====== ======== ==============================
Name   Size     Contents
====== ======== ==============================
length 2 bytes  int16
before 33 bytes `Kinded_hash <#kinded-hash>`__
after  33 bytes `Kinded_hash <#kinded-hash>`__
state  variable `Tree <#tree>`__
====== ======== ==============================

Kinded_hash
~~~~~~~~~~~

==== ======== =================================
Name Size     Contents
==== ======== =================================
tag  1 byte   0x00 for \`Value, 0x01 for \`Node
hash 32 bytes hash
==== ======== =================================

Tree
~~~~

value of tree
^^^^^^^^^^^^^

===== ======== ==================
Name  Size     Contents
===== ======== ==================
tag   1 byte   0x00
value variable `Bytes <#bytes>`__
===== ======== ==================

blinded_value of tree
^^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x01
hash 32 bytes hash
==== ======== ========

node of tree
^^^^^^^^^^^^

=================== ============== ==================================================
Name                Size           Contents
=================== ============== ==================================================
tag                 1 byte         0x02
length              4 bytes        byte length of below list
(step \* tree) list (length) bytes sequence of (`Step <#step>`__ \* `Tree <#tree>`__)
=================== ============== ==================================================

blinded_node of tree
^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x03
hash 32 bytes hash
==== ======== ========

inode of tree
^^^^^^^^^^^^^

====== ======== ================================
Name   Size     Contents
====== ======== ================================
tag    1 byte   0x04
length 8 bytes  uint64
proofs variable `Inode_proofs <#inode-proofs>`__
====== ======== ================================

extender of tree
^^^^^^^^^^^^^^^^

======= ======== ============================
Name    Size     Contents
======= ======== ============================
tag     1 byte   0x05
length  8 bytes  uint64
segment variable `Segment <#segment>`__
proof   variable `Inode_tree <#inode-tree>`__
======= ======== ============================

Inode_tree
~~~~~~~~~~

blinded_inode of inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x00
hash 32 bytes hash
==== ======== ========

inode_values of inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^

=================== ============== ==================================================
Name                Size           Contents
=================== ============== ==================================================
tag                 1 byte         0x01
length              4 bytes        byte length of below list
(step \* tree) list (length) bytes sequence of (`Step <#step>`__ \* `Tree <#tree>`__)
=================== ============== ==================================================

inode_trees of inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^

====== ======== ================================
Name   Size     Contents
====== ======== ================================
tag    1 byte   0x02
length 8 bytes  uint64
proofs variable `Inode_proofs <#inode-proofs>`__
====== ======== ================================

inode_extender of inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

======= ======== ============================
Name    Size     Contents
======= ======== ============================
tag     1 byte   0x03
length  8 bytes  uint64
segment variable `Segment <#segment>`__
proof   variable `Inode_tree <#inode-tree>`__
======= ======== ============================

none of inode_tree
^^^^^^^^^^^^^^^^^^

Used in `Inode_proofs <#inode-proofs>`__
below

==== ====== ========
Name Size   Contents
==== ====== ========
tag  1 byte 0x04
==== ====== ========

Inode_proofs
~~~~~~~~~~~~~~

The number of subtrees of ``Inode`` or ``Inode_trees`` is up to 2

===================== ======== ================================
Name                  Size     Contents 
===================== ======== ================================
(optional) inode_tree variable 0th `Inode_tree <#inode-tree>`__
(optional) inode_tree variable 1st `Inode_tree <#inode-tree>`__
===================== ======== ================================

Stream Proof
------------

======= ============== ==============================
Name    Size           Contents
======= ============== ==============================
version 2 bytes        int16
before  33 bytes       `Kinded_hash <#kinded-hash>`__
after   33 bytes       `Kinded_hash <#kinded-hash>`__
length  4 bytes        byte length of state
state   (length) bytes sequence of `Elt <#elt>`__
======= ============== ==============================

Elt
~~~

value of elt
^^^^^^^^^^^^

===== ======== ==================
Name  Size     Contents
===== ======== ==================
tag   1 byte   0x00
value variable `Bytes <#bytes>`__
===== ======== ==================

node of elt
^^^^^^^^^^^

========================== ============== ================================================================
Name                       Size           Contents
========================== ============== ================================================================
tag                        1 byte         0x01
length                     4 bytes        byte length of below list
(step \* kinded_hash) list (length) bytes sequence of (`Step <#step>`__ \* `Kinded_hash <#kinded-hash>`__)
========================== ============== ================================================================

inode of elt
^^^^^^^^^^^^

====== ======== ================================================
Name   Size     Contents
====== ======== ================================================
tag    1 byte   0x02
length 8 bytes  uint64
proofs variable `Inode_proofs_of_hash <#inode-proofs-of-hash>`__
====== ======== ================================================

inode_extender of elt
^^^^^^^^^^^^^^^^^^^^^

======= ======== ======================
Name    Size     Contents
======= ======== ======================
tag     1 byte   0x03
length  8 bytes  uint64
segment variable `Segment <#segment>`__
hash    32 bytes hash
======= ======== ======================

Inode_proofs_of_hash
~~~~~~~~~~~~~~~~~~~~~~

=============== ======== ======================================
Name            Size     Contents
=============== ======== ======================================
(optional) hash variable 0th `Optional_hash <#optional-hash>`__
(optional) hash variable 1st `Optional_hash <#optional-hash>`__
=============== ======== ======================================

Other Components
----------------

Bytes
~~~~~

======= ============== ========
Name    Size           Contents
======= ============== ========
length  4 bytes        int
content (length) bytes bytes
======= ============== ========

Step
~~~~

======= ============== ========
Name    Size           Contents
======= ============== ========
length  1 byte         < 256
content (length) bytes bytes
======= ============== ========

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

Optional_hash
~~~~~~~~~~~~~

none case
^^^^^^^^^

==== ====== ========
Name Size   Contents
==== ====== ========
tag  1 byte 0x00
==== ====== ========

some case
^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x01
hash 32 bytes hash
==== ======== ========
