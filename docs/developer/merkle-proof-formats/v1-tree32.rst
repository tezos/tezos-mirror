V1 for Tree32
=============

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

Value of Tree
^^^^^^^^^^^^^

===== ======== ==================
Name  Size     Contents
===== ======== ==================
tag   1 byte   0x00
value variable `Bytes <#bytes>`__
===== ======== ==================

Blinded_value of Tree
^^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x01
hash 32 bytes hash
==== ======== ========

Node of Tree
^^^^^^^^^^^^

=================== ============== ==================================================
Name                Size           Contents
=================== ============== ==================================================
tag                 1 byte         0x02
length              4 bytes        byte length of below list
(step \* tree) list (length) bytes sequence of (`Step <#step>`__ \* `Tree <#tree>`__)
=================== ============== ==================================================

Blinded_node of Tree
^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x03
hash 32 bytes hash
==== ======== ========

Inode of Tree
^^^^^^^^^^^^^

====== ======== ================================
Name   Size     Contents
====== ======== ================================
tag    1 byte   0x04
length 8 bytes  uint64
proofs variable `Inode_proofs <#inode-proofs>`__
====== ======== ================================

Extender of Tree
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

Blinded_inode of Inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x00
hash 32 bytes hash
==== ======== ========

Inode_values of Inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^

=================== ============== ==================================================
Name                Size           Contents
=================== ============== ==================================================
tag                 1 byte         0x01
length              4 bytes        byte length of below list
(step \* tree) list (length) bytes sequence of (`Step <#step>`__ \* `Tree <#tree>`__)
=================== ============== ==================================================

Inode_trees of Inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^

====== ======== ================================
Name   Size     Contents
====== ======== ================================
tag    1 byte   0x02
length 8 bytes  uint64
proofs variable `Inode_proofs <#inode-proofs>`__
====== ======== ================================

Inode_extender of Inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

======= ======== ============================
Name    Size     Contents
======= ======== ============================
tag     1 byte   0x03
length  8 bytes  uint64
segment variable `Segment <#segment>`__
proof   variable `Inode_tree <#inode-tree>`__
======= ======== ============================

None of Inode_tree
^^^^^^^^^^^^^^^^^^

Used in `Dense Case of Inode_proofs <#dense-case-of-inode-proofs>`__
below

==== ====== ========
Name Size   Contents
==== ====== ========
tag  1 byte 0x04
==== ====== ========

Inode_proofs
~~~~~~~~~~~~

When Inode_proofs has less than 16 trees, Sparse Case is selected.
Otherwise, Dense Case is selected.

Sparse Case of Inode_proofs
^^^^^^^^^^^^^^^^^^^^^^^^^^^

========================== ============== ===================================================
Name                       Size           Contents
========================== ============== ===================================================
tag                        1 byte         0x00
length                     4 bytes        byte length of below list
(index \* inode_tree) list (length) bytes sequense of (uint8 \* `inode-tree <#inode-tree>`__)
========================== ============== ===================================================

Dense Case of Inode_proofs
^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------+-----------------------+-----------------------+
| Name                  | Size                  | Contents              |
+=======================+=======================+=======================+
| tag                   | 1 byte                | 0x01                  |
+-----------------------+-----------------------+-----------------------+
| (optional) inode_tree | variable              | sequence of           |
| list                  |                       | `Inode_tree <#inode-t |
|                       |                       | ree>`__               |
|                       |                       | (the number of        |
|                       |                       | inode_tree is 32)     |
+-----------------------+-----------------------+-----------------------+

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

Value of Elt
^^^^^^^^^^^^

===== ======== ==================
Name  Size     Contents
===== ======== ==================
tag   1 byte   0x00
value variable `Bytes <#bytes>`__
===== ======== ==================

Node of Elt
^^^^^^^^^^^

========================== ============== ================================================================
Name                       Size           Contents
========================== ============== ================================================================
tag                        1 byte         0x01
length                     4 bytes        byte length of below list
(step \* kinded_hash) list (length) bytes sequence of (`Step <#step>`__ \* `Kinded_hash <#kinded-hash>`__)
========================== ============== ================================================================

Inode of Elt
^^^^^^^^^^^^

====== ======== ================================================
Name   Size     Contents
====== ======== ================================================
tag    1 byte   0x02
length 8 bytes  uint64
proofs variable `Inode_proofs_of_hash <#inode-proofs-of-hash>`__
====== ======== ================================================

Inode_extender of Elt
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
~~~~~~~~~~~~~~~~~~~~

When Inode_proofs_of_hash has less than 16 trees, Sparse Case is
selected. Otherwise, Dense Case is selected.

Sparse Case of Inode_proofs_of_hash
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

==================== ============== =========================================================
Name                 Size           Contents
==================== ============== =========================================================
tag                  1 byte         0x00
length               4 bytes        byte length of below list
(index \* hash) list (length) bytes sequense of (uint8 \* `Optional_hash <#optional-hash>`__)
==================== ============== =========================================================

Dense Case of Inode_proofs_hash
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------+-----------------------+-----------------------+
| Name                  | Size                  | Contents              |
+=======================+=======================+=======================+
| tag                   | 1 byte                | 0x01                  |
+-----------------------+-----------------------+-----------------------+
| (optional) hash list  | variable              | sequence of           |
|                       |                       | `Optional_hash <#opti |
|                       |                       | onal-hash>`__         |
|                       |                       | (the number of hashes |
|                       |                       | is 32)                |
+-----------------------+-----------------------+-----------------------+

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
~~~~~~~

-  The segment int is in 5 bits
-  10\* is filled at the end of the bytes
-  ``n`` segments need ``(n*5+8)/8`` bytes

::

   ex: Encoding of [aaaaa; bbbbb; ccccc; ddddd; eeeee; ..; zzzzz]

   |76543210|76543210|7654.. ..       |76543210|
   |aaaaabbb|bbcccccd|ddde.. ..        zzzzz100|

   |76543210|76543210|7654.. ..  43210|76543210|
   |aaaaabbb|bbcccccd|ddde.. ..  yzzzz|z1000000|

   |76543210|76543210|7654.. .. 543210|76543210|
   |aaaaabbb|bbcccccd|ddde.. .. yzzzzz|10000000|

======= ============== ==============================
Name    Size           Contents
======= ============== ==============================
length  1 byte         < 256
content (length) bytes 5bit integers with termination
======= ============== ==============================

Optional_hash
~~~~~~~~~~~~~

None Case
^^^^^^^^^

==== ====== ========
Name Size   Contents
==== ====== ========
tag  1 byte 0x00
==== ====== ========

Some Case
^^^^^^^^^

==== ======== ========
Name Size     Contents
==== ======== ========
tag  1 byte   0x01
hash 32 bytes hash
==== ======== ========
