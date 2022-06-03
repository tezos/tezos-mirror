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

.. _v1-tree32-inode-tree:

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

Used in `dense case of inode_proofs <#dense-case-of-inode-proofs>`__
below

==== ====== ========
Name Size   Contents
==== ====== ========
tag  1 byte 0x04
==== ====== ========

Inode_proofs
~~~~~~~~~~~~

When Inode_proofs has less than 16 trees, sparse case is selected.
Otherwise, dense case is selected.

sparse case of inode_proofs
^^^^^^^^^^^^^^^^^^^^^^^^^^^

========================== ============== ===================================================
Name                       Size           Contents
========================== ============== ===================================================
tag                        1 byte         0x00
length                     4 bytes        byte length of below list
(index \* inode_tree) list (length) bytes sequense of (uint8 \* `inode-tree <#inode-tree>`__)
========================== ============== ===================================================

dense case of inode_proofs
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
~~~~~~~~~~~~~~~~~~~~

When Inode_proofs_of_hash has less than 16 trees, sparse case is
selected. Otherwise, dense case is selected.

sparse case of inode_proofs_of_hash
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

==================== ============== =========================================================
Name                 Size           Contents
==================== ============== =========================================================
tag                  1 byte         0x00
length               4 bytes        byte length of below list
(index \* hash) list (length) bytes sequense of (uint8 \* `Optional_hash <#optional-hash>`__)
==================== ============== =========================================================

dense case of inode_proofs_hash
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

.. _v1-tree32-optional-hash:

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
