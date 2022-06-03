V2 for Tree2
============

Same as :doc:`./v2-tree32`
except `inode of tree <#inode-of-tree>`__, `inode_trees of
inode_tree <#inode-trees-of-inode-tree>`__, `inode of
elt <#inode-of-elt>`__ and `Segment <#segment>`__

Tree Proof
----------

Tree
~~~~

inode of tree
^^^^^^^^^^^^^^^

+-----------------------+-----------------------+-------------------------+
| Name                  | Size                  | Contents                |
+=======================+=======================+=========================+
| tag                   | 1 byte                | 0b000000yy where yy     |
|                       |                       | is tag for length       |
|                       |                       | (0b00 for p = 1, 0b01   |
|                       |                       | for p = 2, 0b10 for p   |
|                       |                       | = 4, 0b11 for p = 8)    |
+-----------------------+-----------------------+-------------------------+
| length                | p bytes               | int                     |
+-----------------------+-----------------------+-------------------------+
| (optional) inode_tree | variable              | 0th :ref:`Inode_tree    |
|                       |                       | <v2-tree32-inode-tree>` |
+-----------------------+-----------------------+-------------------------+
| (optional) inode_tree | variable              | 1st :ref:`Inode_tree    |
|                       |                       | <v2-tree32-inode-tree>` |
+-----------------------+-----------------------+-------------------------+

Inode_tree
~~~~~~~~~~

inode_trees of inode_tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------+-----------------------+-------------------------+
| Name                  | Size                  | Contents                |
+=======================+=======================+=========================+
| tag                   | 1 byte                | 0b000000yy for yy is    |
|                       |                       | tag for length (0b00    |
|                       |                       | for p = 1, 0b01 for p   |
|                       |                       | = 2, 0b10 for p = 4,    |
|                       |                       | 0b11 for p = 8)         |
+-----------------------+-----------------------+-------------------------+
| length                | p bytes               | int                     |
+-----------------------+-----------------------+-------------------------+
| (optional) inode_tree | variable              | 0th  :ref:`Inode_tree   |
|                       |                       | <v2-tree32-inode-tree>` |
+-----------------------+-----------------------+-------------------------+
| (optional) inode_tree | variable              | 1st :ref:`Inode_tree    |
|                       |                       | <v2-tree32-inode-tree>` |
+-----------------------+-----------------------+-------------------------+

Stream Proof
------------

Elt
~~~

inode of elt
^^^^^^^^^^^^^^

+-----------------------+-----------------------+-----------------------+
| Name                  | Size                  | Contents              |
+=======================+=======================+=======================+
| tag                   | 1 byte                | 0b0000zwyy where yy   |
|                       |                       | is tag for length     |
|                       |                       | (0b00 for p = 1, 0b01 |
|                       |                       | for p = 2, 0b10 for p |
|                       |                       | = 4, 0b11 for p = 8)  |
+-----------------------+-----------------------+-----------------------+
| length                | p bytes               | int                   |
+-----------------------+-----------------------+-----------------------+
| (optional) hash       | 0 or 32 bytes         | if w is 0b1, 0th hash |
|                       |                       | (Otherwise, this      |
|                       |                       | record does not       |
|                       |                       | exist)                |
+-----------------------+-----------------------+-----------------------+
| (optional) hash       | 0 or 32 bytes         | if z is 0b1, 1st hash |
|                       |                       | (same as above)       |
+-----------------------+-----------------------+-----------------------+

Other Components
----------------

Segment
~~~~~~~~~

Same as in :doc:`v1-tree2`

* The segment int is in 1 bits
* 10\* is filled at the end of the bytes
* ``n`` segments need ``(n+8)/8`` bytes

::

   ex: Encoding of [a; b; c; d; e; ..; z]

   |76543210|7654.. ..       |76543210|
   |abcdefgh|ijkl.. ..        vwxyz100|

   |76543210|7654.. ..  43210|76543210|
   |abcdefgh|ijkl.. ..  uvwxy|z1000000|

   |76543210|7654.. .. 543210|76543210|
   |abcdefgh|ijkl.. .. uvwxyz|10000000|

======= ============== ===============================
Name    Size           Contents
======= ============== ===============================
length  1 byte         < 256
content (length) bytes 1 bit integers with termination
======= ============== ===============================
