DAL Slots
=========

There is a limited number of blobs, called *DAL slots* (or slots for
short), that can be published at a L1 level. A slot is identified by
the L1 level at which it is published, an index, and its
commitment. Depending on the context, a “slot” may refer to the data
blob or to the placeholder (the slot index) of that blob. Each slot is
treated independently of other slots. Presently, all slots are of the
same size, though this may change with future updates. To publish a
slot, it is first expanded to create redundancy through erasure
coding, increasing its size by a factor known as the *redundancy
factor*. This expanded slot is then divided into a set of smaller data
pieces of equal size, called *shards*. Finally, shards are distributed
across the DAL P2P network.

The advantage of using erasure coding is that one only needs a subset
of the shards, specifically, “number of shards / redundancy factor” to
recover the full original slot data. A key feature for the shards is
that each shard can be verified to ensure it corresponds to a
particular commitment, which safeguards against spam in the DAL P2P
network.

For each slot index, multiple commitments can be included in a block,
but only one will be acknowledged, the first one appearing in the
block ordering. Because bakers typically prioritize operations that
offer higher fees, the commitment with the higher fee is usually
chosen for inclusion, although this outcome is not guaranteed if a
baker uses a different selection criterion.

For smart rollups, it is necessary to divide the slot into smaller
segments called *pages* (see
:ref:`populating_the_reveal_channel`). The economic protocol specifies
the size of each page as a constant, with 4KiB being a practical
size. While it might seem feasible to choose shards fitting this page
limit and directly feed shards to the rollup, accessing the original
data from shards involves complex cryptographic computations. We
prefer to avoid performing such computations within the smart
rollups. Therefore, even though the DAL network uses shards
internally, the DAL node can serve individual pages as requested by
the smart-rollup node. These pages can then be imported individually
by the smart rollup node to the kernel on demand. To reconstruct the
entire slot, one simply needs to arrange these pages in the correct
sequence. For developers working with smart-rollup kernels, the
technicalities of reconstructing the entire slot out of pages should
ideally be handled by the SDK they use, simplifying the process even
more.

.. |DAL slot| image:: dal_slot.png

|DAL slot|


When the slot producer (user publishing a slot) posts the commitment
onto the L1, it also posts a proof that the slot does not exceed the
slot size. This prevents malicious users from producing and posting
data larger than the expected size.
