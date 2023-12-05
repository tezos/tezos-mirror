DAL overview
============

The *Data-Availability Layer* (DAL) enables users to publish blobs
of data outside of the Tezos Layer 1 (L1) blocks. A blob (for “binary
large object”) is a piece of data in binary form. While the primary
use case for these blobs is to store Layer 2 (L2) operations for Tezos
smart rollups, it is important to note that the DAL is more generic
and could be used for other use cases in the future.

In practice, the DAL employs an additional P2P protocol to publish
blobs, enabling a better bandwidth sharing between the peers (further
elaborated in the P2P section below). The DAL aims to support a
bandwidth of 10 MiB/s, a stark contrast to the current situation on
L1, which has an approximate bandwidth of 32 KiB/s. This highlights
the significant boost in bandwidth provided by the DAL.

Thanks to the DAL, the data bandwidth constraints imposed by the L1
block size are put back by several orders of magnitudes. This
translates into reduced fees for users when posting L2 operations
without compromising the fundamental principle of decentralization.

The amount of data that can be transmitted over the DAL network is,
however, still controlled by the economic protocol. The economic
protocol also plays a crucial role in determining the availability of
those data through the participation of bakers.

Similarly to the Tezos L1, the DAL is permissionless, enabling any
user to effectively contribute data to it, and allowing any smart
rollup kernel or smart rollup operator to access this data.

.. |DAL overview| image:: dal_overview.png

|DAL overview|

The figure illustrates the standard process for interacting with the
DAL as follows:

The diagram depicts a scenario where a user intends to upload data for
a dedicated rollup.

Anyone engaging with the DAL must utilize a tool known as the *DAL
node* (named ``octez-dal-node``). When a user decides to provide a new
blob to the DAL (depicted as step 1 in the diagram), the user
transmits the data to the DAL node to calculate a commitment to the
data. This commitment is then communicated to L1 via a dedicated
operation (indicated as step 2). Following L1’s approval of this
operation (step 3), the DAL expands this data for redundancy,
partitions the expanded data into segments called shards, and
disseminates the shards across the DAL network (shown in step 4).
Shards are assigned to bakers proportionally to their stake by the
economic protocol. Hence, the bakers, also connected to the DAL
network, retrieve these assigned shards (step 5). The bakers must
download and verify these shards within a specific timeframe,
precisely defined by the economic protocol as the ``attestation lag``
period. At the end of the attestation lag period, bakers declare using
another dedicated operation, whether they were able to download the
shards effectively (illustrated in step 6). The economic protocol
collates these attestations and, if a sufficient number of bakers have
successfully obtained the shards, the data is declared as available
(step 7). Only when the data is labeled as available can the rollup
utilize it (represented as step 8).

The rationale for having the attestation lag parameter is to give
bakers sufficient time to download their assigned shards and to
guarantee that the latency stays within acceptable limits (around one
minute).
