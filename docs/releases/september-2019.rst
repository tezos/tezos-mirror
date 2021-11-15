Mainnet September 2019
======================

**This release contains the patch necessary to correctly activate protocol Babylon.**

During the testing phase of protocol 005_PsBABY5H, a bug was
identified (more details in the :ref:`005-bigmap-bug`).
The tezos-node in this release contains a corrected version of
Babylon, protocol 005_PsBabyM1, that will be activated in place of
005_PsBABY5H if the promotion vote is successful.

Any node running the May release, in case of a successful promotion
vote, will activate protocol 005_PsBABY5H, which contains a bug
affecting bigmaps in smart contracts.

**We advise users to update to this release so that the corrected protocol 005_PsBabyM1 will be activated.**

In case of a negative promotion vote, any release of the tezos-node
will simply proceed to a new proposal phase.

Changelog
---------

Changes introduced by Babylon (including RPC calls) can be found in
:doc:`the protocol documentation <../protocols/005_babylon>`.

Shell
~~~~~

- Override Babylon by its fixed version if vote succeeds
- Commits:
    - ``f7c18c30a``: ``Shell: patches PsBABY5HQ with PsBabyM1 only if activated``
    - ``dc93bd034``: ``Mainnet: import PsBabyM1 and replace PsBABY5H with it``
    - ``eee089b85``: ``Shell: introduce voted protocol overrides``

Baker
~~~~~

- Include version number at the beginning of the proof-of-work nonce
- Commits:
    - ``0c7fb2add``: ``Build: update tezos-version.opam``
    - ``e9846ab85``: ``Baker: preserve the commit hash hexadecimal form if possible``
    - ``5ece2883a``: ``Baker: include commit hash in pow nonce``
    - ``5e9fdb77e``: ``Version: refresh Git commit hash if a file changes``
    - ``66c2660e0``: ``Base: move Git info into new lib tezos-version``

Client
~~~~~~

- Fix ``run script`` command output format for big maps
- Commits:
    - ``246c9165b``: ``Client: fix `run script` command output format``
