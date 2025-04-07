Protocol Release Checklist
==========================

This page documents the steps needed for releasing and injecting a
protocol proposal for Tezos. These include:

- technical steps such as releasing code and launching a test network;
- meta-technical steps such as extending the developer documentation;
- non-technical steps such as managing public relations (PR) via appropriate communication.

Protocol release is one step of the process of developing a protocol proposal, see :doc:`protocol_playbook`.

Summary
-------

Documentation & Public Relations Steps
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before protocol proposal time, there must be both a complete technical
description of the changes in the protocol (including all necessary
documentation), and a complete set of blog posts; one blog post must introduce
the proposal to a non-technical audience and describe who created it, while a
second blog post points to the technical documentation and gives a high level
overview of the technical changes.

Technical Steps
~~~~~~~~~~~~~~~

The release manager, in coordination with both protocol and shell teams, must
ask for the protocol to be released, a test network must be created, and
whatever tests are currently demanded for the protocol must pass as green.

The new shell should be released before injection occurs (see :doc:`../releases/releases`),
to make it simpler for third parties to run tests before voting.
This is especially necessary if a new environment is required for the
new protocol.

Timeline and Checklist
----------------------

**Protocol Name:**

The protocol name is
chosen by active members of the development team at least one week before the
planned release. *Selecting the name is the privilege of the whole set of
developers who were involved in the release.*

-  Traditionally, the name must be that of an ancient city, and its first letter
   must be the successor of the first letter of the previous protocol.

**Communication Preparation:**

During this phase, blog and social network posts are drafted and reviewed.

**Protocol CI Checklist:**

The release of a new protocol requires some manual configuration in the CI:

- The new ``n+1`` protocol must be added to those being built and tested
- The old ``n-1`` protocol must be removed from those being built and tested

This is done by adding and removing the respective protocols to
:src:`script-inputs/active_protocol_versions` and :src:`script-inputs/slim-mode-dune`,
which is done by defining the protocols with the right functions
(typically ``active`` or ``frozen``) in :src:`manifest/product_octez.ml`
and running ``make -C manifest``.

**Final Tests and sign-off:**

In the below, “Protocol Shepherd” is abbreviated “Shepherd”.

-  **Shepherd Initiates Final QA Check**:

   -  declares feature freeze;
   -  checks final gas rescaling;
   -  checks final migration code.

-  **Tests Are Conducted**

   -  **Unit Tests:** All unit tests must pass.
   -  **Integration Tests:** All integration tests must pass.
   -  **Protocol Migration Test**: the Mainnet migration test must be
      run and must pass.

-  **Developer Meeting Agrees to Freeze the Code** (up to bug fixes)

-  **Shepherd compiles the doc page with the changelog**
   (e.g. :doc:`../protocols/006_carthage`),
   and resets the :doc:`Alpha changelog <../protocols/alpha>`,
   as the diff between protocol Alpha and the proposed
   protocol restarts at zero (e.g. :gl:`!3123`)

-  **Shepherd Declares We Are Ready For Proposal Creation**

**At this point there should be at least one week left** before
injection to give time for the test network and the release to be
ready and tested before the injection. If some features
need to be dropped to meet this deadline (including because they were
not reviewed thoroughly enough), they should be dropped.

-  **Protocol Hash**: a vanity hash for the new protocol is computed and
   chosen by its developers

   -  Several developers may create hashes which are meant to capture as
      much of the protocol name as possible; the specific hash that is
      chosen is picked by consensus of the developers involved in the
      release.

-  **Release Artifacts are Created**: an MR on ``master`` is made with the result
   of the protocol snapshot (mostly automated by script :src:`scripts/snapshot_alpha_and_link.sh`) linked into the
   node and client.

   The final snapshot is
   produced from the latest version of ``proto-beta``. It is rehashed and
   renamed. Release material (including blogposts and other communication
   artifacts) should be made ready using the final protocol hash and
   name.

   In the merge request introducing ``proto-<proposal name>``,
   **``proto-beta`` is deleted from the repository**. In order to avoid a
   period of time where no testnet is easily reachable to test the new
   proposed protocol, this change should be made (as in ‘merged into the
   master branch of tezos/tezos repository’) only once the communication
   and testnets are ready.
   **Release and** **Injection of this protocol amendment proposal should
   follow minutes/hours after.**

   The few additional manual steps to be done on the documentation part are listed in meta-issue :gl:`#2170` (Section "Protocol snapshot").
   The delegates must also be built in this branch. CI must be
   run on this branch and pass.

   **NB: Make sure NOT to merge this MR about the same day when a previous protocol gets activated.** It is recommended to avoid merging a snapshot for a new protocol within 1-2 days of another protocol's activation, due to significant interactions and interferences between the snapshotting process and that for upgrading the documentation site to reflect a protocol activation.
   Note that the activation date cannot be modified, but is known at least two weeks in advance.

-  **Test Network Preparation**: everything before the Spawn Test
   Network section of:
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-launch-a-test-network.md

-  **New Shell Release Preparation**: see the Preparation section of:
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-release-tezos.org

-  **Release Manager Declares Release Artifacts Final**: If previous steps all
   work, the artifacts are ready for public release.

-  Blogpost and Agora post are prepared to announce <X> Protocol Proposal release.

**At the time of release: Past here, preparation work is done: we are at
the point of no return.**

-  **Agora Posts, Tweets**: Blog posts are published on Tezos Agora announcing
   the release; tweets linking to the Agora posts may go out at this time. (The
   Agora posts go out first to encourage people to view Agora as the place to go
   first and earliest for Tezos announcements.)

-  **Company Blog Posts, Tweets**: Blog posts are made on company web
   sites 15 minutes to an hour later than the Agora posts (so that the
   Agora post is the first announcement) and tweets pointing to company
   blogs may go out at this time.

-  **Shell Release(s)**: new version with the protocol and the delegates
   (and possibly :doc:`adding a new protocol environment <../developer/protocol_environment_upgrade>`),
   see the Release section of:
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-release-tezos.org

-  **Test Network**: the test network for the new protocol is started, see
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-launch-a-test-network.md

-  **Injection**: a baker injects the protocol using a Proposal operation.
   This must happen ideally during the first half of a Proposal period.
