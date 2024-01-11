Protocol Release Checklist
==========================

This page documents the steps needed for preparing and injecting a
protocol proposal for Tezos. These include:

- technical steps such as releasing code and launching a test network;
- meta-technical steps such as extending the developer documentation;
- non-technical steps such as managing public relations (PR) via appropriate communication.

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

**Protocol Development:**

Protocol development occurs during the months before an injection slot is available.

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
``active_protocol_versions``, :src:`scripts/remove-old-protocols.sh` will ``rm`` the appropriate older protocol.

Additionally, the ``unit:NNN_PROTONAME`` unit test jobs in
:src:`.gitlab/ci/jobs/test/oc.unit.yml` must be updated to test the new protocols and stop
testing the old ones, in the same manner as above.

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

**One Month Before Potential Activation**

-  **Test Network End of Line**: announce the end of the previous test
   network. More precisely, announce that the test network for protocol
   ``n-1`` will be stopped when (if) protocol ``n+1`` activates.
   This gives one month for users to migrate to the test network for
   protocol ``n`` or ``n+1``.

**A Couple of Days Before Activation**

-  **Reminder For Bakers**: post in the baking slack and in the baker newsletter
   a reminder for them to upgrade.

-  **PR Team Work**: The Tezos Foundation's (TF) PR team prepares news releases
   for the press to go out after protocol activation is known to have been fully
   successful.

-  **Short Blog Post Drafted**: A short blog post is drafted to announce
   successful activation; it will be posted around the same time that
   news releases go out.

-  **Doc Update Approved**: A documentation update reflecting the new active protocol and droping the documentation of the previous protocol has to be ready. Technically, an MR instantiating meta-issue :gl:`#2170` (Section "Protocol activation") for the new protocol has to be ready and must have sufficient approvals to be triggered right after activation.

**After Activation**

A few hours after activation, when it is certain that everything has
happened successfully, a blog post goes out to Agora and then company
blogs (the same order as “At The Time Of Release", above”) and then a
news release is sent out by the TF’s PR people.

On the main `tezos/tezos <https://gitlab.com/tezos/tezos>`__ repository, on the
master branch, the ``proto_alpha`` directory is reset to the newly activated
protocol and its associated daemons, with the exception of vanity nonce and
migration that should be reverted.

Soon after the activation (preferably on the same or next day), the MR updating the documentation to reflect the new active protocol (see above) has to be merged. Also, part
of the code related to the old protocol can now be dropped, see
:doc:`../developer/howto-freeze-protocols`.

One month after the activation of protocol N, we deactivate the N-1 test
network. (For example, the Babylon net was deactivated one month after
Carthage went live on the main network.) This deactivation has been already
announced one month before activation (see above).
