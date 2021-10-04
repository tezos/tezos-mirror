:orphan:

Protocol Release Checklist
==========================

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

A test network must be launched, and whatever tests are currently demanded for
the test network must pass.

The new shell must be released at about the same time as injection
occurs, to make it simpler for third parties to run tests before voting.
This is especially necessary if a new environment is required for the
new protocol.

Timeline and Checklist
----------------------

**Protocol Development:**

occurs during the months before an injection slot is available.

**Protocol Name:**

chosen by active members of the development team at least one week before the
planned release. *Selecting the name is the privilege of the whole set of
developers who were involved in the release.*

-  Traditionally, the name must be that of an ancient city, and its first letter
   must be the successor of the first letter of the previous protocol.

**Communication Preparation:**

blog and social network posts are drafted and reviewed

**Protocol CI Checklist:**

The release of a new protocol requires some manual configuration in the CI:

- The new ``n+1`` protocol must be added to those being built and tested
- The old ``n-1`` protocol must be removed from those being built and tested

This is done by adding and removing the respective protocols to
`active_protocol_versions`, and by changing the `build` job in
:src:`.gitlab/ci/build.yml` to `rm` the appropriate older protocol.

Additionally, the `unit:NNN_PROTONAME` unit test jobs in
:src:`.gitlab/ci/unittest.yml` must be updated to test the new protocols and stop
testing the old ones, in the same manner as above.

**Final Tests and sign-off:**

In the below, “Protocol Shepherd” is abbreviated “Sheperd”.

-  **Sheperd Initiates Final QA Check**:

   -  declares feature freeze
   -  final gas rescaling
   -  final migration code

-  **Tests Are Conducted**

   -  **All Unit Tests:** All unit tests must pass.
   -  **All Integration Tests:** All integration tests must pass.
   -  **Protocol Migration Test**: the Mainnet migration test must be
      run and must pass.

-  **Developer Meeting Agrees to Freeze the Code** (up to bug fixes)

-  **Sheperd compiles the doc page with the changelog**
   e.g. https://tezos.gitlab.io/protocols/006_carthage.html

-  **Sheperd Declares We Are Ready For Proposal Creation**

**At this point there should be at least one week left** before the
injection period to give time for the test network and the release to be
ready and tested before the injection period start. If some features
need to be dropped to meet this deadline (including because they were
not reviewed thoroughly enough), they can be dropped.

-  **Protocol Hash**: a vanity hash for the new protocol is computed and
   chosen by its developers

   -  Several developers may create hashes which are meant to capture as
      much of the protocol name as possible; the specific hash that is
      chosen is picked by consensus of the developers involved in the
      release.

-  **Release Artifacts are Created**: an MR on ``master`` is made with the result
   of the protocol snapshot (:src:`snapshot_alpha_and_link.sh`) linked into the
   node and client. The delegates must also be built in this branch. CI must be
   run on this branch and pass.

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
   (and possibly a new protocol environment), see the Release section of:
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-release-tezos.org

-  **Test Network**: the test network for the new protocol is started, see
   https://gitlab.com/romain.nl/howtos/-/blob/master/HOWTO-launch-a-test-network.md

-  **Injection**: a user injects the protocol

**One Month Before Potential Activation**

-  **Test Network End of Line**: announce the end of the previous test
   network. More precisely, announce that the test network for protocol
   ``n-1`` will be stopped when (if) protocol ``n+1`` activates.
   This gives one month for users to migrate to the test network for
   protocol ``n`` or ``n+1``.

**A Couple of Days Before Activation**

-  **Reminder For Bakers**: post in the baking slack and in the baker newsletter
   reminder for them to upgrade.

-  **PR Team Work**: The Tezos Foundation's (TF) PR team prepares news releases
   for the press to go out after protocol activation is known to have been fully
   successful.

-  **Short Blog Post Drafted**: A short blog post is drafted to announce
   successful activation; it will be posted around the same time that
   news releases go out.

**After Activation**

A few hours after activation, when it is certain that everything has
happened successfully, a blog post goes out to Agora and then company
blogs (the same order as “At The Time Of Release", above”) and then a
news release is sent out by the TF’s PR people.

On the main `tezos/tezos <https://gitlab.com/tezos/tezos>`__ repository, on the
master branch, the ``proto_alpha`` directory is reset to the newly activated
protocol and its associated daemons, with the exception of vanity nonce and
migration that should be reverted.

Soon after the injection (during the following days), the documentation has to
be shifted to reflect the new active protocol and to drop the documentation of
the previous protocol, see meta-issue :gl:`nomadic-labs/tezos#462`. Also, part
of the code related to the old protocol can now be dropped, see
:doc:`../developer/howto-freeze-protocols`.

One month after the activation of protocol N, we deactivate the N-1 test
network. (For example, the Babylon net was deactivated one month after
Carthage went live on the main network.) This deactivation needs to be
announced some time before.
