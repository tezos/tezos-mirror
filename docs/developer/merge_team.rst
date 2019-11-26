Merge Team
==========

The merge team is a group of developers from different teams of the
Tezos ecosystem that controls what is merged into the
https://gitlab.com/tezos/tezos/ repository.
They are automatically selected as approvers when you create a merge
request.

The general policy is to assure that at least two members of the merge
team have looked at an MR, typically one would approve and another
would merge.
Simple MRs can be merged by a single reviewer.

How to submit an MR
-------------------

Your goal is to help the reviewers convince themselves that your patch
should be merged.
Well-documented merge requests will receive feedback faster.
Complicated patches with no comments to help the reviewer will cause
the reviewer to make the wrong decision or will discourage the
reviewer to work on the MR.

- *Give context*: why was this patch written?

  - Does it fix a bug, add a feature or refactor existing code?
  - Is there an open issue on Gitlab, or a post from an angry user
    somewhere?
  - Must it be merged before another merge request?

- *Test*:

  - Explain how you tested your patch (or why you didn't).
  - Give instructions to reproduce the bug or to test the new feature.

- *Divide and conquer*: it is easier to merge several simple merge
  requests than a big one.

  - Isolate complicated parts of your patch in their own commits.
  - Put simple, non-controversial commits first. For instance: commits
    which fix typos, improve documentation, or are simple enough that
    we may want to merge them even without the rest of the merge
    request.
    Even better put them in a separate MR which can be merged easily.
  - Split your commits so that each step is convincing on its own, like
    the proof of a big theorem which is split into several lemmas.

- *Anticipate questions*: explain anything which may look surprising, as comments in the code itself if it has value to future readers, or in the MR description.

- *Find reviewers*: it is the responsability of the author to find a
  suitable reviewer, ideally before opening an MR. The reviewer(s)
  should be mentioned in the description or in the comments.
  An MR without a reviewer for several days will be closed (and happily
  reopened once one is found).

WIP and RFC merge requests
~~~~~~~~~~~~~~~~~~~~~~~~~~

A merge request can be marked as Work In Progress or Request For
Comments to signal that it should **not** be merged immediately but
nonetheless requires some feedback.
It is important to maintain this kind of MRs to a minimum and to
constantly check that the discussion is progressing.

Dos and Don'ts
~~~~~~~~~~~~~~

- Set up correctly your editor:

  + automatically run `ocamlformat` when saving a file
  + no tabs, use whitespaces
  + no trailing whitespaces
  + indent correctly (e.g. use lisp-mode for dune files)

  Many of these checks can be run with ``make test-lint``.

- No peneloping: don't do something in a commit just to undo it two
  commits later.
- Don't mix refactors or indentation with real changes.

- Follow the format of commit names, `<Component>: <message>`, with
  message in indicative or imperative present mood e.g. ``Shell: fix
  bug #13`` rather than ``Shell: fixed bug #13``.
  Use multilines commit messages for important commits.

- Document your changes, in the form of docstrings or documentation.
  Imagine if somebody asked what your change was about in front of the
  coffee machine, write down your answer and put it at the top of the
  file.
  This is especially important for shell scripts, dune files and parts
  of the codebase that are rarely touched and few people know.
  Plus you can copy it and you get a description of your MR for free.

- Add Gitlab's Labels to MRs, like `doc` or `protocol`.
- When opening an MR you should probably tick the following options:

  + `Delete source branch when merge request is accepted.`
    Helps keeping the repository clean of old branches.
  + `Squash commits when merge request is accepted.`
    Sometimes it's useful to have many small commits to ease the
    review and see the story of a branch, but they are not relevant
    for the history of the project. In this case they can be squashed
    and replaced with a single meaningful commit.
  + `Allow commits from members who can merge to the target branch.`
    This option is useful to allow members of the merge team, who are
    not developers in your project, to commit to your branch.
    It helps to rebase and propose fixes.

- No MR targeting ``master`` should touch
  ``src/proto_alpha/lib_protocol``, those are for ``proto-proposal``
  (see later).

- Tezos uses a `git rebase workflow
  <https://www.atlassian.com/git/articles/git-team-workflows-merge-or-rebase>`_.
  Make sure that your source branch is cleanly rebased on top of your
  target branch.
  Being proficient with interactive rebases is mandatory to avoid
  mistakes and wasting time.


Example of good MR::

  * Doc: mark bug #13 as fixed
  * Test_python: add test for p2p bug #13
  * Flextesa: add test for p2p bug #13
  * Shell: fix p2p bug #13
    - fix bug in the shell
    - fix relative unit test
    - add docstrings
  * (master)

Protocol development
--------------------

Because of the amendment procedure that governs the protocol, the
workflow for protocol development is significantly different from
master.

All work on the protocol is done in the branch ``proto-proposal``, which
is regularly rebased on master.
Before a proposal, a new branch, e.g. ``proto-005-PsBabyM1``, is
created from ``proto-proposal`` where the development continues.
When and if ``proto-005-PsBabyM1`` is activated, it is then merged
into master.

The hash of the protocol is computed from the directory
``src/proto_alpha/lib_protocol``, so every change in this directory is
bound to ``proto-proposal``.
MRs that touch the client or daemons in ``src/proto_alpha/`` should be
merged in master, except if they depend on a new protocol feature, in
this case they go to ``proto-proposal``.
Make an effort to split your MR so that a maximum of code can be
merged in master.


The Migration
~~~~~~~~~~~~~

Right before the activation of a new protocol, there is a migration of
the context that takes place.
This migration typically converts data structures from the old to the
new format.
Each migration works exclusively between two protocol hashes and it is
useless otherwise.
For this reason after the activation of a protocol the first step to
start a new development cycle is to remove the migration code.
In order to facilitate this, *migration code is always in a different commit* with respect to the protocol features it migrates.
When submitting an MR which contains migration code, **the author must also have tested the migration** (see :ref:`proposal_testing`) and write in the
description what was tested and how so that **reviewers can reproduce it**.

Shape of commits
~~~~~~~~~~~~~~~~

In order to ease rebasing and reworking the history, **we don't expect
every commit to compile and pass tests**.
We prefer to keep commits small and local to a component.
Note that we do expect to pass tests between each MR.

A typical MR for ``proto-proposal`` would look like this::

  * Tests_python: test that no block is produced during a weekend
  * Flextesa: test that no block is produced during a weekend
  * Proto/test: test that no block is produced during a weekend
  * Proto/Baker: skip weekend when producing blocks
  * Proto/Migration: migrate table of rights to remove weekends
  * Proto: stop block production during weekends
    + block submitted during a weekend fails application
    + adapt computation of rights
    + add RPC to check weekends

Right after the change to the protocol, the code might not compile,
because the baker is not fixed yet.
After the baker commit, the test might not pass because of a change in
behavior.

The Merge-Request Bot
~~~~~~~~~~~~~~~~~~~~~

Every 6 hours, an automated process running as the
`Tezbocop <https://gitlab.com/tezbocop>`_ 🤖 user, inspects recent MRs and posts
or edits one comment on them; giving an inspection report on the contents of the
MR.

Some warnings/comments are for you to potentially improve your MR, other
comments just help us in the assignment & review process.

The first time Tezbocop posts a message you should receive a notification; for
the subsequent edits there won't be notifications; feel free to check Tezbocop's
comment any time.

If you think some of the remarks/warnings do not apply to your MR feel free to
add a comment to justify it.

The code for the bot is at
`smondet/merbocop <https://gitlab.com/smondet/merbocop>`_. It is of course
work-in-progress and new warnings and comments will appear little by little.
We welcome specific issues or contributions there too.


