How to contribute to Octez
==========================

The purpose of this document is to help developers contribute to
Octez.

Introduction
------------

There are several ways to get involved with the codebase, and you may want to start with some preliminary steps.


Reporting issues
~~~~~~~~~~~~~~~~

The simplest way to contribute to Octez is to report issues that you may
find with the software on `GitLab <https://gitlab.com/tezos/tezos/-/issues>`__.
If you are unsure about an issue
consult the :ref:`Tezos community <tezos_community>`
first and always make sure
to search the existing issues before reporting a new one.
Some information that are probably important to include in the description:
the architecture (e.g. *ARM64*), the operating system (e.g. *Debian
Stretch*), the network you are connected to (e.g. *Ghostnet*), the
binary or component (e.g. *octez-node crashes* or *rpc X returns Y
while Z was expected*).

Other useful contributions consist in inspecting existing issues and adding details, suggesting workarounds, etc.

Fixing typos
~~~~~~~~~~~~

You may also want to fix some typos and minor errors or incoherencies in the *documentation*, which is situated in the ``docs/`` subfolder of the code repository, or within *comments* or *docstrings* within the code.
Small tweaks like these can be contributed without creating a merge request and commits can rather be pushed directly to the ``typo-doc`` branch in the ``tezos/tezos`` repository. This branch is regularly merged into the master branch, e.g., every one or two weeks.
(If the branch has been automatically deleted following a merge, just create it again.)
This periodic merging is implemented by a series of MRs named "the typo train", created for you by a volunteer, and batching the currently pending fixes.
Of course, all these commits will be reviewed before being integrated.
The current edition of the typo train MR can be found in meta-issue :gl:`#2329`.

If you don't have enough permissions to push to the branch above, you can  still make commits in your own fork of the Octez repository, and ask for them to be cherry-picked on the typo/train on the ``#documentation`` channel on the Tezos Dev Slack space.
Alternatively, you may of course create your own MRs for submitting your changes, without using the typo train.

Other non-code contributions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you acquired a good expertise level in using, configuring, or overcoming problems with the various Octez executables, you may bring useful contributions by providing assistance to others via the different channels (Slack,  StackExchange, etc.) in the :ref:`Tezos community <tezos_community>`.

If you want to shape the future of Octez, you may consider contributing to Tezos Improvement Proposals (TZIPs), that specify new features or define standards. These are located in the `TZIP repo <https://gitlab.com/tezos/tzip/>`__.

Going further
~~~~~~~~~~~~~

To directly contribute to the *code* itself, expertise in a few areas is necessary.

First, make sure that you are proficient enough in OCaml. The OCaml community
website https://ocaml.org gives a few useful pointers for that. In
particular, we use a lot of functors, and a few GADTs in the codebase,
so you may want to make sure that you master these advanced concepts.
For a more specific explanation of GADT usage in Octez you can check out
:doc:`gadt`.

Then, if you don’t know much about the Lwt library, that’s what you want
to learn next. This library is used extensively throughout the code base:
we use it to handle concurrency. You can use the
`online documentation <https://ocsigen.org/lwt/3.2.1/manual/manual>`__. The
chapter on concurrency of the `Real World OCaml <https://dev.realworldocaml.org/>`__ book
has also been `ported to Lwt <https://github.com/dkim/rwo-lwt>`__.

After that, it is a good idea to read the tutorials for
:doc:`error_monad <error_monad>` and
:doc:`data_encoding <data_encoding>`, two homegrown
libraries that we use pervasively.

While you familiarize yourself with the basics as suggested above, you
can have a look at the :doc:`software architecture
<../shell/the_big_picture>` of Tezos. It will
give you the main components and their interactions, and links to the
documentation for the various parts.

You may also want to take a look to some :ref:`developer tools <dev_tools>` that can make protocol development more convenient.

Now, that you're ready to delve into the code, it is time to know how
contributions to the code are submitted, reviewed, and finally accepted into the master branch.

Our git strategy
----------------

First of all, the repository is https://gitlab.com/tezos/tezos. So if you want
to contribute, simply create an account there.

There are many ways to use Git, here is ours.

We mostly use merge requests (aka MRs) for contributing to the master branch,
meaning that nobody should be pushing into the master branch directly. Once a
merge request is ready, it is reviewed and approved, then merged with a merge commit.

Therefore, you can bring useful contributions both by creating your own MRs and by reviewing MRs submitted by others.

We maintain a `semi-linear history <https://docs.gitlab.com/ee/user/project/merge_requests/reviews/index.html#semi-linear-history-merge-requests>`_,
which means that merge requests are only
merged if they are direct suffixes of the master branch.
This means that merge requests are rebased on top of ``master`` before they are merged.
This can only be done automatically if there is no conflict though.
So whenever ``origin/master`` changes, you should make sure that your branch
can still be rebased on it. In case of conflict, you need to rebase manually
(pull ``master``, checkout your branch and run ``git rebase master``).
You may have to edit your patches during the rebase.
Then use ``push -f`` in your branch to rewrite the history.
Being proficient with interactive rebases is mandatory to avoid
mistakes and wasting time.

This Git strategy is a variant of the `git rebase workflow <https://www.atlassian.com/git/tutorials/merging-vs-rebasing>`_.

.. _mr_workflow:

Workflow of an MR
-----------------

This section presents a global view of our MR workflow. Details about
the individual steps in this workflow are described in the following
sections.

Our code review process uses GitLab. First a developer creates a new
branch for :ref:`preparing the MR <preparing_MR>`.  As this is a
private new branch, the developer is free to rebase, squash commits,
rewrite history (``git push --force``), etc. at will.

Once the code is ready to be shared with the rest of the team, the
developer :ref:`opens a Merge Request <creating_MR>`.  It is useful to
explain why the MR is created, to add a precise description of the
code changes, and to check if those are in line with the initial
requirements (if responding to an issue), or to the stated reasons
(otherwise).  Dependencies on other merge requests, other
relationships to MRs, to issues, etc, should also be mentioned.

While the code is still not ready to be peer reviewed, but it is
merely a work in progress, the developer assigns it to themselves and
prefixes the MR title with ``Draft:``. This will
tell everybody they can look at the code, comment, but there is still
work to be done and the branch can change and history be rewritten.

Finally, when the code is ready for the :ref:`code review
<code_review>`, the developer assigns the MR to a reviewer
and freezes the branch. From this moment on, the
developer will refrain from rewriting history, although they can add new
commits and rebase the branch for syncing it with master (this can be
done regularly to make sure the branch does not get stale). At this
point the developer interacts with the reviewers to address their
comments and suggestions.

GitLab allows both to comment on the code and to add general comments
on the MR.  Each comment should be addressed by the developer. They
can add additional commits to address each comment. This incremental
approach will make it easier for the reviewer to keep interacting till
each discussion is resolved. When the reviewer is satisfied, they
will mark the discussion resolved.

The `Draft: <https://docs.gitlab.com/ee/user/project/merge_requests/drafts.html>`_
status can be set at any time during the
authoring/review process to signal that the MR is not yet ready for
merge (for instance, the ``git`` history might not be clean). But, the
``Draft`` status *does not* prohibit review -- this should be achieved by
assigning the MR to the developer.

You can refer to the following table that summarizes how assignee and
``Draft`` status interact to decide whether an MR is open for review:

.. table::

    +-------+----------+-----------+------------+
    | Draft | Assignee | Reviewers | Reviewable |
    +=======+==========+===========+============+
    | \-    | \-       | ∅         | No         |
    +-------+----------+-----------+------------+
    | Yes   | Author   | \-        | No         |
    +-------+----------+-----------+------------+
    | Yes   | Others   | Some      | Yes        |
    +-------+----------+-----------+------------+
    | No    | Author   | Some      | Yes        |
    +-------+----------+-----------+------------+

When all discussions are resolved, and the MR has got at least two
approvals from Octez Merge Team members, the developer should squash
any fix-up commits that were applied (remembering to edit the commit
message appropriately). Then the developer (or anyone else when not possible) can assign the MR to the `Nomadic
Marge-bot <https://gitlab.com/nomadic-margebot>`__, which will
automatically rebase the branch on top of master and finally merge it.

Merge Queue Order
-----------------

By default, Marge-bot will merge MRs assigned to it in the assignment order. That is, the first MR assigned is the first that is merged. There are situations when it is desirable to modify this order. To achieve this, Marge-bot respects two priority labels, whose usage is reserved to the :doc:`Octez merge team <merge_team>`:

- ``marge-priority::critical``: is reserved for MRs
  resolving critical production issues (e.g., fixing a blocked CI).
- ``marge-priority::high``: can be applied to MRs that needs to be
  merged urgently. For instance, it can applied to MRs for urgent
  releases or to reduce the merge delay between MRs that compose a
  stack.

MRs with ``marge-priority::critical`` will be treated before those with ``marge-priority::high`` who in turn will be treated before all MRs lacking either of these labels.

.. _preparing_MR:

Preparing a Merge Request
-------------------------

While working on your branch to prepare a Merge Request, make sure you respect the following rules:

-  Give a meaningful and consistent name to the branch

   * It is useful to prefix the name of the branch with the name of
     the developer to make it clear at a glance who is working on what: e.g.
     ``john@new-feature``.

   * Suffix the branch name by ``-release`` if it is a release branch.

-  Prefer small atomic commits over a large one that does many things.
-  Don’t mix refactoring, reindentation, whitespace deletion, or other style
   changes with new features or other real changes.
-  No peneloping: don't do something in a commit just to undo it two
   commits later.
-  We expect every commit to compile and pass tests.
   Obviously, we require tests to pass between each MR.
-  Follow the format of commit names, ``<Component>: <message>``, with
   message in indicative or imperative present mood e.g. ``Shell: fix
   bug #13`` rather than ``Shell: fixed bug #13``.
   Use multiline commit messages for important commits.
-  Adhere to the :doc:`coding guidelines <guidelines>`.
-  Document your changes, in the MR description and commit messages.
   Imagine if somebody asked what your change was about in front of the
   coffee machine, write down your answer and put it in the MR.
-  If there is a design description at the top of the file, consider updating
   it to reflect the new version. Additionally, if you feel that your design
   *changes* are worth mentioning to help upcoming contributors (e.g. justify a
   non-obvious design choice), you should document them in this file header,
   but in a separate "History" section.
-  If you add new functions to an interface, don’t forget to
   document the function in the interface (in the corresponding ``.mli`` file; or,
   if there is no ``.mli`` file, directly in the ``.ml`` file)
-  If you add a new RPC endpoint or modify an existing one, be sure to take
   into account the impact on :ref:`RPC security <rpc_security>`.
-  If you modify the user API (e.g. add or change a configuration parameter or
   a command-line option), update the corresponding documentation. In
   particular, for configuration parameters of the Octez node, update the node
   configuration :doc:`documentation <../user/node-configuration>` and the
   documentation of the modified component(s), usually referred by that page.
-  If your MR introduces new dependencies, follow the
   :ref:`additional instructions <adding_new_dependencies>`.
-  Check whether your changes need to be reflected in changes to the
   corresponding README file (the one in the directory of the patched
   files). If your changes concern several directories, check all the
   corresponding README files.
-  For parts that have specifications in the repository (e.g., Michelson),
   make sure to keep them in sync with the implementation.
-  If your contribution has an impact on users, e.g. if it fixes a bug,
   adds a new feature, changes an RPC or the command-line interface,
   add an item to the changelog in the :src:`CHANGES.rst` file located
   at the root of the repository.

.. _creating_MR:

Creating the Merge Request
--------------------------

Your goal is to help the reviewers convince themselves that your patch
should be merged.
Well-documented merge requests will receive feedback faster.
Complicated patches with no comments to help the reviewer will cause
the reviewer to make the wrong decision or will discourage the
reviewer to work on the MR.

Therefore, when creating your MR, observe the following rules:

- *Give it an appropriate title*.

- *Give context*: why was this patch written?

  - Does it fix a bug, add a feature or refactor existing code?
  - Is there an open issue on GitLab, or a post from an angry user
    somewhere?
  - Must it be merged before another merge request?

- *Test*:

  - Explain how you tested your patch (or why you didn't).

  - The description of merge requests must include instructions for
    how to manually test them, when applicable.

  - Merge requests should include automated tests for new
    functionality and bug fixes.

    - Refer to the :doc:`testing guide <testing>` for more information.

    - Bug fixes should include a test that demonstrates that the bug has been fixed
      (i.e. that fails before the supplied fix).

    - The :ref:`test coverage <measuring-test-coverage>` can be used to
      guide testing of the proposed MR. If the modified code lacks
      coverage, then this indicates that tests should be added.

    - If no tests are included, a justification should be given in the
      description. Possible justifications include that testing is
      prohibitively difficult, or that the modified code is already
      well-exercised by the existing test suite. The point of the
      justification is to stress the importance of testing and to guide
      improvements of the test framework.

- *Divide and conquer*: it is easier to merge several simple commits than a big one.

  - Isolate complicated parts of your patch in their own commits.
  - Put simple, non-controversial commits first. For instance: commits
    that fix typos, improve documentation, or are simple enough that
    we may want to merge them even without the rest of the merge
    request.
    Even better put them in a separate MR which can be merged easily.
  - Split your commits so that each step is convincing on its own, like
    the proof of a big theorem which is split into several lemmas.
  - Avoid merge requests that are too large. They are harder to rebase and
    request a longer continuous time for reviewing, making them overall slower
    to merge. See :ref:`favoring small merge requests <favoring_small_mrs>`
    below for more details.

- *Anticipate questions*: explain anything which may look surprising, as comments in the code itself if it has value to future readers, or in the MR description.

- *MR Labels*: Add GitLab labels to the MR, like ``doc`` or ``protocol``.

  * The following special labels can be used to trigger different parts of the CI pipeline. To take effect, the label must
    be added before any push action is made on the MR.

    + ``ci--opam`` is for triggering the opam packaging tests pipeline.
    + ``ci--docs`` is for testing some scripts in the documentation (e.g. Octez installation scenarios).
    + ``ci--docker`` is for publishing the Docker image of the MR.
    + ``ci--arm64`` is for building on the ARM64 architecture.
    + ``ci--no-coverage`` disables the job ``unified_coverage`` (but it does not disable coverage instrumentation).


- *MR Options*: When opening an MR you should probably tick the following
  options:

  + ``Delete source branch when merge request is accepted.``
    Helps keeping the repository clean of old branches.
  + ``Squash commits when merge request is accepted.``
    Sometimes it's useful to have many small commits to ease the
    review and see the story of a branch, but they are not relevant
    for the history of the project. In this case they can be squashed
    and replaced with a single meaningful commit. Nevertheless, you
    should squash yourself all fix-up commits when all discussions are resolved,
    as described above in the :ref:`MR workflow <mr_workflow>`, in order
    to ease the reviewers' task.
  + ``Allow commits from members who can merge to the target branch.``
    This option is useful to allow members of the :doc:`Octez merge team <merge_team>`, who are
    not developers in your project, to commit to your branch.
    It helps to rebase and propose fixes.

- *Find reviewers*: it is the responsibility of the author to find
  suitable reviewers. In this context,
  finding a reviewer means finding someone that agreed to review in the
  next days after the MR becomes ready.
  Opting for a reviewer that is not in the capacity to review your MR
  in the next days when others can is unfortunate, because
  the merge request will become unnecessarily blocked; which is bad for:

  - the author, as their work gets delayed, and
  - the health of the repository, as it gives the impression that a new contribution
    will land soon; while it is not the case.

  To find reviewers that will review promptly, we refer to the documentation
  of the :ref:`reviewer field <reviewers_field>` below.

- *Check progress*:
  It is important to maintain to a minimum the number of your open MRs,
  and to constantly check that the discussion is progressing.

Example of an MR with a good, clean history (each bullet is a commit,
any subitems represent the longer description of that commit)::

  * Doc: mark bug #13 as fixed
  * Test_python: add test for p2p bug #13
  * Shell: fix p2p bug #13
    - fix bug in the shell
    - fix relative unit test
    - add docstrings

**Beware**: For MRs touching
``src/proto_alpha/lib_protocol``, see :ref:`protocol MRs <protocol_mr>`.

.. _favoring_small_mrs:

Favoring Small Merge Requests
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Small merge requests are encouraged for multiple reasons:

- They are faster to review, which encourage reviewers to pick them.
- They are easier to rebase, hereby saving developers time.
- They are reviewed more thoroughly.
- If the merge request is not accepted, less work is lost; in particular
  less review time has been spent.

However, small merge requests also come with drawbacks:

- They make it more difficult for reviewers to get the global picture of the intended change.
- They may introduce intermediate states, during which a feature
  is not yet finished; or dead code is temporarily introduced.
- They have to be reverted if the entire feature is ultimately cancelled.

For ``tezos/tezos`` to evolve fast, however, we are convinced that the advantages
of small merge requests outweigh the drawbacks. If possible, drawbacks
must be mitigated as follows:

- Have the entire piece of work described or done somewhere. For example in
  an issue, or a branch containing the entire change, or a
  large (unsplit) work as a draft merge request.
  For complex works, an external document may be referred in the issue/MR, detailing the design/implementation rationale; if such documents are only targeted to reviewers and/or are only describing a *change*, they should not go in the online documentation.
- Include a link to the entire piece of work in the description of each
  small merge requests created by splitting the large piece of work.
  This will help reviewers get the big picture.
- Explain why the intermediate state is harmless, if applicable.
- To mitigate loss of work if the whole piece is not accepted,
  we advise to split the work so that improvements that are desirable on their own
  are the first ones to be merged in the sequence of small merge requests.
  A desirable standalone improvement is for example a refactoring that
  improves the quality of the code, or adds new tests, or fixes typos.

.. _assignee_field:

Merge Request "Assignees" Field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Merge requests targeted at ``tezos/tezos master`` should in general
have exactly one assignee. The assignee is someone from which an
action is required to get the merge request moving. Example actions include:

- review;
- respond to a comment thread;
- update the code;
- rebase (in particular in case of conflicts);
- merge;
- find someone else who can get the merge request moving.

The assignee will thus often be one of the reviewers (if they needs to review
or respond to a comment) or one of the merge request authors (if they need
to update the code or respond to a comment).

If a merge request has no assignee, it is implicitly the role of the
:ref:`merge coordinator <merge_coordinator>` to assign it to someone.

Even though merge requests could require action from several people
to be merged, we avoid assigning more than one to avoid diluting responsibility.

.. _reviewers_field:

Merge Request "Reviewers" Field
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reviewer field of GitLab is used to specify reviewers.
Once the merge request is ready for review,
:ref:`assign <assignee_field>` one of the reviewers that
you specified in the reviewers field.
As mentioned previously, it is the responsibility of authors to find reviewers.
To find reviewers, either:

  - Advertize your merge request on the ``#mr-advertizing`` channel of
    the `tezos-dev <https://tezos-dev.slack.com/>`_ Slack. Good advertisement
    consists of a link to the MR and a one sentence summary.
  - Look at authors of the code you are modifying using
    `git blame <https://git-scm.com/docs/git-blame>`_.
  - Ask help to the :ref:`merge coordinator <merge_coordinator>`, either
    by asking them on Slack or mentioning them in a comment (see next paragraph).

Depending on your `GitLab role <https://docs.gitlab.com/ee/user/permissions.html>`_
you may or may not be able to use the *Reviewers* field for specifying
the reviewers. If you don't have the right, mention the reviewers using
their GitLab handle (username prefixed with ``@``) in a comment.
It causes GitLab to send a notification to them.

.. _draft_mode:

Merge Request "Draft" Mode
~~~~~~~~~~~~~~~~~~~~~~~~~~

A merge request that is not yet ready to be merged should be marked as
`draft
<https://docs.gitlab.com/ee/user/project/merge_requests/drafts.html>`_
by prefixing its title with ``Draft:``. Typical reasons for this is
that the author still has some minor tasks to accomplish, such as
cleaning up the merge request's ``git`` history (e.g. squashing ``fixup!`` commits), even
if the request is already reviewed and approved (and in this case, the draft status will prevent
the request from being prematurely merged).

Being in ``Draft:`` mode does not signal "not yet ready for
review". Instead, MRs that should not be reviewed should be in the ``Draft`` state and
assigned to the author, as it is the MR's assignee that should take action
next.

To decide the correct draft status with respect to whether the MR is
ready for review, consider the following table:

.. table::

    +----------+-------------+----------------+
    | Assignee | Has fixups? | Correct status |
    +==========+=============+================+
    | Other    | No          | \              |
    +----------+-------------+----------------+
    | Other    | Yes         | Draft          |
    +----------+-------------+----------------+
    | Author   | No          | Draft          |
    +----------+-------------+----------------+
    | Author   | Yes         | Draft          |
    +----------+-------------+----------------+

In sum: the correct status for an MR assigned to another person than
the author should be ``Draft`` if its history contains ``fixup!`` or
if other development tasks remain. If the MR is assigned to the
author, then it should be ``Draft``.

Merge Request's owner
~~~~~~~~~~~~~~~~~~~~~

Usually the person who has created a Merge Request is also responsible for
pushing it forward: finding reviewers, addressing their comments and so on.
Occasionally though it happens that the author has to move to more pressing
tasks and hands his MR over to another person to finish. This is a problem,
because an MR assigned to its author has sort of a special status – it
informs everyone that the branch is likely to change at any moment. When an
MR is handed over, we lose that important information.

For this reason, when take-over happens, it's useful to make that explicit.
The person taking over should put a comment informing about this so that
everyone knows that the person attending to the MR changes. Also the
description should be updated to reflect that information. It is suggested to
prefix the description with a line specifying the MR's owner if that's
different to the author indicated by GitLab.

.. _adding_new_dependencies:

Special case: MRs that introduce a new dependency
-------------------------------------------------

In the special case where your MR adds a new opam dependency or updates an
existing opam dependency, you will need to follow
this additional dedicated guide:

.. toctree::
   :maxdepth: 2

   contributing-adding-a-new-opam-dependency

In the special case where your MR adds a new Python, Rust, JavaScript, or other
dependency, additional steps must also be followed.

* for Python, you can refer to the related section in the :ref:`python testing documentation <python_adding_new_dependencies>`.
* the Rust dependencies are located in the GitLab repository `tezos-rust-libs <https://gitlab.com/tezos/tezos-rust-libs>`_ and the instructions are listed there.

For others, there is currently no dedicated guide. Do not hesitate to ask for
help on the ``#devteam`` channel on the `tezos-dev <https://tezos-dev.slack.com/>`_ Slack.

.. _protocol_mr:

Protocol development MRs
------------------------

Because of the amendment procedure that governs the protocol, the
workflow for protocol development is significantly different from
master.

Protocol snapshots
~~~~~~~~~~~~~~~~~~

Before a proposal, a new directory, e.g. ``proto-005-PsBabyM1/``, is
created from ``proto_alpha/`` where the development continues.
These directories are called *protocol snapshots*.

The hash of each active or candidate protocol is computed from the directory
``src/proto_0*/lib_protocol/``, so every change in these directories
is forbidden.
However, the protocol snapshots contain other subdirectories than the frozen ``lib_protocol/``, implementing protocol-dependent code. For instance, ``lib_client/`` implements commands in the Octez client that can invoke protocol-dependent functionalities, such as transfers between accounts.

Thereby, if your MR changes code in the protocol snapshot directories (outside the frozen ``lib_protocol/``), you may consider patching the corresponding code for the other protocols as well (at least the ones which are still in use), when applicable.

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
When submitting an MR which contains migration code, **the author must also have tested the migration** (see :doc:`proposal_testing`) and write in the
description what was tested and how so that **reviewers can reproduce it**.


.. _code_review:

Code Review
-----------

All code is peer reviewed before getting committed in the
master branch by the :doc:`Octez merge team <merge_team>`.
Briefly, a code review is a discussion between two or
more developers about changes to the code to address an issue.

Merge Request Approvals
~~~~~~~~~~~~~~~~~~~~~~~

Two approvals from different Octez :doc:`Octez merge team <merge_team>` members are required for merge
requests to be merged. Both approvals must result from independent, thorough
reviews. After both reviews, the second approver will also typically merge if the request is not a draft
(which otherwise means that the author has still some cleaning up to do).

However, for less critical parts of the code, an Octez merge team member may
choose to trust the review of a developer who is not a member of the Octez
merge team. In that case, the Octez merge team member may choose to count that
review as their own, effectively "converting" the other developer's passing
review into an approval. This may be done even when the Octez merge team
member is the author of the merge request. A second independent review from an
Octez merge team member is still required, of course.

For this reason, if you make a partial review, for instance if you only
reviewed part of the code, or only the general design, it is good practice
to say so in a comment, so that other reviewers know what is left to review.
If you manually tested the merge request or ran some benchmarks,
you can add a comment with the results.

Author Perspective
~~~~~~~~~~~~~~~~~~

Code review is a tool among others to enhance the quality of the code and to
reduce the likelihood of introducing new bugs in the code base. It is a
technical discussion; not an exam, but rather a common effort to learn
from each other.

These are a few common suggestions we often give while reviewing new code.
Addressing these points beforehand makes the reviewing process easier and less
painful for everybody. The reviewer is your ally, not your enemy.

- Commented code: Did I remove any commented out lines?
  Did I leave a :ref:`TODO/FIXME comment <todo_fixme>` without an issue number?

- Docstrings: Did I export a new function? Each exported
  function should be documented in the corresponding ``.mli`` (or directly in the ``.ml`` file if there is no ``.mli``).

- README: Did I check whether my changes impact the corresponding README
  file(s)?

- Readability: Is the code easy to understand? Is it worth adding
  a comment to the code to explain a particular operation and its
  repercussion on the rest of the code?

- Variable and function names: These should be meaningful and in line
  with the conventions adopted in the code base.

- Testing: Are the tests thoughtful? Do they cover the failure conditions? Are
  they easy to read? How fragile are they? How big are the tests? Are they slow?

- Are your commit messages meaningful? (see https://cbea.ms/git-commit/)

Review your own code before calling for a peer review from a colleague.

Reviewer Perspective
~~~~~~~~~~~~~~~~~~~~

Code review can be challenging at times. These are suggestions and common
pitfalls a code reviewer should avoid.

- Ask questions: How does this function work? If this requirement changes,
  what else would have to change? How could we make this more maintainable?

- Discuss in person for more detailed points: Online comments are useful for
  focused technical questions. On many occasions it is more productive to
  discuss it in person rather than in the comments. Similarly, if discussion
  about a point goes back and forth, it will be often more productive to pick
  it up in person and finish out the discussion.

- Explain reasoning: Sometimes it is best to both ask if there is a better
  alternative and at the same time justify why a problem in the code is worth
  fixing. Sometimes it can feel like the changes suggested are nit-picky
  without context or explanation.

- Make it about the code: It is easy to take notes from code reviews
  personally, especially if we take pride in our work. It is best to make
  discussions about the code than about the developer. It lowers resistance and
  it is not about the developer anyway, it is about improving the quality of
  the code.

- Suggest importance of fixes: While offering many suggestions at once, it is
  important to also clarify that not all of them need to be acted upon and some
  are more important than others. It gives an important guidance to the developer
  to improve their work incrementally.

- When you consider that a fix is important but should not prevent the current MR to be merged (e.g., because it adds a sufficient amount of useful new features), you may suggest creating a follow-up issue.
  If the place in the code that needs to be fixed later is clear, you may also suggest marking it with a :ref:`TODO/FIXME comment <todo_fixme>`.

- Take the developer's opinion into consideration: Imposing a particular design
  choice out of personal preferences and without a real explanation will
  incentivize the developer to be a passive executor instead of a creative agent.

- Do not re-write, remove or re-do all the work: Sometimes it is easier to
  re-do the work yourself discarding the work of the developer. This can give
  the impression that the work of the developer is worthless and adds
  additional work for the reviewer that effectively takes responsibility for
  the code.

- Consider the person you are reviewing: Each developer is a person. If you
  know the person, consider their personality and experience while reviewing their
  code. Sometimes it is possible with somebody to be more direct and terse, while
  other people require a more thorough explanation.

- Avoid confrontational and authoritative language: The way we communicate has
  an impact on the receiver. If communicating a problem in the code or a
  suggestion is the goal, making an effort to remove all possible noise from
  the message is important. Consider these two statements to communicate about
  a problem in the code : "This operation is wrong. Please fix it." and
  "Doing this operation might result in an error, can you please
  review it?". The first one implies you made an error (confrontational), and
  you should fix it (authority). The second suggests to review the code because
  there might be a mistake. Despite the message being the same, the recipient might
  have a different reaction to it and impact on the quality of this work. This
  general remark is valid for any comment.

When reviewing MRs involving documentation, you may check the built documentation directly within the GitLab interface, see :ref:`build_doc_ci`.

.. _merge_bot:

The Merge-Request Bot
---------------------

Every 6 hours, an automated process running as the
`Tezbocop <https://gitlab.com/tezbocop>`__ 🤖 user, inspects recent MRs and posts
or edits comments on them, giving an inspection report on the contents of the
MR.

Some warnings/comments are for you to potentially improve your MR, other
comments just help us in the assignment & review process.

The first time Tezbocop posts a message you should receive a notification; for
the subsequent edits there won't be notifications; feel free to check Tezbocop's
comments any time.

If you think some of the remarks/warnings do not apply to your MR feel free to
add a comment to justify it.

In particular, the Merge-Request Bot may complain about :ref:`TODO/FIXME comments <todo_fixme>` without an issue number ensuring that the intended evolution is tracked.

The code for the bot is at
`oxheadalpha/merbocop <https://gitlab.com/oxheadalpha/merbocop>`__. It is of
course work-in-progress and new warnings and comments will appear little by
little. We welcome specific issues or contributions there too.

.. _dev_tools:

Developer Tools
~~~~~~~~~~~~~~~

Some tools to make protocol development more convenient can be found in the :src:`src/tooling/` folder.
In particular, it contains ``tztop``, a REPL (interactive read-eval-print loop) based on ``utop``.
