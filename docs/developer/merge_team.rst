Octez Merge Team
================

The Octez merge team is a `group of developers
<https://gitlab.com/tezos/tezos/-/project_members>`_
from different teams of the
Tezos ecosystem that ensures quality of what is merged into the
https://gitlab.com/tezos/tezos/ repository.
They are automatically selected as approvers when you create a merge
request.

.. _merge_dispatcher:

Merge Dispatcher
----------------

Every week, a dedicated member of the Octez merge team, called the *merge dispatcher*,
is in charge of making sure merge requests get merged.
Their role is to:

- make sure merge requests follow the :ref:`MR workflow <mr_workflow>`;
- update labels;
- ask people to review, fix issues, etc.;
- make sure that merge requests are assigned to someone;
- merge the merge requests that are ready.

Each week, :gl:`Issue 1062 <tezos/tezos#1062>` is
re-assigned to the current merge dispatcher to make it easy to find out
who it is. Don't hesitate to contact the merge dispatcher if you need help
to get your merge request moving.

Getting into the Octez Merge Team
---------------------------------

Admission to the Octez merge team is done by cooptation.
The Octez merge team is always looking for software engineers with at least the following qualities:

- You are an active developer on the `tezos/tezos
  <https://gitlab.com/tezos/tezos>`_ repository.
- You have done numerous reviews and merge requests in the past, i.e. you have
  shown your expertise and interest in contributing both with code and reviews.
- You are paying attention to detail and are on the lookout for possible
  errors, security issues, and/or enhancements in your reviews. It's fine not knowing
  the entire codebase (no one does), but you should nevertheless
  actively try to make the code of others better when you do reviews.
- You are well capable in software design and can identify subpar design choices.
- One way to show attention to detail and to design is to show these
  qualities in your own merge requests. You should also be willing to amend your merge
  requests by taking into account the feedback of reviewers, be it
  for small changes or architectural changes.
- You know when your expertise is insufficient and you are keen to
  ask for support.
- You are relatively skilled in one of the technologies used in the ``tezos/tezos``
  repository, i.e. ``OCaml``, ``python``, ``CI``, building, packaging, etc.

Helping the Octez Merge Team
----------------------------

Even if you are not in the Octez merge team, you can help it! If you review a merge
request and you think it is ready to be merged, you should click the *Approve* button
even if your approval "doesn't count".

If the merge request is not critical,
or if you are knowledgeable on the merge request topic; your approval
will make it easier for an Octez merge team member to approve too.
This is also an excellent way to show your skills and willingness to move development
forward, contributing to potentially making you an Octez merge team member in the future
(see *Getting into the Octez Merge Team* above).
