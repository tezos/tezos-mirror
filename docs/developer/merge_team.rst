Octez Merge Team
================

The Octez merge team is a `group of developers
<https://gitlab.com/groups/tezos/-/group_members>`_
from different teams of the
Tezos ecosystem.
They are automatically selected as approvers when you create a merge
request.

Octez Merge Team Scope
----------------------

The role of the Octez Merge Team is limited to ensuring the quality
of what gets merged into Octez, i.e. all of `tezos/tezos
<https://gitlab.com/tezos/tezos>`_ except :src:`src/proto_alpha/lib_protocol/`.
By quality, we mean ensuring that the code is as good as it can be, be it
at the implementation level or the architecture level.

The Octez merge team does not decide what contributions are included in
the next protocol (``src/proto_alpha/lib_protocol/``). Companies that contribute to
`tezos/tezos <https://gitlab.com/tezos/tezos>`_
reach a consensus to decide what gets in the Alpha protocol,
i.e. in the proposal of the next upgrade. In particular, other companies
can fork this repository and do their own protocol proposals.

Getting into the Octez Merge Team
---------------------------------

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

To apply for being included in the Octez merge team, contact
an existing member on the `tezos-dev <https://tezos-dev.slack.com/>`_ Slack. Your application
will be discussed during the next weekly meeting of the Octez merge team.

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
