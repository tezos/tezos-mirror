.. _merge_team:

Merge Team
==========

The merge team is a `group of developers
<https://gitlab.com/tezos/tezos/-/project_members>`_
from different teams of the
Tezos ecosystem that controls what is merged into the
https://gitlab.com/tezos/tezos/ repository.
They are automatically selected as approvers when you create a merge
request.

.. _merge_dispatcher:

Merge Dispatcher
----------------

Every week, a dedicated member of the merge team, called the *merge dispatcher*,
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
