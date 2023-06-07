Pre-Commit Hook
===============

The `pre-commit <https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks>`_
hook located in :src:`scripts/pre_commit/pre_commit.py`
executes modified :doc:`Tezt <tezt>` tests automatically. It looks for staged files
(the default) or modified files (if ``--unstaged`` is passed) in
``tezt/tests`` and calls ``tezt`` on those files. This avoids
pushing commits that will break the CI. It is also handy to execute
the relevant subset of tests by calling
``./scripts/pre_commit/pre_commit.py [--unstaged]`` manually.

Using the pre-commit hook requires an installed Python environment, as
described in :doc:`python_environment`.

We refer to the header of ``pre_commit.py`` and its ``--help`` flag
for additional instructions.
