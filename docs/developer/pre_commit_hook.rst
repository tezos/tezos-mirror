Pre-Commit Hook
===============

The `pre-commit hook <https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks>`__
is a script located in :src:`scripts/pre_commit/pre_commit.py`, automatically
executed before any Git commit.
It executes modified :doc:`Tezt <tezt>` tests automatically. It looks for staged files
(the default) or modified files (if ``--unstaged`` is passed) in
``tezt/tests`` and calls ``tezt`` on those files. This avoids
pushing commits that will break the CI. It is also handy to execute
the relevant subset of tests by calling
``./scripts/pre_commit/pre_commit.py [--unstaged]`` manually.

Using the pre-commit hook requires an installed Python environment, as
described in :doc:`python_environment`.

We refer to the header of ``pre_commit.py`` and its ``--help`` flag
for additional instructions.

Using pre-commit.com
~~~~~~~~~~~~~~~~~~~~

`Pre-commit <https://pre-commit.com/>`_ is a framework for managing and maintaining multi-language pre-commit Git hooks.

Using this framework you can specify a list of hooks you want and pre-commit manages the installation and execution of any hooks before every commit. pre-commit is specifically designed to not require root access.

Before you can run hooks, you need to have the pre-commit package manager installed.
There are multiple ways of installing ``pre-commit``, either via ``apt`` in Debian or using other `installation options <https://pre-commit.com/#installation>`__ on the pre-commit website.

Pre-commit is configured via a simple configuration file :src:`.pre-commit-config.yaml` that is kept in sync by the Tezos developers with the best practice used on the Tezos repository.
Installing ``pre-commit`` will minimize CI failure, by avoiding failures that can be detected by sanity checks before pushing upstream.

We can now install the git hooks using the command ``pre-commit install``.

From now on before every commit, the git hook will be run by ``pre-commit`` and make sure all checks are green.

You can find more information and advanced uses on https://pre-commit.com/.
