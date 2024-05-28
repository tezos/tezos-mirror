Pre-Commit Hook
===============

`Pre-commit <https://pre-commit.com/>`_ is a framework for managing and maintaining multi-language pre-commit Git hooks.

Using this framework you can specify a list of hooks you want and pre-commit manages the installation and execution of any hooks before every commit. pre-commit is specifically designed to not require root access.

Before you can run hooks, you need to have the pre-commit package manager installed.
There are multiple ways of installing ``pre-commit``, either via ``apt`` in Debian or using other `installation options <https://pre-commit.com/#installation>`__ on the pre-commit website.

Pre-commit is configured via a simple configuration file :src:`.pre-commit-config.yaml` that is kept in sync by the Tezos developers with the best practice used on the Tezos repository.
Installing ``pre-commit`` will minimize CI failure, by avoiding failures that can be detected by sanity checks before pushing upstream.

We can now install the git hooks using the command ``pre-commit install``.

From now on before every commit, the git hook will be run by ``pre-commit`` and make sure all checks are green.

You can find more information and advanced uses on https://pre-commit.com/.
