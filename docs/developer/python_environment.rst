Python Environment
==================

Octez uses Python to build this documentation website (:doc:`https://tezos.gitlab.io <../index>`) and for a limited set of
utility scripts (although this latter usage of Python is deprecated).
This page contains installation instructions for the Python environment.

.. _install_python:

Installation
------------

Prerequisites:

- ``python 3.11.8``. It is recommended to use `pyenv
  <https://github.com/pyenv/pyenv>`_ to manage the python versions.
  If you want to use ``pyenv``:

  * Follow the `installation instructions <https://github.com/pyenv/pyenv/#installation>`__.
    In particular, this ensures that ``eval "$(pyenv init -)"``
    has been executed first during the shell session, by adding this line to an
    environment script sourced automatically.

  * You can use then ``pyenv install 3.11.8``, followed by:

    + ``pyenv local 3.11.8`` to use ``python 3.11.8`` only in the current directory (and its subdirectories, unless redefined)
    + ``pyenv global 3.11.8`` to set the python version to ``3.11.8`` globally
    + ``pyenv shell 3.11.8`` to use ``python 3.11.8`` only in the current shell

- `poetry 1.2.2 <https://python-poetry.org/>`_ to manage the python dependencies and
  run the tests in a sandboxed python environment. Follow the `installation instructions <https://python-poetry.org/docs/#installation>`__.

  Before using the python environment for the first time, the
  dependencies must be installed. To achieve this, run ``poetry install``
  in the root of the project.

A typical installation of the above prerequisites (including their own prerequisites) proceeds as follows, see below for full details::

  # 1. install pyenv
  # 2. restart shell, to ensure "pyenv init -" has been evaluated
  # 3. then install python using pyenv:
  pyenv install 3.11.8
  pyenv global 3.11.8
  # 4. install poetry
  # 5. restart shell, to activate the poetry setup
  # 6. then install dependencies for Octez using poetry:
  cd tezos/
  poetry install

Installation details for Ubuntu 20.04
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, make sure ``curl`` and ``git`` are installed, as they are required by the ``pyenv`` and ``poetry`` installers:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install pyenv system dependencies]
   :end-before: [install python build dependencies]

For ``pyenv`` to compile Python, you'll have to install `python's build dependencies <https://github.com/pyenv/pyenv/wiki#suggested-build-environment>`__:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install python build dependencies]
   :end-before: [install pyenv]

Now, install ``pyenv`` using `pyenv-installer <https://github.com/pyenv/pyenv-installer>`__:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install pyenv]
   :end-before: [setup shell for pyenv]

To make ``pyenv`` available in your shell session, `add the following
to your shell configuration file <https://github.com/pyenv/pyenv/#set-up-your-shell-environment-for-pyenv>`__,
e.g. ``~/.bashrc`` or ``~/.zshrc``:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [setup shell for pyenv]
   :end-before: [print pyenv version]

To verify the ``pyenv`` installation, restart your terminal and try
executing ``pyenv``:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [print pyenv version]
   :end-before: [verify pyenv installation]

Now we can use ``pyenv`` to install Python 3.11.8 and set it as the
default version:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install python through pyenv]
   :end-before: [print python version]

Now verify that the correct version is called when running ``python``:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [print python version]
   :end-before: [verify python version]

Now, on to installing ``poetry``. We'll use `poetry's official installer <https://python-poetry.org/docs/master/#installing-with-the-official-installer>`__:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install poetry]
   :end-before: [setup shell for poetry]

As for ``pyenv``, we need to do some shell setup to put ``poetry`` in
the shells path. Add the following to your shell configuration file,
e.g. ``~/.bashrc`` or ``~/.zshrc``:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [setup shell for poetry]
   :end-before: [print poetry version]

Restart the terminal and verify that the correct version is called
when running ``poetry``:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [print poetry version]
   :end-before: [verify poetry version]

Finally, let's use ``poetry`` to install the python dependencies of
Octez. This command needs to run in the root of the Octez checkout:

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [install octez python dev-dependencies]
   :end-before: [print sphinx-build versions]

If the installation went well, you should now have the correct version
when executing poetry in the Octez checkout for our main dependency
(``sphinx-build`` for documentation):

.. literalinclude:: install-python-debian-ubuntu.sh
   :language: shell
   :start-after: [print sphinx-build versions]
   :end-before: [verify sphinx-build version]

.. _python_adding_new_dependencies:

Adding new dependencies
-----------------------

Dependencies are managed by poetry in the file :src:`pyproject.toml`.
See `the reference for the pyproject.toml files <https://python-poetry.org/docs/pyproject/>`__.
The file :src:`poetry.lock` is generated by running ``poetry lock``, and must never be changed manually.
The resulting ``poetry.lock`` and its generator ``pyproject.toml`` must be
copied in `this repository <https://gitlab.com/tezos/opam-repository>`__.
