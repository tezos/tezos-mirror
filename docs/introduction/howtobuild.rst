Building Octez from source
==========================

.. _build_from_sources:
.. _compiling_with_make:

If you plan to contribute to the Octez codebase or if you want to build a version of Octez that is based on the most recent code, the way to go is to set up a complete development environment by cloning the repository and compiling the sources using the provided makefile.

Note that setting up a complete development environment requires a powerful machine, and the required resources are increasing in the long run.
If you are using Octez for production, consider installing forms of prebuilt binaries, refer to :doc:`howtoget`.

You can either build all the executables, as illustrated below, or only a subset of the executables, as detailed in section :ref:`compile_sources`.

**TL;DR**: From a fresh Debian Bookworm x86_64, you typically want to select a source branch in the Octez repository, e.g.:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [select branch]
  :end-before: [end]

and then do:

.. literalinclude:: compile-sources-setup.sh
  :language: shell
  :start-after: [install packages]
  :end-before: [get sources]


.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [get sources]
  :end-before: [end init opam]


.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [make build-deps]
  :end-before: [test executables]
.. warning::

  If you rerun the procedure above in an already cloned repository, typically by restarting from ``git checkout $BRANCH`` after upgrading the sources to a new version of Octez, you should do a ``git fetch`` before, to ensure that the tags for the latest release are known to Git. Otherwise, the sources compile perfectly fine but Octez executables may report their version incorrectly (e.g. ``21.2+dev`` instead of ``22.0``).

The following sections describe the individual steps above in more detail.

.. note::

  Besides compiling the sources, it is recommended to also :ref:`install Python and some related tools <install_python>`, which are needed, among others, to build the documentation and to use the Git :doc:`pre-commit hook <../developer/pre_commit_hook>`.

.. _setup_rust:

Install Rust
~~~~~~~~~~~~

Compiling Octez requires the Rust compiler (see recommended version in variable
``$recommended_rust_version`` in file ``scripts/version.sh``) and the
Cargo package manager to be installed. If you have `rustup
<https://rustup.rs/>`_ installed, it should work without any
additional steps on your side. You can use `rustup
<https://rustup.rs/>`_ to install both. If you do not have ``rustup``,
please avoid installing it from Snapcraft; you can rather follow the
simple installation process shown below:

.. literalinclude:: compile-sources-setup.sh
  :language: shell
  :start-after: [install rust]
  :end-before: [get sources]

Once Rust is installed, note that your ``PATH`` environment variable
(in ``.profile``) may be updated and you will need to restart your session
so that changes can be taken into account. Alternatively, you can do it
manually without restarting your session:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [source cargo]
  :end-before: [get sources]

Note that the command line above assumes that rustup
installed Cargo in ``$HOME/.cargo``, but this may change depending on how
you installed rustup. See the documentation of your rustup distribution
if file ``.cargo`` does not exist in your home directory.

.. _setup_zcash_params:

Install Zcash Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

Octez binaries require the Zcash parameter files to run.
Docker images come with those files, and the source distribution also
includes those files. But if you compile from source and move Octez to
another location (such as ``/usr/local/bin``), the Octez binaries may
prompt you to install the Zcash parameter files. The easiest way is to
download and run this script::

   wget https://raw.githubusercontent.com/zcash/zcash/713fc761dd9cf4c9087c37b078bdeab98697bad2/zcutil/fetch-params.sh
   chmod +x fetch-params.sh
   ./fetch-params.sh

The node will try to find Zcash parameters in the following directories,
in this order:

#. ``$XDG_DATA_HOME/.local/share/zcash-params``
#. ``$XDG_DATA_DIRS/zcash-params`` (if ``$XDG_DATA_DIRS`` contains
   several paths separated by colons ``:``, each path is considered)
#. ``$OPAM_SWITCH_PREFIX/share/zcash-params``
#. ``./_opam/share/zcash-params``
#. ``~/.zcash-params``
#. ``~/.local/share/zcash-params``
#. ``/usr/local/share/zcash-params``
#. ``/usr/share/zcash-params``

If the node complains that it cannot find Zcash parameters, check that
at least one of those directories contains both files ``sapling-spend.params``
and ``sapling-output.params``. Here is where you should expect to find those files:

* if you are compiling from source, parameters should be in
  ``_opam/share/zcash-params`` (you may need to run ``eval $(opam env)``
  before running the node);

* if you used ``fetch-params.sh``, parameters should be in ``~/.zcash-params``.

.. note::

   Some operating systems may not be covered by the list of directories above.
   If Zcash is located elsewhere on your system (typically, on MacOS X), you may try creating a symbolic link such as: ``ln -s ~/Library/Application\ Support/ZcashParams ~/.zcash-params``.

Note that the script ``fetch-params.sh`` downloads a third file containing parameters for Sprout (currently called ``sprout-groth16.params``), which is not loaded by Sapling and can be deleted to save a significant amount of space (this file is *much* bigger than the two other files).

.. _setup_dal_crypto_params:

Install DAL trusted setup
~~~~~~~~~~~~~~~~~~~~~~~~~

Users running :doc:`DAL<../shell/dal>` in **operator or observer** :ref:`profiles<dal_profiles>`
need to have a set of cryptographic parameters (known as an SRS) installed in
order to run their :doc:`DAL node<../shell/dal_node>`.
In particular, these are needed when executing the Octez test suite, which involves DAL nodes running in various profiles.
However, for simplicity, on some test networks the initialization parameters are mocked-up and built-in.

The cryptographic parameters can be retrieved via the following script::

  scripts/install_dal_trusted_setup.sh

Get the sources
~~~~~~~~~~~~~~~

Octez ``git`` repository is hosted at `GitLab
<https://gitlab.com/tezos/tezos/>`_. All development happens here. Do
**not** use our `GitHub mirror <https://github.com/tezos/tezos>`_
which we don't use anymore and only mirrors what happens on GitLab.

Checkout the ``latest-release`` branch to use the latest release.
Alternatively, you can checkout a specific version based on its tag.

Install Octez dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install the OCaml compiler and the libraries that Octez depends on::

   make build-deps

Alternatively, if you want to install extra
development packages such as ``merlin``, you may use the following
command instead:

::

   make build-dev-deps

.. note::

   * These commands create a local OPAM switch (``_opam`` folder at the root
     of the repository) where the required version of OCaml and OCaml Octez
     dependencies are compiled and installed (this takes a while but it's
     only done once).

   * Be sure to ``eval $(scripts/env.sh)`` when you ``cd``
     into the repository in order to be sure to load this local
     environment.

   * As the opam hook would overwrite the effects of ``eval $(scripts/env.sh)``
     the script will disable the opam hook temporarily.

   * OPAM is meant to handle correctly the OCaml libraries but it is
     not always able to handle all external C libraries we depend
     on. On most systems, it is able to suggest a call to the system
     package manager but it currently does not handle version checking.

   * As a last resort, removing the ``_opam`` folder (as part of a ``git
     clean -dxf`` for example) allows to restart in a fresh environment.

.. _compile_sources:

Compile
~~~~~~~

Once the dependencies are installed we can update OPAM's environment to
refer to the new switch and compile the project to build all the executables:

.. literalinclude:: compile-sources.sh
  :language: shell
  :start-after: [compile sources]
  :end-before: [optional setup]

.. note::

  Instead of the simple ``make`` command above, you may use more restrictive targets in :src:`the makefile <Makefile>` to build only some subset of the executables.
  For instance, you may exclude experimental executables using ``make release``; furthermore exclude executables such as the EVM node using ``make octez``; or even restrict to Layer 1 executables using ``make octez-layer1``.

Lastly, you can also add the Octez binaries to your ``PATH`` variable,
and after reading the Disclaimer a few
hundred times you are allowed to disable it with
``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y``.

You may also activate Bash autocompletion by executing::

  source ./src/bin_client/bash-completion.sh

.. warning::

  Note that if your shell is ``zsh``, you may need extra configuration to customize shell
  completion (refer to the ``zsh`` documentation).

.. _update_from_sources:

Update
~~~~~~

For updating to a new version, you typically have to
update the sources by doing ``git pull`` in the ``tezos/`` directory and replay
the compilation scenario starting from ``make build-deps``.
You may also use ``make clean`` (and ``rm -Rf _opam/`` if needed) before that, for restarting compilation in a
fresh state.
