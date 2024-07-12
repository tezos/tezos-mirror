Installation troubleshooting
============================

This page groups information about known problems when installing Tezos (more precisely, the "Octez" implementation of Tezos software).
The different issues and their solutions are grouped per installation method, except for generic issues concerning all installation scenarios.

This page lists only the most frequent problems.
If you don't find your problem in this page, chances are that the problem is too specific.
Consult the :ref:`Tezos community <tezos_community>` (e.g. the Tezos Stack Exchange) to see if others have encountered a similar problem, and whether a solution is known.

Generic issues
--------------

Case-sensitive file paths
~~~~~~~~~~~~~~~~~~~~~~~~~

On case-insensitive file systems, which is the default on Windows and MacOS, beware of some related problems.

In the Octez repository, there are no conflicting file paths differing only in case.
However, depending on how Git extracts your clone, the case of some file paths in your clone may differ from the expected one.

For example, file :src:`script-inputs/slim-mode-dune` contains a list of source directories (belonging to old protocols) to ignore when building the documentation locally.
If the case of your extracted files does not match the case in that file, you may encounter unexpected behaviors, such as errors when building the documentation on those parts.
(Note that these documentation errors cannot be fixed in old protocols whose code is frozen.)

If you run into such errors, simply rename your file paths to match the case in the source repository.

.. _mixing_llvm_gnu_binutils:

Mixing LLVM and GNU binutils
----------------------------

Mixing LLVM and GNU binutils toolchains can cause issues when building Octez. If you encounter
errors like the following, it may be that you have tools from both LLVM and GNU in scope.

::

  Error: ExternalToolError { reason: "Failed to create archive index with `ranlib`", tool: "ranlib", args: ["liboctez_rust_deps.a"], stdout: "", stderr: "LLVM ERROR: Invalid encoding\n" }

::

  LLVM ERROR: Invalid encoding

::

  bfd plugin: LLVM gold plugin has failed to create LTO module: Opaque pointers are only supported in -opaque-pointers mode (Producer: 'LLVM17.0.4-rust-1.74.0-stable' Reader: 'LLVM 14.0.0')

You can check ``objcopy``, for example, like this.

::

  objcopy --version

If the output of this command indicates an LLVM version of ``objcopy`` and you have encountered
the above error message, then you are mixing toolchains. In this case, you ought to remove, for
example, the LLVM toolchain (e.g. through your system's package manager) or ensure that GNU's
tools and libraries, like ``objcopy``, have higher precedence in your ``$PATH`` environment
variable.

Compiling the sources
---------------------

These issues concern installing by :ref:`compiling the sources <build_from_sources>`.

Currently, the ``CONFIG_SITE`` environment variable must not be
set during the installation process, or the ``stdcompat`` package
may be installed incorrectly. See `thierry-martinez/stdcompat#13
<https://github.com/thierry-martinez/stdcompat/issues/13>`__.
