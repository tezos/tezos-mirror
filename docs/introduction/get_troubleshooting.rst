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

If you have trouble **compiling on MacOS**, update ``brew`` packages and development tools such as Xcode.
If you have recently updated to a new version of MacOS, you may need to reinstall these tools.
Also, if you have built Octez before, ``brew`` may be linked to an older version of PostgreSQL.
In this case, you can force ``brew`` to update its link to the newer version by running ``brew link --overwrite postgresql@15``, for example.

Running out of Memory
~~~~~~~~~~~~~~~~~~~~~

Compiling the sources can be memory demanding.
On Linux-based operating systems, depending on the memory allocation on the machine, the available memory might not suffice to compile the sources; resulting in a crash during the compilation process.
To address this, we describe below how to increase the swap space on a Linux distribution and solve the aforementioned problem.

.. note::
   The following steps were tested on a Debian-based distribution.
   They can usually be adapted to other Linux distributions, though details may differ.
   For example, Fedora uses Btrfs as its default filesystem (since version 33), which requires special handling for swap files (disabling copy-on-write and compression).
   Note that these commands require root privileges and direct access to the host system; they will not normally work inside standard container environments.


What Is Swap Memory?
^^^^^^^^^^^^^^^^^^^^

Swap memory, or swap space, is a portion of the hard drive used by the
system to store data that would normally reside in RAM when physical
memory is fully utilised.

It can be implemented as a **file** (e.g., ``/swapfile``) or as a
**dedicated partition**.

How Much Swap Memory Is Required for Tezos?
'''''''''''''''''''''''''''''''''''''''''''

Swap memory is essential for compiling the Octez suite, particularly during
critical stages such as:
- Compiling Rust dependencies (``wasm-toolchain``, ``cargo``).
- Linking OCaml binaries (``ocamlopt``).
- Parallel execution (``make -j16``).

Thus, a larger swap allocation is necessary to install Tezos. The
minimum requirement depends on the machine configuration, but additional
swap may be needed if resource-intensive applications are run
concurrently with the build process.

+---------------+-----------------+----------------------------------+
| Configuration | Recommended     | Notes                            |
|               | Swap            |                                  |
+===============+=================+==================================+
| RAM ≤ 16 GiB  | 16 GiB          | Minimum to prevent crashes       |
+---------------+-----------------+----------------------------------+
| RAM > 16 GiB  | 12 GiB          | Sufficient for most builds       |
+---------------+-----------------+----------------------------------+
| 16-core CPU   | 16 GiB          | Recommended for ``make -j16``    |
+---------------+-----------------+----------------------------------+

For the remainder of this section, we assume an increase to **16 GiB**,
though this value can be adjusted as needed.

Checking Available Swap
'''''''''''''''''''''''

To determine the amount of available swap memory, several methods can be
used.


.. note::

	The commands ``free``, ``vmstat``, and ``htop``, used below, may not be installed by default in some Linux distributions, so you may have to install them beforehand (e.g. for Debian, they can be found both in package ``procps``).

The ``free`` command provides information on available RAM and swap:

.. code:: shell-session

   $ free -h
                 total        used        free      shared  buff/cache   available
   Mem:           15Gi       12Gi       1.2Gi      0.5Gi       2.0Gi       1.8Gi
   Swap:         2.0Gi      1.5Gi       0.5Gi

The ``swapon --show`` command lists the location of active swap files or
partitions:

.. code:: shell-session

   $ sudo swapon --show
   NAME       TYPE      SIZE   USED PRIO
   /swapfile  file      2G     1.5G   -2

Additionally, the ``htop`` command offers real-time system monitoring.


Increasing Swap Memory
^^^^^^^^^^^^^^^^^^^^^^

In this section, we will increase the swap memory. This involves
defining the desired capacity, creating a file of the corresponding
size, disabling the current swap, and then re-enabling it with the new
configuration.

Creating a Swap File
''''''''''''''''''''

To create a **16 GiB** swap file, use the ``fallocate`` command:

.. code:: sh

   sudo fallocate -l 16G /swapfile

If ``fallocate`` fails, the ``dd`` command can be used as an
alternative:

.. code:: sh

   sudo dd if=/dev/zero of=/swapfile bs=1G count=16

In both cases, the value ``16`` can be replaced with a different size if
required. Alternatively, a different filename can be specified, though
this will need to be reflected in subsequent steps.

Once created, set the correct permissions for the file:

.. code:: sh

   sudo chmod 600 /swapfile

Verify the file has been created correctly:

.. code:: shell-session

   $ ls -lh /swapfile
   -rw------- 1 root root 16G  [date] /swapfile

Disabling Current Swap
''''''''''''''''''''''

For security reasons, we will disable the existing swap. Ensure
resource-intensive processes are stopped to prevent system slowdowns
during this step.

.. code:: sh

   sudo swapoff -a

Associating ``/swapfile`` with Swap Memory
''''''''''''''''''''''''''''''''''''''''''

Now, inform the system of the new swap file location:

.. code:: shell-session

   sudo mkswap /swapfile

Re-enabling Swap Memory
'''''''''''''''''''''''

Re-enable the swap with the new configuration:

.. code:: sh

   sudo swapon /swapfile

Verifying Changes
'''''''''''''''''

To confirm the changes, use ``free -h``, ``swapon --show``, or ``htop``:

.. code:: shell-session

   $ free -h
                 total        used        free      shared  buff/cache   available
   Mem:           15Gi       3.2Gi       8.1Gi       0.5Gi       3.7Gi        11Gi
   Swap:          16Gi       0.0Gi       16Gi

.. code:: shell-session

   $ sudo swapon --show
   NAME       TYPE      SIZE   USED PRIO
   /swapfile  file      16G    0B   -2


Making Changes Permanent (Optional)
'''''''''''''''''''''''''''''''''''

If the swap modification is only needed for a single build, you can
proceed with the build and the changes will revert upon reboot.

To retain the new swap configuration permanently, modify the
``/etc/fstab`` file:

.. code:: sh

   echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab


Real-Time Swap Monitoring
^^^^^^^^^^^^^^^^^^^^^^^^^

To monitor swap usage in real time while building the Octez sources with ``make``, use (in another terminal)
``htop``.

If the build process causes significant system slowdowns, you can log
swap activity using:

.. code:: sh

   vmstat 1 > tezos_build_swap.log

The log file (``tezos_build_swap.log``) can be reviewed after the build
completes or following a system reboot.
