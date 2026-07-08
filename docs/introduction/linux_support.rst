Supported Linux distributions
=============================

This page describes which Linux distributions and releases the Octez
:ref:`binary packages <installing_packages>` are built and tested for, and how
that support evolves over time.

.. note::

   This applies to the **APT (Deb) packages**. The
   :ref:`static binaries <getting_static_binaries>` embed all their
   dependencies and run on any Linux distribution, so they are not tied to the
   list below.

Currently supported
-------------------

All combinations below are provided for both **amd64** and **arm64**.

**Debian**

+-----------+----------+
| Release   | Codename |
+===========+==========+
| Debian 12 | bookworm |
+-----------+----------+
| Debian 13 | trixie   |
+-----------+----------+

**Ubuntu** (LTS releases only)

+-------------+-----------------+---------------------------+
| Release     | Codename        | Upstream end of support   |
+=============+=================+===========================+
| 22.04 LTS   | Jammy Jellyfish | April 2027                |
+-------------+-----------------+---------------------------+
| 24.04 LTS   | Noble Numbat    | April 2029                |
+-------------+-----------------+---------------------------+
| 26.04 LTS   | Plucky Puffin   | April 2031                |
+-------------+-----------------+---------------------------+

See :ref:`installing_deb` for installation instructions.

Support policy
--------------

- **LTS only.** For Ubuntu, we support only Long-Term Support releases.
  Interim (non-LTS) releases are not supported.
- **Two Debian stables.** For Debian, we support the current stable release and
  the previous one (oldstable). Testing and unstable branches are not supported.
- **Both architectures.** Every supported release is built for ``amd64`` and
  ``arm64``.
- **Dropped at end of life.** A release is dropped once its vendor ends regular
  (non-extended) security support.
- **Added after validation.** A new LTS release is added once it passes our
  build and test pipelines -- typically within one Octez release cycle of its
  upstream release.

Planned end of support
----------------------

New LTS releases are added within one Octez release cycle of their upstream
release; see the policy above. The following currently supported releases have
a known end of support:

+----------------------+---------------------------+--------------------------+
| Release              | Upstream end of support   | Dropped from Octez       |
+======================+===========================+==========================+
| Debian 12 (bookworm) | ~June 2026 (security)     | When Debian 14 (forky)   |
|                      |                           | is released (trixie      |
|                      |                           | becomes oldstable)       |
+----------------------+---------------------------+--------------------------+
| Ubuntu 22.04 LTS     | April 2027                | First Octez release      |
|                      |                           | after April 2027         |
+----------------------+---------------------------+--------------------------+

.. note::

   **RPM packages** (Rocky Linux, Fedora) were provided up to Octez v23 and
   have been discontinued since Octez v24.

Recent changes
--------------

+---------------+------------------------------------------------------------+
| Octez version | Change                                                     |
+===============+============================================================+
| v24           | RPM packages (Rocky Linux, Fedora) discontinued            |
+---------------+------------------------------------------------------------+
| v25           | Ubuntu repositories moved from codenames (``jammy``,       |
|               | ``noble``) to version numbers (``22.04``, ``24.04``);      |
|               | Ubuntu 26.04 added                                         |
+---------------+------------------------------------------------------------+
