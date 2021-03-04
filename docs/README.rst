***************************
The documentation subsystem
***************************

The documentation is available online at `tezos.gitlab.io <http://tezos.gitlab.io/>`_,
and always up to date with branch ``master`` on `GitLab <https://gitlab.com/tezos/tezos>`_.

Building instructions
=====================

To build the documentation, you need to install the Python package
manager `Poetry <https://python-poetry.org/>`_. For instructions on
how to obtain Python and Poetry, see :ref:`the installation
instructions for the python testing
framework<python_testing_framework>`.  Once this is done, you can
install the needed dependencies locally:

.. code-block:: bash

    # in the "docs" directory
    poetry install
    make

The output is generated and available in ``docs/_build``. It is built by
Sphinx, and uses the Read The Docs theme.


OCaml documentation
-------------------

As part of the above procedure,
Odoc is used for OCaml API generation. You can install Odoc with:

.. code-block:: bash

    opam install odoc

Tezos generates the API documentation for all libraries in HTML format. The
generated HTML pages are put in ``_build/<context>/_doc``.
It creates one sub-directory
per public library and generates an ``index.html`` file in each sub-directory.

The documentation is not installed on the system by Tezos. It is meant to be
read locally while developing and then published on the www when releasing
packages.

Writing documentation
=====================

Online documentation is written in reStructuredText format, also known as RST.
reStructuredText is the default plaintext markup language used by
`Sphinx <https://www.sphinx-doc.org/>`_, which
is the tool used to compile this format into plain web pages in HTML format.

For the RST syntax, see the `Sphinx RST primer <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html>`_.

Line breaking
-------------

When writing documentation in text formats such as RST, it is not required to respect a maximal line width, such as 80 columns.
Therefore, you may choose between the different line breaking policies your text editor proposes.
However, you should be aware that differencing tools such as ``diff`` tend to output large differences for a paragraph that has been reformatted after only a small change in one phrase.
Also, reviewing tools such as the one in the ``gitlab`` user interface associate comments and change suggestions to lines, while these comments and suggestions are usually logically associated to whole phrases.

For such reasons:

- Some contributors use one line per complete phrase, which allows to make rephrasing suggestions more easily in ``gitlab``, associated to this (possibly long) line; and which allows ``diff`` to isolate modified phrases, instead of showing the whole container paragraph as modified.
- Other contributors, whose editor breaks lines at a fixed width, introduce an extra line break at the end of each phrase. This also allows ``diff`` to isolate modified phrases.

Thus, you may choose your own formatting style, while tolerating different styles from other contributors.

Sphinx extensions
-----------------

Some ad-hoc reference kinds are supported.

- ``:package-src:`name``` or ``:package-src:`text<name>``` points
  to the gitlab source tree viewer where the `.opam` for the package
  is located
- ``:package:`name``` or ``:package:`text<name>``` now points
  either to the `odoc` page, or if it doesn't exist, to the gitlab
  source tree viewer
- ``:package-name:`name``` or ``:package-name:`text<name>``` just
  displays the package name (no link), checking that the package
  exists
- ``:src:`/path/to/file/or/dir``` or
  ``:src:`text</path/to/file/or/dir>``` points to the gitlab source
  tree viewer
- ``:opam:`package``` or ``:opam:`text<package>``` points to the
  package page on ``opam.ocaml.org``, version number is supported
  (``package.version``)

Writing protocol documentation
------------------------------

Writing protocol documentation is a special case because protocol-related
documentation pages are duplicated for several protocol versions (under directories named ``NNN/``, where *NNN* is either a protocol number or "alpha"), and possibly
also in a protocol-independent part (typically under directory
``shell/``).

Besides the need of maintaining several versions of these pages, this
duplication introduces the need to carefully handle documentation
cross-references, in particular to avoid duplicate labels (i.e., multiple labels with the same name in different pages) and wrong references (i.e.,
escaping from one protocol version into another).

The following rules promote a systematic way of handling documentation
cross-references that avoids introducing such errors.

Definitions
~~~~~~~~~~~

First let us introduce the following definitions:

- A *label* is an identifier defining a specific position in a documentation page (typically, before a section name). A *reference* is a link to a label, in the same or another page. In Sphinx, labels are written ``.. _label:`` and references are written ``:ref: `textual description <label>```, or ``:ref: `label```. Labels and references are case-insensitive.
- A *versioned* label bears a protocol version in it (e.g. ``label_NNN``); an  *unversioned* label doesn't (i.e. just ``label``)
- A *local* reference is a link from a protocol-specific page to the same page or to another protocol-specific page. An *external* reference is a reference from a protocol-independent page to a label in a protocol-specific page.

Rules
~~~~~

The following simple rules are proposed for safely managing cross-references:

1. In all but the **current** protocol, any defined label must be versioned::

    .. _<label>_NNN:

2. In the **current** protocol, labels may be versioned (as targets of local references), unversioned (as targets of external references), or both. The last case is done by defining *two* labels for such location::

    ..  _<label>:
    ..  _<label>_NNN:

3. Any local reference in protocol NNN must be versioned NNN. This includes references appearing in the currently active protocol.

4. External references must be unversioned.

The rationale of the above rules:

- Any label defined in a protocol-specific page must be versioned to avoid name conflicts (as by definition the containing page is duplicated).
- External references must be unversioned to avoid modifying protocol-independent pages when the current protocol is changed.
- Local references in the current protocol could also work if unversioned, but when the protocol is changed, they should be rewritten as versioned. It is much simpler to enforce the rule that all local references in a page for any protocol NNN must be versioned NNN.

Protocol changes
~~~~~~~~~~~~~~~~

When a new protocol is adopted, its pages must be "linked" with the protocol-independent pages:

- remove in the old protocol all the unversioned labels (this operation is unnecessary if the pages of the old protocol are removed altogether)
- add in the new protocol an unversioned label before each versioned label

**NB** no rewriting of any reference is needed on protocol changes.

On creating a new protocol proposal version NNN out of alpha:

- rename all versioned labels AND references _alpha in its pages to version _NNN

Rules automation
~~~~~~~~~~~~~~~~

To help enforcing the above cross-referencing rules in protocol-specific pages, the following scripts are provided under ``docs/scripts``:

- ``check_proto_xrefs.py``: checks the references, and optionally the labels, in all pages of a given protocol version

  + can be used at any time, e.g. when changing a protocol-specific page
- ``add_labels_without_proto.py``: adds unversioned labels before each versioned label in a protocol-specific page

  + can be used when a new protocol is adopted, to "link" its documentation into protocol-independent pages
- ``remove_labels_without_proto.py``: removes unversioned labels in a protocol-specific page

  + can be used when a new protocol is adopted for "unlinking" the pages of the old protocol, only if those pages are not removed altogether

Moreover, the script ``scripts/snapshot_alpha.sh``, used to create a new protocol proposal version NNN out of alpha is planned to integrate renaming of labels and references.

Documenting protocols
~~~~~~~~~~~~~~~~~~~~~

Due to the duplication of the documentation for multiple protocol versions, the following extra guidelines should be observed.

- In principle, protocol-independent pages should only refer to the currently active protocol. Indeed, until newer protocols are adopted, there is no guarantee that their features will be part of Tezos someday.

- When modifying the pages of a given protocol version, you might have to also modify it for later versions. Otherwise, when newer protocols are adopted, your changes will vanish! In particular, when fixing a problem in the documentation of the current protocol (e.g. adding a term in the glossary), you might have to fix it also for the candidate protocol (if there is one under the voting procedure) and for the Alpha protocol under development (assuming that the features of the candidate protocol will be inherited by or proposed in another form in Alpha).

- As there is a considerable overhead for maintaining protocol-specific pages, think twice before duplicating a page as protocol-specific. Does this page really refer to the protocol? If yes, does *all* the page refer to the protocol? If the answer to the last question is "no", consider splitting the page in two parts, respectively protocol-specific and protocol-independent.
