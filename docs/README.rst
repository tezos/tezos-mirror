******************************
Building documentation locally
******************************

The documentation is available online at `tezos.gitlab.io <http://tezos.gitlab.io/>`_,
and always up to date with branch ``master`` on `GitLab <https://gitlab.com/tezos/tezos>`_.

Building instructions
---------------------

To build the documentation, you need to install the Python package manager
`Poetry <https://python-poetry.org/>`_. For instructions on how to 
obtain Python and Poetry, see :ref:`python_testing_framework<installation>`. 
Once this is done, you can install the needed dependencies locally:

.. code-block:: bash

    # in the "docs" directory
    poetry install

Then, you can use the main Makefile target ``doc-html`` to generate the HTML:

.. code-block:: bash

    # in the project root directory
    make doc-html

The output is generated and available in ``docs/_build``. It is built by
Sphinx, and uses the Read The Docs theme.


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

OCaml documentation
-------------------

Odoc is used for OCaml API generation, that you can install with:

.. code-block:: bash

    opam install odoc

Tezos generates the API documentation for all libraries in HTML format.  The
generated HTML pages in ``_build/<context>/_doc``. It creates one sub-directory
per public library and generates an ``index.html`` file in each sub-directory.

The documentation is not installed on the system by Tezos. It is meant to be
read locally while developing and then published on the www when releasing
packages.
