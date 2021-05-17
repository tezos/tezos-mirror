.. _coding_guidelines:

Coding guidelines
=================

This document provides guidelines that should be observed by all the contributors to the Tezos codebase. It first presents documentation guidelines, and then rules more specific to coding (e.g., logging levels, code formatting, naming conventions, etc.).

License
-------

The Tezos software is distributed under the MIT license. Every OCaml source file should start with a header comment instantiating the following template (use appropriate comment syntax for other languages):

.. literalinclude:: LICENSE.ml

Note that:

- The holder, on the copyright line, is the name of the company which hires the employee or the sub-contractor.
- For sub-contractors, check your specific contract terms. They sometimes allow to include, as an additional copyright holder, the name of a particular developer, but consider that this may end up with bloated license headers.
- When adding a significant new contribution to a file (i.e. more like whole new features, rather than simple fixes), check whether there already is a copyright for your copyright holder (see above).

  + If there is one, mentioning any year, it is not required to add the current year (but this is allowed). In no case should you *replace* the existing year with the current one.
  + If there is no line for your copyright holder, you should add one, with the current year.

- Old source files may contain on the first line `Open Source License` instead of `MIT License`. When touching such a file, please replace the former with the latter, correct form.

For example, for a source file with multiple contributors spanning several years, the copyright lines may look as follows::

  (* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
  (* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
  (* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)


.. _in_code_comments:

Comments in the code
--------------------

The OCaml code should include comments facilitating the comprehension and the maintenance. In particular, the main syntactic constructs in the code should be commented as follows.

Modules:

- One-line comment explaining the purpose of the module
- If needed: More detailed description

Types:

- One-line comment explaining what the type represents, typically the invariants satisfied by its inhabitants

Functions and methods:

- Purpose of the function, brief description of the returned value
- If needed: How and why to use this function
- If needed: Pre-conditions for calling the function
- If needed: Conditions under which the function will return an error
- If needed: Any special behavior that is not obvious

Constants and struct fields:

- Purpose and definition of this data. If the unit is a measurement of time, include it, e.g., TIMEOUT_MS for timeout in milliseconds.

.. _todo_fixme:

TODO/FIXME comments
~~~~~~~~~~~~~~~~~~~

During the :ref:`code review process <code_review>`, follow-up issues may be created to improve some piece of code that already implement its specification (e.g., optimize, refactor, or bring a potentially useful generalization).
When the place of the future evolution is known in advance (e.g. a given function), you should mark it with a ``TODO`` comment of the form::

    (* TODO: <reference to issue>
       <one-line explanation>
       <Add long explanation if the issue description is not the right place.>
    *)

If the evolution is needed to fix some code that does *not* fully implement its specification, while serving as a useful intermediate step (e.g., it temporarily handles just some particular cases), rather than to merely improve the code, you should use a ``FIXME`` tag instead of the ``TODO``, adhering to the same syntax for the rest.

Thus, the difference between ``TODO`` and ``FIXME`` tags is a semantic one,  reflecting the full/partial implementation of the specification.
Consequently, when the specification evolves to become more demanding, some ``TODO`` tags corresponding to *potential* evolutions may have to be recasted as ``FIXME`` tags, corresponding to *required* evolutions.

Note that the reference to an existing issue on the first line is mandatory, to facilitate searches of evolutions corresponding to given issues, and might be checked automatically by the :ref:`Merge-Request Bot <merge_bot>`.
The reference to an issue may be one of:

- a URL such as ``https://gitlab.com/tezos/tezos/-/issues/1377``
- a GitLab notation such as ``#123`` (implicitly under ``tezos/tezos``), ``michelson-reference#123`` (implicitly under ``tezos/michelson-reference``), or ``smondet/merbocop#123`` (fully qualified).

Documenting interfaces and implementations
------------------------------------------

At the granularity of OCaml files, it is essential to document the interface implemented by each file.
In many cases, it is useful to also document the implementation, but *separately* from the interface.

Implementation (``.ml``) files:

- Document the interface:

  + In the common case where there is a corresponding interface (``.mli``) file,
    document the interface file instead, as detailed below.
  + In the less common case where there is no corresponding interface (``.mli``)
    file, document the exported elements directly in the implementation
    (``.ml``) file.

- Document the implementation: For many non-trivial implementations, it is most useful to document the design principles, code structure, internal invariants, and so on.
  Such information should be placed in a comment block at the top of the file.

Interface (``.mli``) file comments:

- One-line description
- Brief description of the library, introducing the needed concepts
- Brief description of each module, type, function, data, as described for :ref:`comments in the code<in_code_comments>`
- If applicable, external invariants (i.e., visible to the user).

README files
------------

At coarser levels, source file directories should be documented by Markdown files called ``README.md``. Such files are mandatory in top-level directories of the Tezos codebase (such as ``src/`` and ``docs/``), and at least in immediate sub-directories of the source directory (``src/*/``).

Source directories must instantiate the following ``README.md`` template::

  # Component Name
  <!-- Summary line: One sentence about this component. -->

  ## Overview
  <!--
  - Describe the purpose of this component and how the code in this directory
    works. If needed, design rationale for its API.
  - Describe the interaction of the code in this directory with the other
    components. This includes dependencies on other components, for instance.
  - Describe the security model and assumptions about the crates in this
    directory.
  -->

  ## Implementation Details
  <!--
  - Describe how the component is modeled.
  - Describe the code structure and implementation design rationale.
  - Other relevant implementation details (e.g. global invariants).
  - Testing specifics, if needed.
  -->

  ## API Documentation
  <!--
  - Link to the external API.
  - For the top-level source directory, link to the most important APIs within.
  -->

The rationale of this template is that a README file addresses two different kinds of developers:

#. the users of the module, which are concerned only about the component
   concepts and API, and not about its implementations details, and
#. the developers and maintainers of the module, which are also concerned about
   implementation details.

When filling in the template, you should keep untouched the guidelines within
HTML comments (which are visible to the document maintainers but invisible to
end-users), so that any maintainer can check how well the README instantiates
the template, and address any gap if needed.

Logging Levels
--------------

The Tezos libraries use an internal logging library with 5 different verbosity `levels`.
It is important to choose the appropriate level for each event in the code to
avoid flooding the node administrator with too much information.

These are the rules-of-thumb that we use in the code to decide the appropriate
level (here listed from most to least verbose) for each event:

- ``Debug`` level -- the most verbose -- it is used by developers to follow
  the flow of execution of the node at the lowest granularity.
- ``Info`` level is about all the additional information that you might want to
  have, but they are not important to have if your node is running OK
  (and definitely do not require any action).
- ``Notice`` level (the default) should be about things that the node
  admin should be concerned, but that does not require any action.

The two following levels are used to provide information to the node
administrator of possible problems and errors:

- ``Warning`` level are all those events that might require the attention of
  the node administrator, and can reveal potential anomalies in the workings of
  the node.
- ``Error`` level are all those events that require an intervention of the node
  administrator or that signal some exceptional circumstance.

Code formatting
---------------

To ensure that your OCaml code is well formatted, set up correctly your editor:

+ automatically run `ocamlformat` when saving a file
+ no tabs, use whitespaces
+ no trailing whitespaces
+ indent correctly (e.g. use lisp-mode for dune files)

Many of these checks can be run with ``make check-python-linting``.

Some of these checks can be executed with a `pre-commit <https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks>`_
which is installed with
``ln -sr scripts/pre_commit/pre_commit.py .git/hooks/pre-commit``
(see the header of `./scripts/pre_commit/pre_commit.py` and its `--help`
for additional options).

Coding conventions
------------------

Other than the formatting rules above, there are currently no coding
conventions enforced in the codebase. However, Tezos developers should be aware
of general `OCaml programming guidelines <http://caml.inria.fr/resources/doc/
guides/guidelines.en.html>`_, which recommend formatting, naming conventions,
and more.
