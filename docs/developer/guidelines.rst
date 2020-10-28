.. _coding_guidelines:

Coding guidelines
=================

This document provides guidelines that should be observed by all the contributors to the Tezos codebase. It consists mostly in documentation guidelines, but rules more specific to coding (e.g., code formatting, naming conventions, etc.) may be added later on.

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

Documenting interfaces
----------------------

At the granularity of OCaml files, it is essential to document the interface implemented by each file.

Implementation (``.ml``) files:

- In the common case where there is a corresponding interface (``.mli``) file,
  document the interface file instead, as detailed below.
- In the less common case where there is no corresponding interface (``.mli``)
  file, document the exported elements directly in the implementation (``.ml``)
  file.

Interface (``.mli``) file comments:

- One-line description
- Brief description of the library, introducing the needed concepts
- Brief description of each module, type, function, data, as described for :ref:`comments in the code<in_code_comments>`.

README files
------------

At coarser levels, source file directories should be documented by Markdown files called ``README.md``. Such files are mandatory in top-level directories of the Tezos codebase (such as ``src/`` and ``docs/``), and at least in immediate sub-directories of the source directory (``src/*/``).

Source directories must instantiate the following ``README.md`` template::

  # Component Name
  Summary line: One sentence about this component.

  ## Overview
  - Describe the purpose of this component and how the code in this directory
    works. If needed, design rationale for its API.
  - Describe the interaction of the code in this directory with the other
    components. This includes dependencies on other components, for instance.
  - Describe the security model and assumptions about the crates in this
    directory.

  ## Implementation Details
  - Describe how the component is modeled.
  - Describe the code structure and implementation design rationale.
  - Other relevant implementation details (e.g. global invariants).
  - Testing specifics, if needed.

  ## API Documentation
  - Link to the external API.
  - For the top-level source directory, link to the most important APIs within.

The rationale of this template is that a README file addresses two different kinds of developers:

#. the users of the module, which are concerned only about the component
   concepts and API, and not about its implementations details, and
#. the developers and maintainers of the module, which are also concerned about
   implementation details.
