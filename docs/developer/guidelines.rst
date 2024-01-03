===================================
Documentation and coding guidelines
===================================

This document provides guidelines that should be observed by all the contributors to the Octez codebase. It first presents documentation guidelines, and then rules specific to coding.

Documentation guidelines
========================

Documentation guidelines are presented for different scales: functions, modules, files, and libraries. General remarks applying to all scales are presented :ref:`afterwards <general_doc_rules>`.

.. _in_code_comments:

Comments within the code
------------------------

The OCaml code should include comments facilitating comprehension and maintenance.

Towards this goal, comments are useful in the code wherever they bring important information that is not easily deducible from the code itself, such as: explaining the high-level goal of a code fragment, justifying an unusual pattern of code, exhibiting an  invariant, etc. See also rule :ref:`rule_dont_repeat_the_code`.

Additionally, the main syntactic constructs in the code should be commented as follows.

Modules:

- One-line comment explaining the purpose of the module
- If needed: More detailed description

The comments on a module may describe the overall abstraction provided by the module and its capabilities, without implementation details or even specifics of the different functions.
It may also describe limitations of the module that are important to developers using it (e.g. concurrency or performance limitations).

Types:

- One-line comment explaining what the type represents, typically the invariants satisfied by its inhabitants

Functions and methods:

- Purpose of the function, brief description of the arguments and returned value
- If needed: How and why to use this function
- If needed: Pre-conditions for calling the function, including constraints on arguments values or dependencies between arguments, if any
- If needed: Conditions under which the function will return an error or raise an exception
- If needed: Any special behavior that is not obvious

Constants, variables, variants, and struct fields:

- Purpose and definition of this data. Useful comments do not just expand a name using the same words, but rather describe the data including details that could not be included in its name.
- If there is an associated measurement unit, define it in the comment, unless it is contained in the name, e.g.,  ``TIMEOUT_MS`` for a timeout in milliseconds.

.. _todo_fixme:

TODO/FIXME comments
~~~~~~~~~~~~~~~~~~~

During the :ref:`code review process <code_review>`, follow-up issues may be created to improve some piece of code that already implements its specification (e.g., optimize, refactor, or bring a potentially useful generalization).
When the place of the future evolution is known in advance (e.g. a given function), you should mark it with a ``TODO`` comment of the form:

.. code-block:: ocaml

    (* TODO: <reference to issue>
       <one-line explanation>
       <Add long explanation if the issue description is not the right place.>
    *)

If the evolution is needed to fix some code that does *not* fully implement its specification, (e.g., a known bug detected but not yet fixed, or a temporary implementation handling only some particular cases), rather than to merely improve the code, you should use a ``FIXME`` tag instead of the ``TODO``, adhering to the same syntax for the rest.

Thus, the difference between ``TODO`` and ``FIXME`` tags is a semantic one,  reflecting the full/partial implementation of the specification.
Consequently, when the specification evolves to become more demanding, some ``TODO`` tags corresponding to *potential* evolutions may have to be recasted as ``FIXME`` tags, corresponding to *required* evolutions.

Note that the reference to an existing issue or MR on the first line is mandatory, to facilitate searches of specific evolutions.
These tags might be checked automatically by the :ref:`Merge-Request Bot <merge_bot>`, e.g. to signal tags that refer to non-existent issues or MRs, to closed issues, or to already merged MRs.
The reference to an issue may be one of:

- a URL such as ``https://gitlab.com/tezos/tezos/-/issues/1377``
- a GitLab notation for an issue such as ``#123`` (implicitly under ``tezos/tezos``), ``michelson-reference#123`` (implicitly under ``tezos/michelson-reference``),
  or ``oxheadalpha/merbocop#123`` (fully qualified).
- a GitLab notation for a merge request such as ``!10990``.

License
-------

The Octez software is distributed under the MIT license. Every OCaml source file should start with a header comment instantiating the following template:

.. code-block:: ocaml

    (*****************************************************************************)
    (*                                                                           *)
    (* SPDX-License-Identifier: MIT                                              *)
    (* SPDX-FileCopyrightText: [year(s)] [Holder <email>]                        *)
    (*                                                                           *)
    (*****************************************************************************)

For other languages than OCaml, use the appropriate comment syntax. For example, here is the Rust template::

    //
    // SPDX-License-Identifier: MIT
    // SPDX-FileCopyrightText: [year(s)] [Holder <email>]
    //


The following, full-text equivalent, template for OCaml is also valid but deprecated:

.. literalinclude:: LICENSE.ml
   :language: ocaml


Note that:

- The holder, on the copyright line, is the name of the company which hires the employee or the sub-contractor.
- For sub-contractors, check your specific contract terms. They sometimes allow to include, as an additional copyright holder, the name of a particular developer, but consider that this may end up with bloated license headers.
- When adding a significant new contribution to a file (i.e. more like whole new features, rather than simple fixes), check whether there already is a copyright for your copyright holder (see above).

  + If there is one, mentioning any year, it is not required to add the current year (but this is allowed). In no case should you *replace* the existing year with the current one.
  + If there is no line for your copyright holder, you should add one, with the current year.

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
- Brief description of each module, type, function, data, as described for :ref:`comments in the code<in_code_comments>` but :ref:`using special comments <using_docstrings>`
- If applicable, external invariants (i.e., visible to the user).

Interface comments should omit implementation details, to the extent where these are invisible to users of the interface.

.. _using_docstrings:

Using docstrings
~~~~~~~~~~~~~~~~

The documentation of OCaml interfaces is automatically generated by ``odoc`` from *special comments* in the source code, of the form ``(** ... *)``, also called documentation comments, or "docstrings".
Here are a few tips and guidelines on using docstrings.

- A documentation page is generated for each module *interface* and module type, and for each class *interface* and class type, from the docstrings attached to them and to their contained elements.
  Docstrings inside a module *implementation* (i.e. ``struct ... end`` constructs) are not kept.
  You should use normal comments (``(* ... *)``) inside the implementation.

- The page generated for each interface starts with a "preamble", consisting of: the docstring on its declaration, if any, and the "top-comment" inside the interface (the first docstring inside the interface not attached to a contained element), if any.
  The first paragraph of the preamble, if any, will serve as the "synopsis" of the interface, and will be used when the interface appears in a list (typically, in the page of its containing interface).

- When it makes sense, you should document not only interfaces at the top level of a file, but also those embedded in other (module or class) interfaces.

- Docstrings in ``.mli`` and ``.ml`` files are handled the same, so do not omit documenting the interfaces in the latter files.

For more information on using docstrings, see the ``odoc`` `documentation for library authors <https://ocaml.github.io/odoc/odoc_for_authors.html>`__.

Docstrings errors
"""""""""""""""""

When ``odoc`` is generating documentation from docstrings, it performs various syntax and semantics checks and may thereby emit many kinds of warnings.
Some of these warnings are turned into errors when the flag ``ODOC_WARN_ERROR`` is on.
You have to fix at least these errors for making the CI green.

When ``odoc`` returns a non-zero exit code, the list of errors is displayed in the terminal but some details are abstracted away.
The full details of each error, and also the regular warnings, can be found in the log produced by ``odoc`` in file ``${TMPDOCDIR}/odoc.log``, where variable ``TMPDOCDIR`` is defined in file :src:`docs/Makefile`.
Search for the string ``Error:`` in that file to find all the errors.

You may consult a list of `typical error messages found by odoc in the Tezos repository <https://gitlab.com/nomadic-labs/protodoc/-/blob/master/doc/errors.md>`__.
These examples may help both to avoid common pitfalls when writing docstrings,  and to better understand ``odoc`` errors that you may encounter.

index.mld files
---------------

At the granularity of the library, you can optionally include an ``index.mld`` file.
This file is used to generate the landing page for the online API of the library.
If you do not include this file, the landing page is automatically generated and only includes a list of all top-level modules.

An ``mld`` file is written in the `ocamldoc markup language <https://ocaml.org/manual/ocamldoc.html#s%3Aocamldoc-comments>`_.

Because it is used for the online API of the library it should contain information that might be of interest to the users.
This includes

- A general introduction to the library.
- Design decisions for the API.
- Usage recommendations including the security model and assumptions.
- Example uses.
- Links to related documents (such as RFC and ISO standards).

The file should also include a link to all of the modules that are intended entry-points for the users.
You can include those with ``{!module:Foo}`` inline in an appropriate paragraph of the documentation.
You can also include those with a dedicated block::

  {!modules:
  Foo
  Bar
  Blah
  }

.. warning::
   When you add an ``index.mld`` file, don't forget to add a ``~documentation:[]`` parameter to the package's manifest which adds a documentation stanza in the corresponding dune file.
   Otherwise, your ``index.mld`` file will be ignored by ``odoc`` and the API page will be the default one!


README files
------------

Also at the level of the library, you should include a ``README.md`` in `Markdown format <https://daringfireball.net/projects/markdown/>`_.
Such files are mandatory in top-level directories of the Octez codebase (such as ``src/`` and ``docs/``), and at least in immediate sub-directories of the source directory (``src/*/``).
Because it is accessible only in the source tree, the file should contain information that might be of interest to the maintainers and contributors.

You must instantiate the ``README.md`` file with the following template:

.. code-block:: markdown

  # Component Name
  <!-- Summary line: One sentence about this component. -->

  ## API Documentation
  <!-- Link to the external API. -->

  ## Installation
  <!-- Describe how this component can be installed (if applicable). -->

  ## Overview
  <!--
  - Describe the purpose of this component.
  - Describe the interaction of the code in this directory with the other
    components. This includes dependencies on other components, for instance.
  -->

  ## Implementation Details
  <!--
  - Describe the file structure and the location of the main components.
  - Other relevant implementation details (e.g., global invariants,
    implementation design rationale, etc.).
  - Testing specifics, build-system specifics, etc. as needed.
  -->

The rationale of this template is that a README file addresses the developers
that are not just using the library but also fixing or modifying it.
To avoid duplication between the ``index.mld`` and ``README.md`` files, follow
this simple rule-of-thumb: the ``index.mld`` contains information for the users
(how to use the API) whereas the ``README.md`` file contains information for the
maintainers (how are the files organised, are there any build-system quirks,
etc.).

When filling in the template, you should keep untouched the guidelines within
HTML comments (which are visible to the document maintainers but invisible to
end-users), so that any maintainer can check how well the README instantiates
the template, and address any gap if needed.

.. _general_doc_rules:

General documentation rules
---------------------------

.. _rule_dont_repeat_the_code:

Don't repeat the code
~~~~~~~~~~~~~~~~~~~~~

When writing comments, a common mistake is to repeat the code, paraphrasing it with the same words, or at the same abstraction level.
Good comments bring information that is not obvious from the code itself.
They complement the code with details at a *different level of abstraction*, thus typically using different words to provide details on the meaning of the code.

- Lower-level comments add precision

  Such comments add precision by providing details not easily inferred from the code. For example, comments on a function may define its behaviour in particular cases, or provide defaults for the arguments; comments on a variable may add details, such as the measurement units, boundary conditions, invariants, or define the meaning of the different values (e.g. for a boolean variable).

- Higher-level comments enhance intuition

  Such comments are written at a higher level of discourse than the code, and provide a basis to understand the provided asbtractions and underlying concepts, and to reason about them.
  For example:

    * An implementation comment inside a module or function body may explain the overall goal and provide a conceptual framework in which the details of the code may be easily understood.
      Typically, the comment will explain *what* the code is doing, and possibly *why* (e.g., in which context the code is called), rather than *how* it is done --  this can be usually seen in the code itself.

    * A comment on the interface provided by a library may describe the solution provided for the given problem domain, how the different functions may be combined together, some limitations of the library (e.g. in terms of concurrency), etc.

Comments maintainability
~~~~~~~~~~~~~~~~~~~~~~~~

Maintaining comments up-to-date with the code is a crucial component of their usefulness.
Moreover, if many comments in a piece of software become out of date, developers may start ignoring them altogether.
Fortunately, a few techniques may greatly increase the maintainability of comments.

* Place comments near the concerned code.

  Comments are more likely to be updated if they are placed at the "natural" place where developers would look for them.
  For interface comments, the position of docstrings is already defined by Odoc: on the definition of the module, function, or variable.
  Implementation comments should be placed near the code they describe; avoid placing all the comments at the beginning of the file, unless their scope is the whole file.

* Stay at a high level.

  Usually, the farther comments are from the code, the more high-level they must be.
  Comments associated to a large scope of code may typically present a global view of the code in general terms, which makes them less likely to be invalidated by local changes; when changes concern a large part of that scope, they can be impacted but there are chances that developers will see them.

* Avoid duplicating comments.

  Some comments are needed in several places.

  * For instance, an externally-visble module exporting part of the functionality of some internal module(s) should describe each exported element, because users may not look to the internal module(s); but the internal modules should also describe their interface for their own users and maintainers.

    In such cases, duplication may be avoided by choosing a single place for the comments, and using cross-references to it wherever needed.
    Fortunately, Odoc provides `such cross-references <https://ocaml.github.io/odoc/odoc_for_authors.html#links-and-references>`__ and checks for each reference that the target element exists, ensuring they do not get stale when code is moved or suppressed.

  * In other cases, the documentation needed near some piece of code is already available outside the code. For instance, a user-visible client command or node RPC should not duplicate explanations in the user reference.

    Most such cases are handled in Octez by automatically generating the reference manual from the code.
    When this is not the case, comments in the code should refer to the outside sources instead of duplicating them, by using: web links to the technical documentation, to GitLab repositories, or to third-party websites when appropriate (e.g. Wikipedia for known concepts).

  * Another important case is comments describing a cross-module design decision or property.

    Duplication can be avoided here by using cross-references, as described above.
    However, if all the modules belong to the same library, you could rather place the comments in a ``index.mld`` file describing the library, and refer to that file in all the needed places, using Odoc-checked cross-references.

* Envision cohesive documentation.

  * When writing online technical documentation, refer whenever needed to code artefacts: module APIs, GitLab entities in the Octez repository, or source files, using our Sphinx-checked :ref:`custom roles <custom_sphinx_roles>`.

  * More generally, including various kinds of checked cross-references in the documentation makes it more cohesive, more robust and easier to maintain.
    Indeed, the more checked links you include in the documentation (from comments to related code elements, from comments to ``.mld`` files and back, from external documentation to code and back, to external websites, etc.), the more are chances that the documentation will be updated along with the code, when some links break.

    Our documentation tools (Odoc and Sphinx, currently) provide support for such cohesive documentation which is probably more advanced than the average, so take advantage from it!

Logging vs comments
~~~~~~~~~~~~~~~~~~~

Sometimes, it may be preferable to use logging instead of commenting a section of code.
That is, consider using:

.. code-block:: ocaml

   Log.info "Does this and that..." ;

instead of:

.. code-block:: ocaml

   (* Does this and that... *)

and certainly instead of:

.. code-block:: ocaml

   (* Does this and that... *)
   Log.info "Does this and that..." ;

This form of "executable comments" is appropriate for instance when writing tests, and more generally, whenever a dynamic execution trace is useful, provided that the log message contains all the information that you would put in a comment.
Of course, if the log message is too short to fully describe the code section, consider completing it (rather than duplicating it) with comments.

For details on logging, see :ref:`logging_levels`.

Comment first
~~~~~~~~~~~~~

This section is by no means a rule to comply with, but just some arguments, advocated by John Ousterhout [1]_, for a documentation practice consisting in writing most comments *before* writing the code, or *while* writing the code.
These arguments are left to the appreciation of each developer.

Most comments, including all the docstrings that describe interfaces, are more related to the software design phase than to the other phases of software development such as coding, testing, releasing.
Indeed, these comments are currently the only way to fully describe the abstractions provided by software and their underlying concepts.
Writing comments during the design phase (e.g. woven with the definition of interfaces), may be an effective way to improve the quality of both the design and the comments, because:

* During this phase, the developers' mindset is at the right level of abstraction to describe software components clearly and simply, not distracted by implementation details.
  This usually results in better comments.

* Moreover, by expressing the interface design concisely in words, comments are an effective design tool: they allow evaluating the simplicity of interfaces from the start, and thus producing better designs.
  For example, a module or function difficult to describe in a simple way may be a sign that the design does not effectively hide complexity or does not provide the right abstractions.

* Finally, writing comments before the code may be more fun and rewarding.
  Indeed, integrating comments in a useful way in the design process can make documentation more appealing, and a well-presented result may by more satisfying.
  As opposed to that, many developers are writing comments as some sort of menial work to do after code is complete, when the design has to be partly reconstituted from the code; this typically leads to more work, and to lower-quality, and hence less satisfactory comments.

Coding guidelines
=================

This section presents coding guidelines, related to aspects such as logging levels, code formatting, and naming.

.. _logging_levels:

Logging Levels
--------------

The Octez libraries use a logging library with 5 different verbosity *levels*
defined in ``src/lib_event_logging/internal_event.mli`` for shell and
``src/lib_protocol_environment/sigs/v3/logging.mli`` for protocol code.

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

There is another level ``Fatal`` with the highest priority but it is rarely
relevant. Specifically, ``Fatal`` should be reserved for errors that can
absolutely not be recovered. All logging at the ``Fatal`` level should be
immediately followed by a call to ``Lwt_exit.exit_and_raise``.

Note that a library is never able to decide whether a certain condition is fatal
or not. Indeed, the application that calls into the library may not consider the
function call as essential to the continuation of the application's main
purpose. Consequently, ``Fatal`` should never be used within libraries.


Code formatting
---------------

To ensure that your OCaml code is well formatted, set up correctly your editor:

+ automatically run ``ocamlformat`` when saving a file
+ no tabs, use whitespaces
+ no trailing whitespaces
+ indent correctly (e.g. use lisp-mode for dune files)

Many of these checks can be run with ``make check-python-linting``.

Some of these checks can be executed with a `pre-commit hook <https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks>`_
which is installed with
``ln -sr scripts/pre_commit/pre_commit.py .git/hooks/pre-commit``
(see :doc:`pre_commit_hook` for more details).

Exposing internals
------------------

Sometimes you want to expose some internal functions, types or sub-modules of a module: usually it is for testing part of the module logic, but it may also be to let some power users access internals (for performance, extra-functionality, or any other reason).

A first question you should ask yourself is: "Is this a hint that my module has too many responsibilities?".

If the answer is yes: consider splitting your module per responsibility and see if you still need to expose internals.

If the answer is no: the guideline is to expose an internal module as part of your API:

+ ``Internal_for_tests`` for internal types, functions and sub-modules that are only meant to be accessed by tests
+ ``Internal`` for internal types, functions and sub-modules that are meant to be accessed by production code (but may also be accessed by tests)

Additionally you should add the ``(**/**)`` `Stop special comment <https://ocaml.org/manual/ocamldoc.html#ss:ocamldoc-stop>`_ so that this module is
not displayed in the public module documentation.

The rationale of ``Internal_for_tests`` is to make it explicit both for developers writing code and for reviewers that this module must not be used in production code.

Example:

.. code-block:: ocaml
  :emphasize-lines: 6,8-20,35-43

   (* mli *)
   type t

   val f : t -> u

   (**/**)

   module Internal_for_tests : sig
     (** The actual representation of [t] *)
     type t_raw = {bar : string; baz : int}

     (** Access inner fields of a value of type [t] *)
     val unwrap : t -> t_raw

     (** An internal function that we want to test in isolation *)
     val g : t -> v

     (** You can also expose internal modules, e.g. the inner cache *)
     module Mycache : Cache.S
   end

   (* ml *)
   module Mycache = Cache.Make ...

   type t = {bar : string; baz : int}

   (* [g] is an internal function, not part of the public API *)
   let g t = ...

   (* [f] uses internal function [g] *)
   let f t =
     let w = g t in
     ...

   module Internal_for_tests = struct
     type t_raw = t = {bar : string; baz : int}

     let unwrap t = t

     let g = g

     module Mycache = Mycache
   end

Exceptions and errors
---------------------

The following pieces of advice should be applied in general, although exceptions apply (pun intended).

- Only use exceptions locally and don't let them escape: raise them and catch them within the same function or the same module.

  - If a function that is exported can fail, return a ``result`` or a ``tzresult``.

  - If you cannot (or for another reason do not) handle an exception and it may escape you **must** document it.

- Never catch ``Stack_overflow`` nor ``Out_of_memory`` which are exceptions from
  the OCaml runtime rather than the code itself. In other words, when one of
  these exception is raised in one process, the same exception may or may not be
  raised in another process executing the same code on other machines. When you
  catch this exception, you make a branching in the code that is decided not
  based on properties of the code, but properties of the process executing the
  code. Consequently, the same branching may differ on two distinct runs of the
  same code. This is, in essence, non-determinism.

  - If you are in one of the small cases where non-determinism is ok and you
    have a compelling reason to catch either ``Stack_overflow`` or
    ``Out_of_memory``, you **must** include a comment explaining why.

  - Note that catch-all patterns (such as wildcard (``| _ ->``) and variable
    (``| exn ->``) include ``Stack_overflow`` and ``Out_of_memory``.

- Do not let low-level, implementation-dependent exceptions and errors bubble up
  to high-level code. For example, you should catch ``Unix_error`` near the
  syscall sites (ideally, within the same module) and handle it there. If you
  cannot handle it (e.g., if the error is non-recoverable) you should translate
  it into an error that is more relevant to the high-level code.

  - E.g., If a file-writing call to a library function raises
    ``Unix_error(ENOSPC, _, _)``, the caller of that library function should

    - catch the exception,

    - attempt to recover (if possible; e.g., by removing other old files before attempting it again),

    - and if the recovery does not work (e.g., does not release sufficient
      space) or is impossible (e.g., there are no references to old files in
      scope) then it should fail in a more meaningful way than by forwarding the
      exception (e.g., indicating what operation it was trying to carry).

  - In the rare case that the underlying exception/error is satisfactory to the
    higher level code, then you may propagate it as is.

The ``Lwtreslib`` and the ``Error_monad`` libraries provide functions that can
help you follow these guidelines. Notably, ``traces`` allow callers to
contextualise the errors produced by its callees.

.. _rpc_security:

RPC security
------------

During the development of the codebase a lot of RPC endpoints were created, some
of which are responsible for delicate or computationally intense tasks like
validating blocks or executing Michelson scripts. While some of them are
necessary for the node's users to interact with the blockchain, others are there
to expose API to processes responsible for baking and attesting, for
configuration or debugging purposes or to facilitate development of smart
contracts.

In order to mitigate risks related to exposing these endpoints, Access Control
Lists (ACL for short) were introduced to limit the scope of the API exposed to
public networks (see also :ref:`configure_rpc`). While node administrators are
free to configure these ACLs however they like, there is :ref:`the default ACL
<default_acl>`, which lists all the endpoints that are **exposed by default**.

When adding a new RPC endpoint, please consider whether or not there is a reason
to call it over a public network. If the answer is yes, you should probably
consider adding the new endpoint to the ACL. If there are also risks related to
calling the endpoint by a potentially malicious user, they should be weighed
when making the decision too. There are no simple answers here. Remember that
all new endpoints are **blocked by default** unless explicitly added to the ACL.

When changing an existing public RPC endpoint it is also important to consider,
how does the change impact possible risks related to calling the endpoint.
Should it be removed from the ACL?

.. _RPC-versioning-dev:

RPC Versioning
--------------

General information about RPC versioning can be found in
:doc:`../introduction/versioning`.

How to Version an RPC
~~~~~~~~~~~~~~~~~~~~~

If an RPC already has a query parameter ``version``, just add a
variant to the corresponding type ``t_with_version`` (see
:ref:`RPC-versioning-dev-adding-an-rpc`). Otherwise, the ``version``
query parameter should be added (a natural number starting from
``0``). See example `here
<https://gitlab.com/tezos/tezos/-/merge_requests/3480>`_.

For versioning an RPC which returns a type ``t``, you have to write in
the service module of the RPC a type ``t_with_version`` which has one
constructor by version. The encoding of ``t_with_version`` is simply
constructed using the ``Data_encoding.union`` function.

To ensure that the implementation of the RPC (the directory module)
uses the version parameter we recommend that ``t_with_version`` is
abstract and a dispatcher is written in the service file. This way,
when a new version is added, only the dispatcher function needs to be
updated. In general, the type for this dispatcher will be:

.. code-block:: ocaml

   val t_dispatcher : t -> ~version:int -> t_with_version

A similar process can be followed to modify the input of an RPC.
However, in that case, the semantics of ``union`` makes the parameter
``version`` optional (all versions are supported by default). It is
still interesting to version RPCs to allow removing older versions in the
future.

Notice that we use only one version number for both input and output
of an RPC.

.. _RPC-versioning-dev-adding-an-rpc:

When to Add a New Version to an RPC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you modify the input or the output of an RPC, it is not always
necessary to create a new version for the RPC. A new version should be
brought in when a **breaking change** is introduced. Assume that an
RPC returns the following JSON value:

.. code-block:: json

    {
      "foo": 5,
      "bar": {
        "baz": 10
      }
    }

If you introduce a new field ``foobar`` like this:

.. code-block:: json

    {
      "foo": 5,
      "bar": {
        "baz": 10,
        "foobar" : 5
      }
    }

it should not be considered a breaking change. Indeed, many
decoders which accept the former value also accept the latter.

However, if you remove a field or change the encoding of a field in a
non-extensible way as above, it should be considered a breaking change
like the two examples below.

.. code-block:: json

    {
      "foo": "bar",
      "bar": {
        "baz": 10
      }
    }

.. code-block:: json

    {
      "bar": {
        "baz": 10
      }
    }


Linting
-------

The OCaml part of Octez code is analysed by a linter. You can check more details
in :src:`scripts/semgrep/README.md`. Below are explanations for the different
rules that may trigger linting errors.

.. _linting-list-lengths-comparison:

Comparing the length of two lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rule detects inefficient comparisons between two list lengths and suggests
more efficient specialised functions.

When comparing the lengths of two lists it might be tempting to compute the
lengths and compare them. This seems the most straightforward approach.
Unfortunately, this approach is costly. Specifically,
``List.length xs > List.length ys`` is O(``length(xs)``+``length(ys)``) because
each list is traversed in full.

The OCaml ``Stdlib.List`` module provides the function
``compare_lengths : 'a list -> 'b list -> int`` which traverses both lists at
once, and only as much of it as is necessary to determine which is longer.
Consequently, the cost of
``List.compare_lengths xs ys`` is O(min(``length(xs)``, ``length(ys)``) because
the function stops when it reaches the end of one list.

The value returned by ``compare_lengths`` is compatible with the semantic of
other comparison functions in the Stdlib. This means that the naive comparison
``List.length xs > List.length ys`` can be rewritten more efficiently as
``List.compare_lengths xs ys > 0`` (note the same comparison operator is used).

In Octez, there is also ``Compare.List_lengths`` which provides infix operators
to compare the lengths of two lists directly. The same example can be rewritten
``Compare.List_lengths.(xs > ys)``.


.. _linting-list-length-comparison:

Comparing the length of a list to a constant
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rule detects inefficient comparisons between a list length and a constant
and suggests more efficient specialised functions.

When comparing the length of a list to a constant it might be tempting to
compute the length and compare it to the constant. This seems the most
straightforward approach. Unfortunately, this approach is costly. Specifically,
``List.length xs > k`` is O(``length(xs)``) because the expression traverses the
entirety of ``xs``.

The OCaml ``Stdlib.List`` module provides the function
``compare_length_with : 'a list -> int -> int`` which only traverses as much of
the list as is necessary to determine if it is longer than the constant. The
cost of ``List.compare_length_with xs k`` is O(min(``length(xs)``, ``k``)
because it stops when it reaches the end of ``xs`` or after traversing ``k``
elements.

The value returned by ``compare_length_with`` is compatible with the semantic of
other comparison functions in the Stdlib. This means that the expression
``List.length xs > k`` can be rewritten more efficiently as
``List.compare_length_with xs k > 0`` (note the same comparison operator is
used).

In Octez, there is also ``Compare.List_length_with`` which provides infix
operators to compare the length of a list to a constant directly. The same
example can be written ``Compare.List_length_with.(xs > k)``.

.. _linting-list-length-empty:

Comparing the length of a list with zero
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A common special case of the former consists in testing if a list is
empty. While ``List.compare_length_with xs 0 = 0`` or
``Compare.List_length_with.(xs = 0)`` test this property in constant
time, pattern-matching on ``xs`` or calling ``List.is_empty xs``
should generally be preferred for readability.

.. _linting-folding-over-a-promise:

Folding over a promise or a result
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rule detects difficult-to-read patterns of code wherein you traverse a
list, with an Lwt promise or a result as accumulator.

When folding over a list (using ``List.fold_left`` or ``List.fold_right``), the
accumulator can be of any type. In particular, it can be an Lwt promise or a
result, and the folding can perform some additional control-flow.

This is valid code accepted by the compiler. But it often produces code which is
difficult to read. For example, when folding over a promise, a small change can
affect whether the traversal sequential (one element at a time) or concurrent
(all elements treated at the same time).

To make the code more readable, you should use the functions provided in the
Octez support libraries. Specifically, the ``List`` module in Octez includes
Lwt-, Result-, and Lwt-Result-specific variants of all the traversal functions
(``map``, ``iter``, ``for_all``, ``exists``, etc.)

Check the
:package-api:`online documentation <octez-libs/Tezos_base/TzPervasives/List/index.html>` for a full list of the content of the ``List`` module.

.. _chaining_concat_map:

Chaining List.concat and List.map
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rule detects compositions of ``List.concat`` and ``List.map`` that are
suboptimal.

The specialised ``List.concat_map`` function is equivalent to, but more
efficient than, the composition of ``List.concat`` and ``List.map``. The
specialised traversor is even tail-recursive.

Lwtreslib provides additional combinators ``List.concat_map_s``,
``List.concat_map_e``, and ``List.concat_map_es`` to replace the non-vanilla
compositions.

Check the :package-api:`online documentation <octez-libs/Tezos_lwt_result_stdlib/Lwtreslib/Bare/List/index.html#val-concat_map>`.

Naming
------

Good names help maintainers quickly understand the code by concisely suggesting what a variable represents or what a function computes.

This implies that good names precisely evoke the denoted value(s), without being longer than necessary:

* *precision*: avoid using too generic names such as "buffer", "counter", or "result"; give instead some hint about what is stored, counted, or computed.
* *concision*: as you cannot put everything into a name, select the most important notion(s), and avoid redundant words (abbreviating or not is up to you; it's just about avoiding names longer than necessary).
  For instance, suffixing the name with its type such as "..._list" is not needed since the IDE can tell its type; just using plural is usually enough.
  On the contrary, it is useful to add details to the type, such as "..._dir" or "..._title" for a value of type string.

Beyond choosing good individual names, it is also important to ensure *consistency* of naming.
For example, if within a same module, "operation" is used sometimes for Michelson operations and other times for arithmetic operations, this introduces ambiguity, that can be solved by refining the naming scheme.

Other coding conventions
------------------------

Other than the guidelines above, Octez developers should be aware
of general `OCaml programming guidelines <https://caml.inria.fr/resources/doc/guides/guidelines.en.html>`_, which recommend formatting, naming conventions,
and more.

.. [1]
   J. Ousterhout, `A Philosophy of Software Design <https://web.stanford.edu/~ouster/cgi-bin/aposd.php>`__, Yaknyam Press, 2021.
