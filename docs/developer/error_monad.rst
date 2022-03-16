The Error Monad
===============

There are multiple ways to deal with errors in OCaml: exceptions,
``result``, ``option``, etc., each approach has strengths and
weaknesses. However, in Octez we settle on one, single, uniform way of
dealing with errors: the *error monad*.

This is a tutorial for the error monad. It makes the following
assumptions

-  You can read English.
-  You have a basic familiarity with OCaml.
-  You have a basic familiarity with Lwt (if not, this tutorial links to
   Lwt resources when it becomes necessary).

Other than that, each section of this tutorial can be understood using
the information of previous sections, no additional information is
necessary. Note, however, that most sections also make forward
references along the lines of “more details on this later” or
“alternatives are presented later”. During a first read, this
foreshadowing is only useful in that it lets you know that the
information is coming and that you can move on with a partial
understanding. On subsequent reads, it helps putting things in context.

Note that the core of this tutorial focuses on error management in Octez.
For a short section on error management in the protocol see Part 3 :ref:`error_monad_within_protocol`.

.. toctree::
   :maxdepth: 2

   error_monad_p1_result

.. toctree::
   :maxdepth: 2

   error_monad_p2_tzresult

.. toctree::
   :maxdepth: 2

   error_monad_p3_advanced

.. toctree::
   :maxdepth: 2

   error_monad_p4_appendices
