================================
Making legacy code unit-testable
================================

This guide describes how to make legacy code testable and how to test it.
The following techniques do not require any API change: they concern testing each single module as a **unit** without touching modules depending on it.
These techniques are applicable to **legacy** code: you do not need to have an exact knowledge of every implementation detail of a module to test it.

The goals are:

- to gain confidence in a module as a developer (or even part of test-driven development)
- to document it: tests are examples
- to have a safety net as a maintainer (non-regression testing), e.g., when:
  - adding/changing features
  - performing various forms of refactoring (changing the API or not)

In the rest of this page, "testable" means "unit-testable", for conciseness.

Here "legacy" is a loose term, encompassing:

- existing code written by other developers, not necessarily containing a right amount of built-in unit tests
- code that is only tested by heavy/slow/high level tests, like :doc:`Tezt <tezt>` - this is often a symptom of code that is hard to unit-test
- tightly-coupled code (e.g. module ``A`` directly depends on functions of modules ``B``, ``C`` and ``D``, thus unit-testing ``A`` requires setting everything up for ``B``, ``C`` and ``D`` too, which not only makes unit testing harder, but is also complex and brittle)
- code that calls system functions (e.g. ``Unix.chmod``, which requires modifying actual files on the filesystem, also making unit testing harder) or more generally any kind of side-effecting function

Solutions
=========

Testing a module and all its dependencies
-----------------------------------------

Sometimes, legacy modules depending on other modules may be "unit-tested" without modifying them at all.

This approach is technically not "unit" testing, as the module/function is not tested in isolation from its dependencies. However as the intention is close to unit testing, we include it here.

Pros:

- No change at all in business code (not even an ``Internal_for_tests`` module)

Cons:

- Code not tested in isolation
- Tests are harder to write and maintain

Works best when:

- Code and its dependencies are pure, or "pure enough" (e.g. code may contain logging and other unharmful/unimportant side effects)
- Dependency tree is not too deep (e.g. if A depends on B which depends on C which depends on D, then it becomes hard to write/read/maintain unit tests)

Using function records
----------------------

This approach decouples the business code from its dependency functions, either by storing dependency functions in the state - if any - or by taking them as additional parameters in each module function.

Note that this does not decouple types, only functions!
If your module depends on a dependency type that can only be built by having side-effects, then you are forced to have those side effects too in your tests.

As an example, consider the following ``Counter`` module (signature and implementation):

.. code:: ocaml

    module Counter : sig
      type t
      val create_from_file : unit -> t
      val increment_and_save_to_file : t -> t
    end = struct
      type t = {
        counter : int;
      }

      let create_from_file () =
        let counter = int_of_string (Files.read "/tmp/counter.txt") in
        {counter}

      let increment_and_save_to_file t =
        let t = {counter = t.counter + 1} in
        Files.write "/tmp/counter.txt" (string_of_int t.counter);
        t
    end

``Counter`` is hardly unit-testable because of the calls to ``Files.read`` and ``Files.write`` which have the side effect of reading from/writing into ``/tmp/counter.txt`` file.
Thus we extract ``Files.read`` and ``Files.write`` into an argument that can be changed with a dumb ("mock") function in tests.

Store dependencies in state
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One way to achieve this - when your module has a state, usually a type ``t`` - is to store all dependency functions in a field, here ``dependencies``:

.. code:: ocaml

    module Counter : sig
      type t
      val create_from_file : unit -> t
      val increment_and_save_to_file : t -> t

      module Internal_for_tests : sig
        type dependencies = {
          files_read : string -> string;
          files_write : string -> string -> unit;
        }
        val create_from_file : dependencies -> unit -> t
      end
    end = struct
      type dependencies = {
        files_read : string -> string;
        files_write : string -> string -> unit;
      }

      type t = {
        counter : int;
        dependencies : dependencies;
      }

      let create_from_file_internal dependencies () =
        let counter = int_of_string (dependencies.files_read "/tmp/counter.txt") in
        {counter; dependencies}

      let create_from_file = create_from_file_internal {files_read = Files.read; files_write = Files.write}

      let increment_and_save_to_file t =
        let t = {t with counter = t.counter + 1} in
        t.dependencies.files_write "/tmp/counter.txt" (string_of_int t.counter);
        t

      module Internal_for_tests = struct
        type nonrec dependencies = dependencies = {
          files_read : string -> string;
          files_write : string -> string -> unit;
        }

        let create_from_file = create_from_file_internal
      end
    end

Note that the direct calls to ``Files.read`` and ``Files.write`` were replaced with indirect calls to ``dependencies.files_read`` and field ``t.dependencies.files_write``:

- They are set to ``Files.[read|write]`` in the business constructor ``Counter.create_from_file``
- They are changed at will in the test constructor ``Counter.Internal_for_tests.create_from_file``

Also note that while the API was extended with test artifacts under the ``Internal_for_tests`` sub-module, the public API is otherwise unchanged, thus keeping this refactoring local - you do not need to change any call sites!

Now we can test this module without any side effect:

.. code:: ocaml

    let test () =
      let counter_value_written = ref "" in
      let fake_files_read file_name = "41" in
      let fake_files_write file_name text =
        counter_value_written := text
      in
      let counter = Counter.Internal_for_tests.create_from_file {files_read = fake_files_read; files_write = fake_files_write} () in
      let _ = Counter.increment_and_save_to_file counter in
      Alcotest.(check string) "counter value was incremented in file" !counter_value_written "42"

Taking dependencies in function argument
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An alternative solution, more verbose but not requiring any "state" value available in each function, is to take the dependencies directly as an additional function argument:

.. code:: ocaml

    module Counter : sig
      type t
      val create_from_file : unit -> t
      val increment_and_save_to_file : t -> t

      module Internal_for_tests : sig
        type dependencies = {
          files_read : string -> string;
          files_write : string -> string -> unit;
        }

        val create_from_file : dependencies -> unit -> t
        val increment_and_save_to_file : dependencies -> t -> t
      end
    end = struct
      type dependencies = {
        files_read : string -> string;
        files_write : string -> string -> unit;
      }

      type t = {
        counter : int;
      }

      let business_dependencies = {
        files_read = Files.read;
        files_write = Files.write;
      }

      let create_from_file_internal dependencies counter =
        let counter = int_of_string (dependencies.files_read "/tmp/counter.txt") in
        {counter}

      let create_from_file = create_from_file_internal business_dependencies

      let increment_and_save_to_file_internal dependencies t =
        let t = {counter = t.counter + 1} in
        dependencies.files_write "/tmp/counter.txt" (string_of_int t.counter);
        t

      let increment_and_save_to_file t = increment_and_save_to_file_internal business_dependencies t

      module Internal_for_tests = struct
        type nonrec dependencies = dependencies = {
          files_read : string -> string;
          files_write : string -> string -> unit;
        }

        let create_from_file = create_from_file_internal
        let increment_and_save_to_file = increment_and_save_to_file_internal
      end
    end

Note that the direct calls to ``Files.read`` and ``Files.write`` were replaced with indirect calls to arguments ``dependencies.files_read`` and ``dependencies.files_write``:

- They are set to ``Files.[read|write]`` in each business function (``create_from_file`` and ``increment_and_save_to_file``)
- They are changed at will in the test function ``Counter.Internal_for_tests.[create_from_file|increment_and_save_to_file]``

As in the previous solution, notice that the public API has not changed - save for additional APIs in ``Internal_for_tests``.

Now we can test this module without any side effect:

.. code:: ocaml

    let test () =
      let counter_value_written = ref "" in
      let fake_files_read file_name = "41" in
      let fake_files_write file_name text =
        counter_value_written := text
      in
      let mock_dependencies = Counter.Internal_for_tests.{
        files_read = fake_files_read;
        files_write = fake_files_write;
      } in
      let counter = Counter.Internal_for_tests.create_from_file mock_dependencies () in
      let _ = Counter.Internal_for_tests.increment_and_save_to_file mock_dependencies counter in
      Alcotest.(check string) "counter value was incremented in file" !counter_value_written "42"

Pros and Cons for Function records
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Works best when:

- Dependency types are not too hard to build

Pros:

- No side-effecting function is called (they are replaced with mocks)
- Enables validating the arguments passed to mock functions (e.g. ``counter_value_written``) have the right value
- Independent of the dependency depth (for functions): if ``A`` calls ``B.f`` which calls ``C.g``, your mock of ``B.f`` will never call ``C.g``

Cons:

- All dependency types remain, so if it is difficult/side-effectful to create those values, testing remains difficult/not so unitary
- Adds a bit of boilerplate in ``Internal_for_tests`` module
- Adds a bit of indirection, by introducing indirect calls to dependency functions. The associated performance overhead should be negligible in most practical cases. There also is a slight decrease in code readability, but documenting this unit-testability pattern should avoid many headaches.

To choose between the field and the argument:

- If your module already has a kind of "state" (usually a type ``t``), then add a ``dependencies`` field
- Else add a ``dependencies`` argument - but this requires duplicating each function, which ends up being very verbose if you have several functions
- If your "state" value (usually a value of type ``t``) is passed to a polymorphic function like ``=`` or ``compare`` (which throw on function fields, and are famous for being an anti-pattern), and it is not possible for you to fix this anti-pattern, then either switch to function arguments, or wrap in an object.

Using functors
--------------

This approach decouples the business code from its dependency modules.
Note that unlike the Function records solution, this decouples both dependency functions **and abstract types**!

Consider the following code: it is similar to the previous ``Counter`` example but this time, the ``Files`` dependency module (which could be another module, a third party library, or even the ``Stdlib``) also has an abstract type ``t``:

.. code:: ocaml

    (* The dependency *)
    module Files : sig
      type t
      val openf : string -> t
      val write : t -> string -> unit
      val close : t -> unit
      (* Many other functions and types *)
    end = struct (* omitted implementation *) end

    module Counter : sig
      type t
      val create : int -> t
      val increment_and_save_to_file : t -> t
    end = struct
      type t = {
        counter : int;
      }

      let create counter = {counter}

      let increment_and_save_to_file t =
        let t = {counter = t.counter + 1} in
        let file = Files.openf "/tmp/counter.txt" in
        Files.write file (string_of_int t.counter);
        Files.close file;
        t
    end

The technique is to transform ``Counter`` into a functor that takes a module looking like ``Files`` in argument - but which can now be changed in tests!

.. code:: ocaml

    module Counter : sig
      module type S = sig
        type t
        val create : int -> t
        val increment_and_save_to_file : t -> t
      end

      include S

      module Internal_for_tests : sig
        module type FILES = sig
          type t
          val openf : string -> t
          val write : t -> string -> unit
          val close : t -> unit
        end
        module Make (Files : FILES) : S
      end
    end = struct
      module type S = sig
        type t
        val create : int -> t
        val increment_and_save_to_file : t -> t
      end

      module type FILES = sig
        type t
        val openf : string -> t
        val write : t -> string -> unit
        val close : t -> unit
      end

      module Make (Files : FILES) = struct
        type t = {
          counter : int;
        }

        let create counter = {counter}

        let increment_and_save_to_file t =
          let t = {counter = t.counter + 1} in
          let file = Files.openf "/tmp/counter.txt" in
          Files.write file (string_of_int t.counter);
          Files.close file;
          t
      end

      (* Do not be mistaken: here [Files] refers to the real, business [Files] module! *)
      include Make (Files)

      module Internal_for_tests = struct
        module type FILES = FILES

        module Make = Make
      end
    end

As you can see, this is significantly more verbose!

However, now we can freely change/mock not only the dependency functions ``Files.[openf|close|write]``, but also the implementation of type ``Files.t``!

.. code:: ocaml

    let test () =
      let written_content = ref "" in
      let module Counter = Counter.Internal_for_tests.Make (struct
        type t = unit
        let openf _ = ()
        let close _ = ()
        let write _ content = written_content := content
      end) in
      let counter = Counter.create 41 in
      let _ = Counter.increment_and_save_to_file counter in
      Alcotest.(check string) "counter value was incremented in file" !written_content "42"

While the real ``Files.t`` type probably contained a file descriptor, our mock module has no side effect outside of the test!

Note on verbosity: some things are duplicated because of OCaml MLI syntax, e.g. module type declaration.
This can be partially mitigated by using `the \_intf trick <https://www.craigfe.io/posts/the-intf-trick>`__ but this in turn induces a bit more complexity, use with caution.

Works best when:

- You need to decouple (abstract) types, not only functions. For example, because building values of those types adds too much complexity, or requires side-effects.
- There are linear dependencies in modules (``A`` depends on ``B`` which depends on ``C``, but ``A`` does not depend on ``C``)

Pros:

- Everything but exposed and private dependency types are mocked
- Enables validating the arguments passed to mock functions (e.g. ``counter_value_written``) have the right value
- Independent of the dependency depth (for functions): if ``A`` calls ``B`` which calls ``C``, your mock of ``B`` will never call ``C`` nor refer to its abstract types

Cons:

- Verbosity
- Additional complexity (functors, module types)
- Does not scale well in more complex dependencies (``A`` depends on ``B`` and ``C`` types, and ``B`` also depends on ``C`` types) as it induces a lot of destructive substitutions and module noise to convince the typechecker that ``C.t`` in ``A`` is the same as ``C.t`` in ``B``
- Does not work for exposed and private (exposed in read-only) dependency types
