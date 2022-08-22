(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)


(** {2 Fundamentals} *)

(** {3 Promises} *)

type +'a t
(** Promises for values of type ['a].

    A {b promise} is a memory cell that is always in one of three {b states}:

    - {e fulfilled}, and containing one value of type ['a],
    - {e rejected}, and containing one exception, or
    - {e pending}, in which case it may become fulfilled or rejected later.

    A {e resolved} promise is one that is either fulfilled or rejected, i.e. not
    pending. Once a promise is resolved, its content cannot change. So, promises
    are {e write-once references}. The only possible state changes are (1) from
    pending to fulfilled and (2) from pending to rejected.

    Promises are typically “read” by attaching {b callbacks} to them. The most
    basic functions for that are {!Lwt.bind}, which attaches a callback that is
    called when a promise becomes fulfilled, and {!Lwt.catch}, for rejection.

    Promise variables of this type, ['a Lwt.t], are actually {b read-only} in
    Lwt. Separate {e resolvers} of type ['a ]{!Lwt.u} are used to write to them.
    Promises and their resolvers are created together by calling {!Lwt.wait}.
    There is one exception to this: most promises can be {e canceled} by calling
    {!Lwt.cancel}, without going through a resolver. *)

(** We omit [u], [wait], [wakeup*] and so on because these are only useful to
    define new synchronization primitives which the protocol doesn't need: it
    gets its synchronization primitives from the environment. *)

val return : 'a -> 'a t
(** [Lwt.return v] creates a new {{: #TYPEt} promise} that is {e already
    fulfilled} with value [v].

    This is needed to satisfy the type system in some cases. For example, in a
    [match] expression where one case evaluates to a promise, the other cases
    have to evaluate to promises as well:

{[
match need_input with
| true -> Lwt_io.(read_line stdin)   (* Has type string Lwt.t... *)
| false -> Lwt.return ""             (* ...so wrap empty string in a promise. *)
]}

    Another typical usage is in {{: #VALbind} [let%lwt]}. The expression after
    the “[in]” has to evaluate to a promise. So, if you compute an ordinary
    value instead, you have to wrap it:

{[
let%lwt line = Lwt_io.(read_line stdin) in
Lwt.return (line ^ ".")
]} *)

(** We omit [fail] as well as [catch] and such because we discourage the use of
    exceptions in the environment. The Error Monad provides sufficient
    primitives. *)

(** {3 Callbacks} *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [Lwt.bind p_1 f] makes it so that [f] will run when [p_1] is {{: #TYPEt}
    {e fulfilled}}.

    When [p_1] is fulfilled with value [v_1], the callback [f] is called with
    that same value [v_1]. Eventually, after perhaps starting some I/O or other
    computation, [f] returns promise [p_2].

    [Lwt.bind] itself returns immediately. It only attaches the callback [f] to
    [p_1] – it does not wait for [p_2]. {e What} [Lwt.bind] returns is yet a
    third promise, [p_3]. Roughly speaking, fulfillment of [p_3] represents both
    [p_1] and [p_2] becoming fulfilled, one after the other.

    A minimal example of this is an echo program:

{[
let () =
  let p_3 =
    Lwt.bind
      Lwt_io.(read_line stdin)
      (fun line -> Lwt_io.printl line)
  in
  Lwt_main.run p_3

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    Rejection of [p_1] and [p_2], and raising an exception in [f], are all
    forwarded to rejection of [p_3].

    {b Precise behavior}

    [Lwt.bind] returns a promise [p_3] immediately. [p_3] starts out pending,
    and is resolved as follows:

    - The first condition to wait for is that [p_1] becomes resolved. It does
      not matter whether [p_1] is already resolved when [Lwt.bind] is called, or
      becomes resolved later – the rest of the behavior is the same.
    - If and when [p_1] becomes resolved, it will, by definition, be either
      fulfilled or rejected.
    - If [p_1] is rejected, [p_3] is rejected with the same exception.
    - If [p_1] is fulfilled, with value [v], [f] is applied to [v].
    - [f] may finish by returning the promise [p_2], or raising an exception.
    - If [f] raises an exception, [p_3] is rejected with that exception.
    - Finally, the remaining case is when [f] returns [p_2]. From that point on,
      [p_3] is effectively made into a reference to [p_2]. This means they have
      the same state, undergo the same state changes, and performing any
      operation on one is equivalent to performing it on the other.

    {b Syntactic sugar}

    [Lwt.bind] is almost never written directly, because sequences of [Lwt.bind]
    result in growing indentation and many parentheses:

{[
let () =
  Lwt_main.run begin
    Lwt.bind Lwt_io.(read_line stdin) (fun line ->
      Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
        Lwt_io.printf "One second ago, you entered %s\n" line))
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The recommended way to write [Lwt.bind] is using the [let%lwt] syntactic
    sugar:

{[
let () =
  Lwt_main.run begin
    let%lwt line = Lwt_io.(read_line stdin) in
    let%lwt () = Lwt_unix.sleep 1. in
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    This uses the Lwt {{: Ppx_lwt.html} PPX} (preprocessor). Note that we had to
    add package [lwt_ppx] to the command line for building this program. We will
    do that throughout this manual.

    Another way to write [Lwt.bind], that you may encounter while reading code,
    is with the [>>=] operator:

{[
open Lwt.Infix

let () =
  Lwt_main.run begin
    Lwt_io.(read_line stdin) >>= fun line ->
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printf "One second ago, you entered %s\n" line
  end

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
]}

    The [>>=] operator comes from the module {!Lwt.Infix}, which is why we
    opened it at the beginning of the program.

    See also {!Lwt.map}. *)



(** We omit [dont_wait] and other such functions because they are only useful
    in mutation-heavy loosely-synchronised code which the protocol shouldn't be.
    *)

(** We omit many synchronisation primitives such as [choose] because they
    introduce non-determinism. *)

(** We omit cancelation-related primitives because we discourage Cancelation in
    the protocol. *)

(** {2 Convenience} *)

(** {3 Callback helpers} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [Lwt.map f p_1] is similar to {!Lwt.bind}[ p_1 f], but [f] is not expected
    to return a promise.

    This function is more convenient that {!Lwt.bind} when [f] inherently does
    not return a promise. An example is [Stdlib.int_of_string]:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.map
    int_of_string
    Lwt_io.(read_line stdin)

let () =
  Lwt_main.run begin
    let%lwt number = read_int () in
    Lwt_io.printf "%i\n" number
  end

(* ocamlfind opt -linkpkg -thread -package lwt_ppx,lwt.unix code.ml && ./a.out *)
]}

    By comparison, the {!Lwt.bind} version is more awkward:

{[
let read_int : unit -> int Lwt.t = fun () ->
  Lwt.bind
    Lwt_io.(read_line stdin)
    (fun line -> Lwt.return (int_of_string line))
]}

    As with {!Lwt.bind}, sequences of calls to [Lwt.map] result in excessive
    indentation and parentheses. The recommended syntactic sugar for avoiding
    this is the {{: #VAL(>|=)} [>|=]} operator, which comes from module
    [Lwt.Infix]:

{[
open Lwt.Infix

let read_int : unit -> int Lwt.t = fun () ->
  Lwt_io.(read_line stdin) >|= int_of_string
]}

    The detailed operation follows. For consistency with the promises in
    {!Lwt.bind}, the {e two} promises involved are named [p_1] and [p_3]:

    - [p_1] is the promise passed to [Lwt.map].
    - [p_3] is the promise returned by [Lwt.map].

    [Lwt.map] returns a promise [p_3]. [p_3] starts out pending. It is resolved
    as follows:

    - [p_1] may be, or become, resolved. In that case, by definition, it will
      become fulfilled or rejected. Fulfillment is the interesting case, but the
      behavior on rejection is simpler, so we focus on rejection first.
    - When [p_1] becomes rejected, [p_3] is rejected with the same exception.
    - When [p_1] instead becomes fulfilled, call the value it is fulfilled with
      [v].
    - [f v] is applied. If this finishes, it may either return another value, or
      raise an exception.
    - If [f v] returns another value [v'], [p_3] is fulfilled with [v'].
    - If [f v] raises exception [exn], [p_3] is rejected with [exn]. *)

(** We omit explicit callback registration ([on_termination] and such) because
    it is only useful for mutation-heavy code *)

(** We omit syntax helpers because they are available through the dedicated
    syntax modules of the Error Monad. *)

(** {3 Pre-allocated promises} *)

val return_unit : unit t
(** [Lwt.return_unit] is defined as {!Lwt.return}[ ()], but this definition is
    evaluated only once, during initialization of module [Lwt], at the beginning
    of your program.

    This means the promise is allocated only once. By contrast, each time
    {!Lwt.return}[ ()] is evaluated, it allocates a new promise.

    It is recommended to use [Lwt.return_unit] only where you know the
    allocations caused by an instance of {!Lwt.return}[ ()] are a performance
    bottleneck. Generally, the cost of I/O tends to dominate the cost of
    {!Lwt.return}[ ()] anyway.

    In future Lwt, we hope to perform this optimization, of using a single,
    pre-allocated promise, automatically, wherever {!Lwt.return}[ ()] is
    written. *)

val return_none : (_ option) t
(** [Lwt.return_none] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ None]. *)

val return_nil : (_ list) t
(** [Lwt.return_nil] is like {!Lwt.return_unit}, but for {!Lwt.return}[ []]. *)

val return_true : bool t
(** [Lwt.return_true] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ true]. *)

val return_false : bool t
(** [Lwt.return_false] is like {!Lwt.return_unit}, but for
    {!Lwt.return}[ false]. *)

(** We omit state introspection because it is discouraged when not defining new
    synchronisation primitives which the protocol doesn't do. *)

val return_some : 'a -> ('a option) t
(** Counterpart to {!Lwt.return_none}. However, unlike {!Lwt.return_none}, this
    function performs no {{: #VALreturn_unit} optimization}. This is because it
    takes an argument, so it cannot be evaluated at initialization time, at
    which time the argument is not yet available. *)

val return_ok : 'a -> (('a, _) result) t
(** Like {!Lwt.return_some}, this function performs no optimization.

    @since Lwt 2.6.0 *)

val return_error : 'e -> ((_, 'e) result) t
(** Like {!Lwt.return_some}, this function performs no optimization.

    @since Lwt 2.6.0 *)
