(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This library contains unofficial extensions to the [QCheck] library.

    - [QCheck2_extra.Stateful.Gen] works much like [QCheck2.Gen], but allows
      embedding of side-effects.
  *)

(** A non-empty list *)
module Non_empty : sig
  type 'a t = {head : 'a; tail : 'a list}

  exception Empty_list

  (** Make a non-empty list from a pair. *)
  val of_pair : 'a * 'a -> 'a t

  (** Make a non-empty list from a list, raising an exception
      on empty list.
    *)
  val of_list_exn : 'a list -> 'a t

  (** Return the length of the given list. *)
  val length : 'a t -> int

  (** Index the given list, raising an exception on bounds error.
      For testing only.
    *)
  val nth_exn : 'a t -> int -> 'a
end

(** Functor abstracts over type constructors that can be mapped over.

    A [Functor] instance should satisfy:

      - Composition:
        [ map f << map g = map (f << g) ]

      - Identity:
        [ map id = id ]

      assuming [ f << g = fun x -> g (f x) ].

 *)
module Functor : sig
  module type S = sig
    type 'a t

    (** Map over the given value. *)
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
end

(** Applicative extends [Functor] with an [map2] and [return] method, allowing us
    to lift functions of arbitrary arity.

    We can define [and+] in terms of [map2] and vice versa:

    {[
    let (and+) = map2 (fun x y -> (x,y))

    let map2 f x y =
      let+ a = x
      and+ b = y in
      f x y
    ]}
    assuming

    [ (and+) = product ]

    and

    [ (let+) x f = map f x ].

    An [Applicative] should satisfy:

      - Associativity:
        [ product (product x y) z = product x (product y z) ]
      - Identities:
        [ product (return ()) x = x = product x (return ()) ]
 *)
module Applicative : sig
  module type S = sig
    include Functor.S

    (** Inject the given value. *)
    val return : 'a -> 'a t

    (** Map a binary function over the given value. *)
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (** Combine the given values. See also [map2]. *)
    val product : 'a t -> 'b t -> ('a * 'b) t
  end
end

(** [Monad] extends the [Applicative] type class with a new function [join].
    [join] takes a value in a nested context [a t t] and joins them together so
    that we have a single context [a t].

    We can define [let*] in terms of [join] and vice versa:

    {[
    let ( let* ) x f = join (map f x)
    let join x =
      let* y = x in
      y
    ]}

    assuming

    [ ( let* ) = bind ].

    A [Monad] should satisfy:

      - Associativity:
        [ (f >=> g) >=> h = f >=> (g >=> h) ]
      - Identities:
        [ return >=> f = f = f >=> return ]

    assuming

    [ ( let* ) = bind ].

    and

    {[ f >=> g =
      fun x ->
        let* y = f x in
        let* z = g y in
        return z
    ]}

 *)
module Monad : sig
  module type S = sig
    include Applicative.S

    (** Monadic join operator. *)
    val join : 'a t t -> 'a t

    (** Monadic bind operator. *)
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Utility to import let-syntax for any [Monad].

      Usage: [let open Monad.Syntax(MyMonad) in <expr>]
    *)
  module Syntax (M : S) : sig
    val ( let* ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t

    val ( and* ) : 'a M.t -> 'b M.t -> ('a * 'b) M.t

    val ( let+ ) : 'a M.t -> ('a -> 'b) -> 'b M.t

    val ( and+ ) : 'a M.t -> 'b M.t -> ('a * 'b) M.t
  end
end

(** The identity type constructor.
 *)
module Identity : sig
  include Monad.S

  val run : 'a t -> 'a
end

module Stateful_gen : sig
  module type S = sig
    include Monad.S

    type 'a m

    (** Generate a [bool]. *)
    val bool : bool t

    (** Generate a [nat] smaller than the given value. *)
    val nat_less_than : int -> int t

    (** Generate a small [int]. *)
    val small_int : int t

    (** Generate a list of the given length. *)
    val replicate : int -> 'a t -> 'a list t

    (** Run a predetermined generator for each element in a list, and
        combine the result with the list element.

        See [traverse] for a more general form. *)
    val replicate_for_each : 'a list -> 'b t -> ('a * 'b) list t

    (** Produce a generator for each element in a list. *)
    val traverse : ('a -> 'b t) -> 'a list -> 'b list t

    (** Return a generator that picks and invokes one of the given generators,
          uniformly distributed. *)
    val oneof : 'a t Non_empty.t -> 'a t

    (** Generate a short list. *)
    val small_list : 'a t -> 'a list t

    (** Generate an optional value. *)
    val opt : 'a t -> 'a option t

    (** Generate a readable character. *)
    val char_readable : char t

    (** Generate a readable string. *)
    val string_readable : string t

    (** Lift the underlying computation type into a generator. *)
    val lift : 'a m -> 'a t

    (** Convert a [QCheck_extra] generator to a [QCheck] generator.

        The QCheck generator is run once once and the result is used as the seed
        of the given generator.

        {i Warning} Because [Stdlib.Random.Gen] and [QCheck.Gen] have mutable
        state, you must make sure this is only called {i once}, to get
        predictable results.
       *)
    val to_qcheck_gen : 'a t -> 'a m QCheck.Gen.t
  end

  (** Produces a pseudo-random generator that can execute side-effects in
    an underlying [Monad.S].

      Applied to [Identity] this is similar to [QCheck.Gen].

      This is a straight RNG without shrinking.
   *)
  module Make (F : Monad.S) : S with type 'a m = 'a F.t

  module Default : S with type 'a m = 'a Identity.t
end
