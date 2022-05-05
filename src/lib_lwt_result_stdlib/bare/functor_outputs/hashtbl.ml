(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Hashtables with the signature [S] are exception-safe replacements for
    hashtables with the {!Stdlib.Hashtbl.S} signature with Lwt- and result-aware
    traversal functions.

    See {!Lwtreslib}'s introductory documentation for explanations regarding
    [_e]-, [_s]-, [_es]-, [_p]-, and [_ep]-suffixed functions and exception
    safety. See {!Stdlib.Hashtbl.S} for explanations regarding OCaml's
    hashtables in general. *)
module type S = sig
  type key

  type !'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  val copy : 'a t -> 'a t

  val add : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val find : 'a t -> key -> 'a option

  val find_opt : 'a t -> key -> 'a option

  val find_all : 'a t -> key -> 'a list

  val replace : 'a t -> key -> 'a -> unit

  val mem : 'a t -> key -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_e :
    (key -> 'a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iter_es :
    (key -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t

  val iter_ep :
    (key -> 'a -> (unit, 'error) result Lwt.t) ->
    'a t ->
    (unit, 'error list) result Lwt.t

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

  val try_map_inplace : (key -> 'a -> ('a, 'trace) result) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_s : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val fold_e :
    (key -> 'a -> 'b -> ('b, 'trace) result) ->
    'a t ->
    'b ->
    ('b, 'trace) result

  val fold_es :
    (key -> 'a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a t ->
    'b ->
    ('b, 'trace) result Lwt.t

  val length : 'a t -> int

  val stats : 'a t -> Stdlib.Hashtbl.statistics

  val to_seq : 'a t -> (key * 'a) Stdlib.Seq.t

  val to_seq_keys : _ t -> key Stdlib.Seq.t

  val to_seq_values : 'a t -> 'a Stdlib.Seq.t

  val add_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit

  val replace_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit

  val of_seq : (key * 'a) Stdlib.Seq.t -> 'a t
end

(** Hashtables with the signature [SeededS] are exception-safe replacements for
    hashtables with the {!Stdlib.Hashtbl.SeededS} signature with Lwt- and
    result-aware traversal functions.

    See {!Lwtreslib}'s introductory documentation for explanations regarding
    [_e]-, [_s]-, [_es]-, [_p]-, and [_ep]-suffixed functions and exception
    safety. See {!Stdlib.Hashtbl.SeededS} for explanations regarding OCaml's
    seeded hashtables in general. *)
module type SeededS = sig
  type key

  type !'a t

  val create : ?random:bool -> int -> 'a t

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  val copy : 'a t -> 'a t

  val add : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val find : 'a t -> key -> 'a option

  val find_opt : 'a t -> key -> 'a option

  val find_all : 'a t -> key -> 'a list

  val replace : 'a t -> key -> 'a -> unit

  val mem : 'a t -> key -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_e :
    (key -> 'a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iter_es :
    (key -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t

  val iter_ep :
    (key -> 'a -> (unit, 'error) result Lwt.t) ->
    'a t ->
    (unit, 'error list) result Lwt.t

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

  val try_map_inplace : (key -> 'a -> ('a, 'trace) result) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_s : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val fold_e :
    (key -> 'a -> 'b -> ('b, 'trace) result) ->
    'a t ->
    'b ->
    ('b, 'trace) result

  val fold_es :
    (key -> 'a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a t ->
    'b ->
    ('b, 'trace) result Lwt.t

  val length : 'a t -> int

  val stats : 'a t -> Stdlib.Hashtbl.statistics

  val to_seq : 'a t -> (key * 'a) Stdlib.Seq.t

  val to_seq_keys : _ t -> key Stdlib.Seq.t

  val to_seq_values : 'a t -> 'a Stdlib.Seq.t

  val add_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit

  val replace_seq : 'a t -> (key * 'a) Stdlib.Seq.t -> unit

  val of_seq : (key * 'a) Stdlib.Seq.t -> 'a t
end

(** Hashtables with the signature [S_ES] are Hashtbl-like with the following
    differences:

    First, the module exports only a few functions in an attempt to limit the
    likelihood of race-conditions. Of particular interest is the following: in
    order to insert a value, one has to use `find_or_make` which either returns
    an existing promise for a value bound to the given key, or makes such a
    promise. It is not possible to insert another value for an existing key.
    This limits the table (e.g., it can only hold one value for any given key),
    but it forces the user to *atomically* test membership and insert an
    element.

    Second, the table is automatically cleaned. Specifically, when a promise for
    a value is fulfilled with an [Error _], the binding is removed. This leads
    to the following behavior:

{[
(* setup *)
let t = create 256 in
let () = assert (length t = 0) in

(* insert a first promise for a value *)
let p, r = Lwt.task () in
let i1 = find_or_make t 1 (fun () -> p) in
let () = assert (length t = 1) in

(* because the same key is used, the promise is not inserted. *)
let i2 = find_or_make t 1 (fun () -> assert false) in
let () = assert (length t = 1) in

(* when the original promise errors, the binding is removed *)
let () = Lwt.wakeup r (Error ..) in
let () = assert (length t = 0) in

(* and both [find_or_make] promises have the error *)
let () = match Lwt.state i1 with
  | Return (Error ..) -> ()
  | _ -> assert false
in
let () = match Lwt.state i2 with
  | Return (Error ..) -> ()
  | _ -> assert false
in
]}

    This automatic cleaning relieves the user from the responsibility of
    cleaning the table (which is another possible source of race condition).

    For consistency, traversal functions ignore [Error _] and rejections.

    Third, every time a promise is removed from the table (be it by [clean],
    [reset], or just [remove]), the promise is canceled.
*)
module type S_ES = sig
  type key

  type ('a, 'trace) t

  val create : int -> ('a, 'trace) t

  (** [clear tbl] cancels and removes all the promises in [tbl]. *)
  val clear : ('a, 'trace) t -> unit

  (** [reset tbl] cancels and removes all the promises in [tbl], and resizes
      [tbl] to its initial size. *)
  val reset : ('a, 'trace) t -> unit

  (** [find_or_make tbl k make] behaves differently depending on [k] being bound
      in [tbl]:
      - if [k] is bound in [tbl] then [find_or_make tbl k make] returns the
        promise [p] that [k] is bound to. This [p] might be already fulfilled
        with [Ok _] or it might be pending. This [p] cannot be already fulfilled
        with [Error _] or already rejected. This is because [Error]/rejected
        promises are removed from the table automatically. Note however that if
        this [p] is pending, [p] might become fulfilled with [Error _] or become
        rejected.
      - if [k] is not bound in [tbl] then [make ()] is called and the returned
        promise [p] is bound to [k] in [tbl]. Then [p] is returned. When [p] is
        resolved, it may be removed automatically from [tbl] as described above.
  *)
  val find_or_make :
    ('a, 'trace) t ->
    key ->
    (unit -> ('a, 'trace) result Lwt.t) ->
    ('a, 'trace) result Lwt.t

  (** [remove tbl k] cancels the promise bound to [k] in [tbl] and removes it.
      If [k] is not bound in [tbl] it does nothing. *)
  val remove : ('a, 'trace) t -> key -> unit

  val find : ('a, 'trace) t -> key -> ('a, 'trace) result Lwt.t option

  val mem : ('a, 'trace) t -> key -> bool

  (** [iter_with_waiting_es f tbl] iterates [f] over the bindings in [tbl].

      Specifically, for each binding [(k, p)] it waits for [p] to be fulfilled
      with [Ok v] and calls [f k v]. If [p] fulfills with [Error _] or is
      rejected, then no call to [f] is made for this binding. Note however that
      an [Error]/rejection in one promise returned by [f] interrupts the
      iteration.

      It processes bindings one after the other: it waits for both the bound
      promise to resolve and then the call promise to resolve before continuing
      to the next binding. *)
  val iter_with_waiting_es :
    (key -> 'a -> (unit, 'trace) result Lwt.t) ->
    ('a, 'trace) t ->
    (unit, 'trace) result Lwt.t

  (** [iter_with_waiting_ep f tbl] iterates [f] over the bindings in [tbl].

      Specifically, for each binding [(k, p)] it waits for [p] to be fulfilled
      with [Ok v] and calls [f k v]. If [p] fulfills with [Error _] or is
      rejected, then no call is made for this binding.

      Note however that if one (or more) of the promises returned by [f] ends in
      [Error]/rejection, the final result of this promise is an
      [Error]/rejection. Even so, it only resolves once all the promises have.

      It processes all bindings concurrently: it concurrently waits for all the
      bound promises to resolve and calls [f] as they resolve. *)
  val iter_with_waiting_ep :
    (key -> 'a -> (unit, 'error) result Lwt.t) ->
    ('a, 'error) t ->
    (unit, 'error list) result Lwt.t

  (** [fold_with_waiting_es f tbl init] folds [init] with [f] over the bindings
      in [tbl].

      Specifically, for each binding [(k, p)] it waits for [p] to be fulfilled
      with [Ok v] and determines the next accumulator by calling [f k v acc]. If
      [p] fulfills with [Error _] or is rejected, then no call is made for this
      binding.

      It processes bindings one after the other. *)
  val fold_with_waiting_es :
    (key -> 'a -> 'b -> ('b, 'trace) result Lwt.t) ->
    ('a, 'trace) t ->
    'b ->
    ('b, 'trace) result Lwt.t

  val fold_keys : (key -> 'b -> 'b) -> ('a, 'trace) t -> 'b -> 'b

  (** [fold_promises f tbl init] folds over the table, passing the raw promises
      to [f]. This means that [f] can observe [Error]/rejections.

      This can be used to, e.g., count the number of resolved/unresolved
      promises. *)
  val fold_promises :
    (key -> ('a, 'trace) result Lwt.t -> 'b -> 'b) -> ('a, 'trace) t -> 'b -> 'b

  (** [fold_resolved f tbl init] folds over the already resolved promises of
      [tbl]. More specifically, it folds over the [v] for all the promises
      fulfilled with [Ok v] that are bound in [tbl]. *)
  val fold_resolved : (key -> 'a -> 'b -> 'b) -> ('a, 'trace) t -> 'b -> 'b

  val length : ('a, 'trace) t -> int

  val stats : ('a, 'trace) t -> Stdlib.Hashtbl.statistics
end
