(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module provides an implementation of the skip list data structure. *)

(** Basic signature for a monad. *)
module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

(** A skip list represents a sequence of values. There are three main
    differences between these [skip list]s and OCaml standard [list]s:

    1. A skip list cannot be empty.

    2. A skip list grows at its end.

    3. Each cell of the skip list provides several back pointers
       allowing to *skip* chunk of ancestors of the sequence to directly
       jump to a given position. More precisely, given a [basis]
       parameter, the i-th back pointers of element number [n] in the
       sequence points to [n - n mod basis^i - 1].  The element number [n]
       in the sequence contains [log_basis n] back pointers.

    The skip list is defined by a pair of dereferencing function
    of type ['ptr -> ('content, 'ptr) cell] and the last cell
    of the sequence. The maintainance of this pair is left to the client.
    In particular, the client is responsible to correctly bind a cell
    to each back pointers reachable from the last cell.

*)
module type S = sig
  (** A cell in the skip list carrying a given ['content] and back
     pointers of type ['ptr]. *)
  type ('content, 'ptr) cell

  val pp :
    pp_ptr:(Format.formatter -> 'ptr -> unit) ->
    pp_content:(Format.formatter -> 'content -> unit) ->
    Format.formatter ->
    ('content, 'ptr) cell ->
    unit

  val equal :
    ('ptr -> 'ptr -> bool) ->
    ('content -> 'content -> bool) ->
    ('content, 'ptr) cell ->
    ('content, 'ptr) cell ->
    bool

  val encoding :
    'ptr Data_encoding.t ->
    'content Data_encoding.t ->
    ('content, 'ptr) cell Data_encoding.t

  (** [index cell] returns the position of [cell] in the sequence. *)
  val index : (_, _) cell -> Z.t

  (** [content cell] is the content carried by the [cell]. *)
  val content : ('content, 'ptr) cell -> 'content

  (** [back_pointer cell i] returns [Some ptr] if [ptr] is the
      [i]-th back pointer of [cell]. Returns [None] if the cell
      contains less than [i + 1] back pointers. *)
  val back_pointer : ('content, 'ptr) cell -> int -> 'ptr option

  (** [back_pointers cell] returns the back pointers of [cell]. *)
  val back_pointers : ('content, 'ptr) cell -> 'ptr list

  (** [genesis content] is the first cell of a skip list. It has
      no back pointers. *)
  val genesis : 'content -> ('content, 'ptr) cell

  (** [next ~prev_cell ~prev_cell_ptr content] creates a new cell
      that carries some [content], that follows [prev_cell]. *)
  val next :
    prev_cell:('content, 'ptr) cell ->
    prev_cell_ptr:'ptr ->
    'content ->
    ('content, 'ptr) cell

  type ('ptr, 'content) search_cell_result =
    | Found of ('ptr, 'content) cell
    | Nearest of {
        lower : ('ptr, 'content) cell;
        upper : ('ptr, 'content) cell option;
      }
    | No_exact_or_lower_ptr
    | Deref_returned_none

  type ('ptr, 'content) search_result = {
    rev_path : ('ptr, 'content) cell list;
    last_cell : ('ptr, 'content) search_cell_result;
  }

  val pp_search_result :
    pp_cell:(Format.formatter -> ('ptr, 'content) cell -> unit) ->
    Format.formatter ->
    ('ptr, 'content) search_result ->
    unit

  module type MONADIC = sig
    (** Type of results for monadic functions. *)
    type 'a result

    (** [find ~deref ~cell_ptr ~target_index] returns [Some cell] where [cell] is
      the cell at position [target_index]. This is done by dereferencing the last
      pointer of the path returned by {!back_path}. *)
    val find :
      deref:('ptr -> ('content, 'ptr) cell option result) ->
      cell_ptr:'ptr ->
      target_index:Z.t ->
      ('content, 'ptr) cell option result

    (** [back_path ~deref ~cell_ptr ~target_index] returns [Some path]
      where [path] is a sequence of back pointers to traverse to go
      from [cell_ptr] to the cell at position [target_index] in the
      sequence denoted by [(deref, cell_ptr)]. *)
    val back_path :
      deref:('ptr -> ('content, 'ptr) cell option result) ->
      cell_ptr:'ptr ->
      target_index:Z.t ->
      'ptr list option result

    (** [valid_back_path ~equal_ptr ~deref ~cell_ptr ~target_ptr path]
      returns [true] iff [path] is a valid and minimal path from
      [cell_ptr] to [target_ptr] in the skip list denoted by
      [(deref, cell_ptr)]. *)
    val valid_back_path :
      equal_ptr:('ptr -> 'ptr -> bool) ->
      deref:('ptr -> ('content, 'ptr) cell option result) ->
      cell_ptr:'ptr ->
      target_ptr:'ptr ->
      'ptr list ->
      bool result

    (** [search ~deref ~compare ~cell] allows to find a cell of the skip
     list according to its content. This function assumes that the
     content of the cells is in increasing order according to the
     ordering defined by the function [compare]. In other words, this
     function assumes that [compare] is a function that returns a
     negative integer for cells before the target and a positive
     integer for cells after the target. The value returned by this
     function is [{rev_path; last_cell}] such that.

     - [rev_path = []] if and only if [compare (content cell) > 0]

     - For all the cases below, if there is a path from cell [A] to
     cell [B], [rev_path] contains the list of cells to go from [B] to
     [A]. Consequently, the first element of [rev_path] is [B].
     Except for [Nearest_lower], this path is a minimal path.

     - [last_pointer = Deref_returned_none] if [deref] fails to
     associate a cell to a pointer during the search. In that case,
     [rev_path] is a path from [cell] to [candidate] where [candidate]
     is the last cell for which candidate did not fail and such that
     [compare (content (candidate)) > 0].

     - [last_pointer = No_exact_or_lower_ptr] if for all cell of the
     skip list, [compare (content cell) > 0]. In that case, [rev_path]
     is a path from [cell] to the genesis cell.

     - [last_pointer = Found target] if there is a cell [target] such
     that [compare (content target) = 0] and a path from [cell] to
     [target]. In that case, [rev_path] is the minimal path from
     [cell] to [target].

     - [last_pointer = Nearest_lower {lower;upper}] if there is no
     cell in the skip list such that [compare (content cell) = 0]. In
     that case [lower] is the unique cell such that [compare (content
     lower) < 0] and for all other cells [candidate] such that
     [compare (content candidate) < 0] then there is a path from
     [lower] to [candidate]. [upper], if it exists is the successor
     cell to [lower], i.e. [deref ((back_pointer upper) 0) = Some
     lower].  In that case, [rev_path] is a path from [cell] to
     [lower]. This path is *NOT* minimal but almost. The path is
     minimal from [cell] to [upper=Some up]. By minimality, the path
     is logarithmic. Consequently, since there is a direct pointer
     from [up] to [lower], the passe to [lower] is also
     logarithmic.

     If a target cell [target_cell] exists, i.e. there is a cell such
     that [compare target_cell = 0], then it is not necessary for
     [deref] to return [Some cell] for all [cell] such that [compare
     cell < 0]. If a [target_cell] does not exist, the lowest bound
     for which [deref] must return [Some cell] is unknown. *)
    val search :
      deref:('ptr -> ('content, 'ptr) cell option result) ->
      compare:('content -> int) ->
      cell:('content, 'ptr) cell ->
      ('content, 'ptr) search_result result
  end

  (** Functions in the empty monad are accessible directly. *)
  include MONADIC with type 'a result := 'a

  (** This module contains functions in the {!Lwt} monad. *)
  module Lwt : MONADIC with type 'a result := 'a Lwt.t

  (** This functor can be used to build monadic functions for the skip list. *)
  module Make_monadic (M : MONAD) : MONADIC with type 'a result := 'a M.t
end

module Make (_ : sig
  val basis : int
end) : S
