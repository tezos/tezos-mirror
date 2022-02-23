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

  val encoding :
    'ptr Data_encoding.t ->
    'content Data_encoding.t ->
    ('content, 'ptr) cell Data_encoding.t

  (** [index cell] returns the position of [cell] in the sequence. *)
  val index : (_, _) cell -> int

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
     that carries some [content], that follows [prev_cell],
     and that has the position [index] in the sequence. *)
  val next :
    prev_cell:('content, 'ptr) cell ->
    prev_cell_ptr:'ptr ->
    'content ->
    ('content, 'ptr) cell

  (** [back_path ~deref ~cell_ptr ~target_index] returns [Some path]
      where [path] is a sequence of back pointers to traverse to go
      from [cell_ptr] to the cell at position [target_index] in the
      sequence denoted by [(deref, cell_ptr)]. *)
  val back_path :
    deref:('ptr -> ('content, 'ptr) cell option) ->
    cell_ptr:'ptr ->
    target_index:int ->
    'ptr list option

  (** [valid_back_path ~equal_ptr ~deref ~cell_ptr ~target_ptr path]
      returns [true] iff [path] is a valid and minimal path from
      [cell_ptr] to [target_ptr] in the skip list denoted by
      [(deref, cell_ptr)]. *)
  val valid_back_path :
    equal_ptr:('ptr -> 'ptr -> bool) ->
    deref:('ptr -> ('content, 'ptr) cell option) ->
    cell_ptr:'ptr ->
    target_ptr:'ptr ->
    'ptr list ->
    bool
end

module Make (_ : sig
  val basis : int
end) : S
