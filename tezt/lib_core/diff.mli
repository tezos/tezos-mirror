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

(** Compute the difference between two sequences of items. *)

(** Whether an item was kept, added or removed.

    - [Kept] carries [(index_before, index_after)].
    - [Added] carries [index_after].
    - [Removed] carries [index_before]. *)
type item_report = Kept of int * int | Added of int | Removed of int

(** Difference reports.

    [before] and [after] are the names of what was compared (e.g. filenames).
    They are used by {!log}.

    [merged] is such that filtering out [Added] items gives the items of [before]
    and filtering out [Removed] items gives the items of [after].

    [different] is [true] iff at least one item's report is not [Kept]. *)
type 'a t = {
  before : string;
  after : string;
  merged : (item_report * 'a) array;
  different : bool;
}

(** Compute the difference between two arrays.

    Usage: [arrays a b]

    The algorithm compares items of [a] and [b] together with [equal].
    When two items are different, the algorithm tries to find equal items
    later in [a] and [b]. [max_sync_distance] is the maximum distance after
    which it gives up. Increasing [max_sync_distance] gives more accurate results
    but increases the time taken quadratically.

    [before] and [after] are stored in the result's [before] and [after] fields.
    Default values are ["before"] and ["after"] respectively. *)
val arrays :
  ?max_sync_distance:int ->
  ?equal:('a -> 'a -> bool) ->
  ?before:string ->
  ?after:string ->
  'a array ->
  'a array ->
  'a t

(** Compute the difference between two files.

    Usage: [files before_filename after_filename]

    Files are compared line by line.

    [before] and [after] are stored in the result's [before] and [after] fields.
    Default values are [before_filename] and [after_filename] respectively. *)
val files :
  ?max_sync_distance:int ->
  ?before:string ->
  ?after:string ->
  string ->
  string ->
  string t

(** Remove some [Kept] items.

    Only keep at most [before] [Kept] items before [Added] and [Removed] items.
    Only keep at most [after] [Kept] items after [Added] and [Removed] items.

    Default value for [before] and [after] is [3]. *)
val reduce_context : ?before:int -> ?after:int -> 'a t -> 'a t

(** Output a diff if there are differences.

    Usage: [output output_line show_item diff]

    If [diff.different] is [false], this does nothing.
    Else, it prints [diff.before] and [diff.after], followed by [diff.merged].
    Printing is done using [output_line]. *)
val output :
  (Log.Color.t option -> string -> unit) -> ('a -> string) -> 'a t -> unit

(** Same as [output] using [Log.log], for string diffs. *)
val log : ?level:Cli.log_level -> string t -> unit
