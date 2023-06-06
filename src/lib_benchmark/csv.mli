(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Save and load tables as CSV files.

    Note that this module does not support the full specification of CSV
    but covers only the specific format used in Snoop.
*)

(** The type of table.  A list of rows. Each row must have the same number of
    columns *)
type csv = string list list

(** Horizontally concat tables. The Tables must have the same number of rows. *)
val concat : ?check_disjoint_headers:bool -> csv -> csv -> csv

(** Save a table to a CSV file *)
val export :
  filename:string -> ?separator:char -> ?linebreak:char -> csv -> unit

(** Print a table as CSV to stdout *)
val export_stdout : ?separator:char -> ?linebreak:char -> csv -> unit

(** Load a CSV file *)
val import : filename:string -> ?separator:char -> unit -> csv

(** Extend the CSV file with the given columns *)
val append_columns :
  filename:string -> ?separator:char -> ?linebreak:char -> csv -> unit
