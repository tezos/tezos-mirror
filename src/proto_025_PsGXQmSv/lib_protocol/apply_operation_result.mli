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

(** The result of an operation in the queue. [Skipped] ones should
    always be at the tail, and after a single [Failed].
    * The ['kind] parameter is the operation kind (a transaction, an
      origination, etc.).
    * The ['manager] parameter is the type of manager kinds.
    * The ['successful] parameter is the type of successful operations.
    The ['kind] parameter is used to make the type a GADT, but ['manager] and
    ['successful] are used to share [operation_result] between internal and
    external operation results, and are instantiated for each case. *)
type ('kind, 'manager, 'successful) operation_result =
  | Applied of 'successful
  | Backtracked of 'successful * error trace option
  | Failed :
      'manager * error trace
      -> ('kind, 'manager, 'successful) operation_result
  | Skipped : 'manager -> ('kind, 'manager, 'successful) operation_result

val trace_encoding : error trace Data_encoding.t
